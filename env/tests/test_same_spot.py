import importlib.util
import contextlib
import io
import json
import os
from pathlib import Path
import subprocess
import sys
import tempfile
import unittest
from types import SimpleNamespace
from unittest.mock import patch

import numpy as np


script = Path(os.environ.get("SAME_SPOT_SCRIPT", Path(__file__).resolve().parents[1]
                             / "desktop/scripts/same-spot.py"))
specification = importlib.util.spec_from_file_location("same_spot", script)
same_spot = importlib.util.module_from_spec(specification)
sys.modules[specification.name] = same_spot
specification.loader.exec_module(same_spot)


def scene(seed=0):
    random = np.random.default_rng(seed)
    pixels = random.integers(30, 180, (27, 48), dtype=np.uint8)
    return same_spot.Scene(pixels, np.ones(pixels.shape, dtype=bool))


class ObserverTests(unittest.TestCase):
    def setUp(self):
        self.image = np.random.default_rng(0).integers(80, 220, (360, 640, 3), dtype=np.uint8)
        detector = patch.object(same_spot.cv2, "FaceDetectorYN")
        self.detector = detector.start().create.return_value
        self.addCleanup(detector.stop)
        self.observer = same_spot.Observer("unused.onnx", 0.85)

    def faces(self, *dimensions):
        faces = np.zeros((len(dimensions), 15), dtype=np.float32)
        for row, (height, score) in zip(faces, dimensions):
            row[:4] = (200, 100, height * 0.7, height)
            row[-1] = score
        return faces

    def test_confident_small_faces_do_not_count_as_presence(self):
        self.detector.detect.return_value = (None, self.faces((18, 0.99)))
        observation = self.observer.observe_image(self.image)
        self.assertEqual(observation.kind, "no-face")
        self.assertAlmostEqual(observation.face_height_fraction, 0.05)
        self.assertEqual(self.detector.detect.call_count, 2)
        self.assertIsNone(observation.scene)

    def test_small_face_does_not_override_a_nearby_face(self):
        self.detector.detect.return_value = (None, self.faces((18, 0.99), (144, 0.9)))
        observation = self.observer.observe_image(self.image)
        self.assertEqual(observation.kind, "present")
        self.assertAlmostEqual(observation.score, 0.9)
        self.assertAlmostEqual(observation.face_height_fraction, 0.4)

    def test_small_face_does_not_skip_enhanced_search_for_nearby_face(self):
        self.detector.detect.side_effect = [(None, self.faces((18, 0.99))),
                                           (None, self.faces((108, 0.9)))]
        self.assertEqual(self.observer.observe_image(self.image).kind, "present")

    def test_marginal_nearby_detection_remains_uncertain(self):
        self.detector.detect.return_value = (None, self.faces((18, 0.99), (108, 0.5)))
        self.assertEqual(self.observer.observe_image(self.image).kind, "uncertain")

    def test_small_faces_in_dark_images_do_not_establish_absence(self):
        self.detector.detect.return_value = (None, self.faces((18, 0.99)))
        self.assertEqual(self.observer.observe_image(np.zeros_like(self.image)).kind, "uncertain")

    def test_minimum_height_is_relative_to_image_and_configurable(self):
        self.detector.detect.return_value = (None, self.faces((60, 0.9)))
        self.assertEqual(self.observer.observe_image(self.image).kind, "no-face")
        self.observer.min_face_fraction = 0.15
        self.assertEqual(self.observer.observe_image(self.image).kind, "present")
        taller = np.random.default_rng(0).integers(80, 220, (480, 640, 3), dtype=np.uint8)
        self.assertEqual(self.observer.observe_image(taller).kind, "no-face")


class SceneTests(unittest.TestCase):
    def test_shift_and_lighting_change_preserve_similarity(self):
        original = scene()
        shifted = same_spot.Scene(np.roll(original.pixels, (2, -3), (0, 1)) + 30,
                                 original.background.copy())
        self.assertAlmostEqual(original.similarity(shifted), 1, places=5)

    def test_person_pixels_are_excluded_in_both_images(self):
        original = scene()
        changed = scene(1)
        changed.pixels[:12] = original.pixels[:12]
        original.background[12:] = False
        changed.background[14:] = False
        self.assertAlmostEqual(original.similarity(changed), 1, places=5)

    def test_blank_or_small_background_is_unknown(self):
        blank = same_spot.Scene(np.full((27, 48), 60, dtype=np.uint8), np.ones((27, 48), dtype=bool))
        self.assertIsNone(blank.similarity(blank))
        small = scene()
        small.background[:, 5:] = False
        self.assertIsNone(small.similarity(small))

    def test_different_scenes_do_not_match(self):
        self.assertLess(scene().similarity(scene(1)), 0.35)


class TrackerTests(unittest.TestCase):
    def setUp(self):
        self.tracker = same_spot.Tracker()
        self.policy = same_spot.Policy()
        self.scene = scene()

    def observe(self, timestamp, kind="present", background=None):
        observation = same_spot.Observation(kind, scene=self.scene if background is None else background)
        return self.tracker.step(timestamp, observation, self.policy)

    def present_until(self, timestamp):
        start = 0 if self.tracker.last_timestamp is None else self.tracker.last_timestamp + 30
        for current in range(int(start), timestamp + 1, 30):
            record = self.observe(current)
        return record

    def test_reminders_at_45_minutes_and_every_15_minutes(self):
        reminders = []
        for timestamp in range(0, 91 * 60, 30):
            if self.observe(timestamp)["notification_due"]:
                reminders.append(timestamp / 60)
                self.tracker.notified(timestamp, self.policy)
        self.assertEqual(reminders, [45, 60, 75, 90])

    def test_lid_and_lighting_changes_while_present_do_not_reset(self):
        for timestamp in range(0, 3600, 30):
            self.observe(timestamp, background=scene(timestamp // 300))
        self.assertEqual(self.tracker.session_start, 0)
        self.assertLessEqual(len(self.tracker.gallery), 8)

    def test_short_absence_and_failed_detection_preserve_streak(self):
        self.present_until(600)
        for timestamp in (630, 660, 690):
            self.observe(timestamp, "no-face")
        self.assertEqual(self.observe(720)["event"], "returned")
        self.observe(750, "uncertain")
        self.observe(780)
        self.assertEqual(self.tracker.session_start, 0)
        self.assertEqual(self.tracker.short_break_count, 1)
        self.assertEqual(self.tracker.short_break_total, 90)
        self.assertEqual(self.tracker.longest_short_break, 90)
        self.assertEqual(self.tracker.last_short_break, 90)

    def test_separate_short_breaks_are_measured_without_resetting_the_timer(self):
        self.present_until(600)
        for timestamp in range(630, 870, 30):
            self.observe(timestamp, "no-face")
        self.observe(870)
        for timestamp in range(900, 1020, 30):
            self.observe(timestamp, "no-face")
        record = self.observe(1020)
        self.assertEqual(self.tracker.session_start, 0)
        self.assertEqual(record["short_break_count"], 2)
        self.assertEqual(record["short_break_seconds"], 360)
        self.assertEqual(record["longest_short_break_seconds"], 240)
        self.assertEqual(record["last_short_break_seconds"], 120)

    def test_return_after_a_full_break_starts_a_new_session(self):
        self.present_until(600)
        for timestamp in range(630, 930, 30):
            self.observe(timestamp, "no-face")
        record = self.observe(930)
        self.assertEqual(record["event"], "probable-break")
        self.assertEqual(self.tracker.session_start, 930)
        self.assertEqual(self.tracker.short_break_count, 0)

    def test_uncertainty_and_missing_frames_only_count_confirmed_absence(self):
        for interrupted in ("uncertain", "missing"):
            with self.subTest(interrupted=interrupted):
                self.setUp()
                self.present_until(600)
                self.observe(630, "no-face")
                self.observe(660, "no-face")
                if interrupted == "uncertain":
                    self.observe(690, "uncertain")
                self.observe(780)
                self.assertEqual(self.tracker.short_break_count, 1)
                self.assertEqual(self.tracker.short_break_total, 30)

    def test_short_break_metrics_survive_restart_and_reset_after_full_break(self):
        self.present_until(600)
        self.observe(630, "no-face")
        self.observe(660, "no-face")
        self.observe(690)
        with tempfile.TemporaryDirectory() as temporary:
            state = Path(temporary) / "state.json"
            self.tracker.save(state)
            self.tracker = same_spot.Tracker.load(state)
        self.assertEqual(self.tracker.short_break_count, 1)
        self.assertEqual(self.tracker.short_break_total, 60)
        self.assertEqual(self.tracker.longest_short_break, 60)
        self.assertEqual(self.tracker.last_short_break, 60)
        for timestamp in range(720, 1021, 30):
            self.observe(timestamp, "no-face")
        self.assertEqual(self.tracker.short_break_count, 0)
        self.assertEqual(self.tracker.short_break_total, 0)

    def test_old_saved_state_defaults_to_no_recorded_short_breaks(self):
        self.present_until(600)
        with tempfile.TemporaryDirectory() as temporary:
            state = Path(temporary) / "state.json"
            self.tracker.save(state)
            value = json.loads(state.read_text())
            for name in ("short_break_count", "short_break_total", "longest_short_break", "last_short_break"):
                del value["tracker"][name]
            state.write_text(json.dumps(value))
            self.tracker = same_spot.Tracker.load(state)
        self.assertEqual(self.tracker.session_start, 0)
        self.assertEqual(self.tracker.short_break_total, 0)

    def test_invalid_saved_short_break_measurements_are_rejected(self):
        with tempfile.TemporaryDirectory() as temporary:
            state = Path(temporary) / "state.json"
            self.present_until(600)
            self.tracker.save(state)
            original = json.loads(state.read_text())
            for changes in ({"short_break_count": -1}, {"short_break_total": float("nan")},
                            {"longest_short_break": 30}, {"short_break_count": 1},
                            {"short_break_total": 60}):
                with self.subTest(changes=changes), contextlib.redirect_stderr(io.StringIO()):
                    value = original | {"tracker": original["tracker"] | changes}
                    state.write_text(json.dumps(value))
                    self.assertIsNone(same_spot.Tracker.load(state).session_start)

    def test_sustained_good_quality_no_face_resets(self):
        self.present_until(600)
        for timestamp in range(630, 931, 30):
            record = self.observe(timestamp, "no-face")
        self.assertEqual(record["event"], "probable-break")
        self.observe(960)
        self.assertEqual(self.tracker.session_start, 960)

    def test_absence_is_not_accumulated_across_uncertain_images(self):
        self.present_until(600)
        for timestamp in range(630, 1201, 30):
            self.observe(timestamp, "uncertain" if timestamp == 900 else "no-face")
        self.assertEqual(self.tracker.session_start, 0)
        self.assertFalse(self.observe(1230, "uncertain")["notification_due"])

    def test_uncertain_images_never_notify_and_reappearance_starts_fresh(self):
        self.present_until(2640)
        for timestamp in range(2670, 3301, 30):
            self.assertFalse(self.observe(timestamp, "uncertain")["notification_due"])
        self.assertEqual(self.observe(3330)["event"], "unobserved")
        self.assertEqual(self.tracker.session_start, 3330)

    def test_new_scene_after_short_absence_needs_confirmation(self):
        self.present_until(600)
        self.observe(630, "no-face")
        new_scene = scene(1)
        for timestamp in (660, 690):
            record = self.observe(timestamp, background=new_scene)
            self.assertEqual(self.tracker.session_start, 0)
            self.assertFalse(record["notification_due"])
        self.assertEqual(self.observe(720, background=new_scene)["event"], "moved")
        self.assertEqual(self.tracker.session_start, 720)

    def test_unmatchable_return_resumes_after_confirmation(self):
        self.present_until(600)
        self.observe(630, "uncertain")
        blank = same_spot.Scene(np.full((27, 48), 60, dtype=np.uint8), self.scene.background)
        for timestamp in (660, 690, 720):
            record = self.observe(timestamp, background=blank)
        self.assertEqual(record["event"], "returned-uncertain-scene")
        self.assertEqual(self.tracker.session_start, 0)

    def test_capture_gap_and_clock_reversal_start_new_estimates(self):
        self.present_until(600)
        self.assertEqual(self.observe(1200)["event"], "capture-gap")
        self.assertEqual(self.tracker.session_start, 1200)
        self.assertEqual(self.observe(1100)["event"], "capture-gap")
        self.assertEqual(self.tracker.session_start, 1100)

    def test_missed_reminders_do_not_create_backlog(self):
        self.present_until(4600)
        self.tracker.notified(4600, self.policy)
        self.assertEqual(self.tracker.next_notification, 90 * 60)

    def test_restart_preserves_timer_and_notification_schedule(self):
        self.present_until(2700)
        self.tracker.notified(2700, self.policy)
        with tempfile.TemporaryDirectory() as temporary:
            state = Path(temporary) / "state.json"
            self.tracker.save(state)
            self.tracker = same_spot.Tracker.load(state)
        self.assertEqual(self.tracker.session_start, 0)
        self.assertEqual(self.tracker.next_notification, 3600)
        self.assertAlmostEqual(self.tracker.gallery[0].similarity(self.scene), 1, places=5)

    def test_corrupt_state_starts_fresh(self):
        with tempfile.TemporaryDirectory() as temporary:
            state = Path(temporary) / "state.json"
            for content in ("{", '{"version": 2}', '{"version": 1, "tracker": {}}'):
                state.write_text(content)
                tracker = same_spot.Tracker.load(state)
                self.assertIsNone(tracker.session_start)


class IntegrationTests(unittest.TestCase):
    def watcher(self, root):
        return SimpleNamespace(state=root / "state.json", watch=root, dry_run=False,
                               once=True, poll_seconds=10)

    def test_capture_discovery_skips_unrelated_files(self):
        with tempfile.TemporaryDirectory() as temporary:
            root = Path(temporary)
            directory = root / "2026-10-15"
            directory.mkdir()
            for name in ("12-00-00.jpg", "12-00-30.jpg", "not-a-capture.jpg"):
                (directory / name).touch()
            self.assertEqual(len(same_spot.captures(root)), 2)
            self.assertEqual(len(same_spot.captures(directory)), 2)

    def test_notification_replaces_previous_and_describes_estimate(self):
        tracker = same_spot.Tracker(session_start=0, notification_id=42)
        with patch.object(same_spot.subprocess, "run", return_value=subprocess.CompletedProcess([], 0, "43\n")) as run:
            same_spot.notify(tracker, 4500)
        command = run.call_args.args[0]
        self.assertEqual(command[command.index("--replace-id") + 1], "42")
        self.assertIn("75 minutes", command[-2])
        self.assertIn("No short breaks detected", command[-1])
        self.assertIn("5m continuously away resets the timer", command[-1])
        self.assertEqual(tracker.notification_id, 43)

    def test_notification_reports_short_break_measurements_and_configured_minimum(self):
        tracker = same_spot.Tracker(session_start=0, notification_id=42, short_break_count=2,
                                    short_break_total=210, longest_short_break=120, last_short_break=90)
        with patch.object(same_spot.subprocess, "run", return_value=subprocess.CompletedProcess([], 0, "43\n")) as run:
            same_spot.notify(tracker, 4500, same_spot.Policy(break_duration=240))
        body = run.call_args.args[0][-1]
        self.assertIn("2 short breaks: 3m 30s total", body)
        self.assertIn("longest 2m, latest 1m 30s", body)
        self.assertIn("4m continuously away resets the timer", body)

    def test_watcher_starts_with_latest_image_and_processes_each_once(self):
        with tempfile.TemporaryDirectory() as temporary:
            root = Path(temporary)
            arguments = self.watcher(root)
            available = [(1000, root / "first.jpg"), (4000, root / "latest.jpg")]
            observer = SimpleNamespace(observe=lambda path: same_spot.Observation("present", scene=scene()))
            with patch.object(same_spot, "captures", return_value=available), \
                    patch.object(same_spot, "notify") as notify, contextlib.redirect_stdout(io.StringIO()) as output:
                same_spot.watch(arguments, observer, same_spot.Policy())
                same_spot.watch(arguments, observer, same_spot.Policy())
            self.assertEqual(len(output.getvalue().splitlines()), 1)
            self.assertEqual(same_spot.Tracker.load(arguments.state).session_start, 4000)
            notify.assert_not_called()

    def test_watcher_notifies_only_for_latest_fresh_frame(self):
        with tempfile.TemporaryDirectory() as temporary:
            root = Path(temporary)
            arguments = self.watcher(root)
            tracker = same_spot.Tracker(session_start=1000, next_notification=1060,
                                        last_timestamp=1030, last_present=1030, gallery=[scene()])
            tracker.save(arguments.state)
            available = [(1060, root / "first.jpg"), (1090, root / "latest.jpg")]
            observer = SimpleNamespace(observe=lambda path: same_spot.Observation("present", scene=scene()))
            with patch.object(same_spot, "captures", return_value=available), \
                    patch.object(same_spot.time, "time", return_value=1100), \
                    patch.object(same_spot, "notifications_allowed", return_value=True), \
                    patch.object(same_spot, "notify") as notify, contextlib.redirect_stdout(io.StringIO()):
                same_spot.watch(arguments, observer, same_spot.Policy(first=60, repeat=60))
            notify.assert_called_once()
            self.assertEqual(notify.call_args.args[1], 1090)
            self.assertEqual(same_spot.Tracker.load(arguments.state).next_notification, 1120)

    def test_failed_delivery_remains_due_for_retry(self):
        with tempfile.TemporaryDirectory() as temporary:
            root = Path(temporary)
            arguments = self.watcher(root)
            same_spot.Tracker(session_start=1000, next_notification=1060, last_timestamp=1030,
                              last_present=1030, gallery=[scene()]).save(arguments.state)
            observer = SimpleNamespace(observe=lambda path: same_spot.Observation("present", scene=scene()))
            with patch.object(same_spot, "captures", return_value=[(1060, root / "image.jpg")]), \
                    patch.object(same_spot.time, "time", return_value=1070), \
                    patch.object(same_spot, "notifications_allowed", return_value=True), \
                    patch.object(same_spot, "notify", side_effect=subprocess.TimeoutExpired("notify-send", 10)), \
                    contextlib.redirect_stdout(io.StringIO()), contextlib.redirect_stderr(io.StringIO()):
                same_spot.watch(arguments, observer, same_spot.Policy(first=60, repeat=60))
            self.assertEqual(same_spot.Tracker.load(arguments.state).next_notification, 1060)

    def test_inhibition_and_screen_lock_suppress_notifications(self):
        with tempfile.TemporaryDirectory() as temporary:
            directory = Path(temporary) / "penrose"
            directory.mkdir()
            deadline = directory / "webcam-inhibit-until"
            deadline.write_text("2000")
            with patch.dict(os.environ, XDG_STATE_HOME=temporary), \
                    patch.object(same_spot.time, "time", return_value=1000), \
                    patch.object(same_spot.subprocess, "run", return_value=subprocess.CompletedProcess([], 0)) as run:
                self.assertFalse(same_spot.notifications_allowed())
                run.assert_not_called()
                deadline.unlink()
                self.assertFalse(same_spot.notifications_allowed())

    def test_replay_proposes_notifications_without_sending_or_saving(self):
        with tempfile.TemporaryDirectory() as temporary:
            root = Path(temporary)
            arguments = SimpleNamespace(replay=root, since=None, until=None)
            observer = SimpleNamespace(observe=lambda path: same_spot.Observation("present", scene=scene()))
            available = [(timestamp, root / f"{timestamp}.jpg") for timestamp in (1000, 1030, 1060)]
            with patch.object(same_spot, "captures", return_value=available), \
                    patch.object(same_spot, "notify") as notify, \
                    patch.object(same_spot.Tracker, "save") as save, contextlib.redirect_stdout(io.StringIO()) as output:
                same_spot.replay(arguments, observer, same_spot.Policy(first=60, repeat=60))
            records = [json.loads(line) for line in output.getvalue().splitlines()]
            self.assertEqual([record["notification_due"] for record in records], [False, False, True])
            notify.assert_not_called()
            save.assert_not_called()


if __name__ == "__main__":
    unittest.main()
