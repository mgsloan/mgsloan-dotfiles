import contextlib
from concurrent.futures import ThreadPoolExecutor
from datetime import datetime
import http.client
from http.server import ThreadingHTTPServer
import importlib.util
import io
import json
import os
from pathlib import Path
import subprocess
import sys
import tempfile
import threading
from types import SimpleNamespace
import unittest
from unittest.mock import patch

import numpy as np


script = Path(os.environ.get("SAME_SPOT_REVIEW_SCRIPT", Path(__file__).resolve().parents[1]
                             / "desktop/scripts/same-spot-review.py"))
specification = importlib.util.spec_from_file_location("same_spot_review", script)
review = importlib.util.module_from_spec(specification)
specification.loader.exec_module(review)
detector_script = Path(os.environ.get("SAME_SPOT_SCRIPT", script.with_name("same-spot.py")))
detector = review.load_detector(detector_script)


class SegmentTests(unittest.TestCase):
    def test_segments_cover_every_frame_including_untracked_and_gaps(self):
        sessions = [None, 30, 30, 30, None, None, None, 600]
        records = [{"timestamp": index * 30, "session_start": session,
                    "event": "capture-gap" if index == 6 else None,
                    "notification_due": index == 3}
                   for index, session in enumerate(sessions)]
        segments = review.split_segments(records)
        self.assertEqual([(segment["start_frame"], segment["end_frame"]) for segment in segments],
                         [(0, 1), (1, 4), (4, 6), (6, 7), (7, 8)])
        self.assertEqual(sum(segment["captures"] for segment in segments), len(records))
        self.assertEqual(segments[0]["kind"], "untracked")
        self.assertEqual(segments[1]["reminders"], 1)

    def test_original_capture_gaps_are_preserved(self):
        with tempfile.TemporaryDirectory() as temporary:
            video = Path(temporary) / "2026-10-14.hevc"
            video.with_name("2026-10-14-timestamps").write_text("12:00:00\n12:00:30\n15:30:00\n")
            timestamps = review.read_timestamps(video)
            self.assertEqual(timestamps[1] - timestamps[0], 30)
            self.assertEqual(timestamps[2] - timestamps[1], 12570)
            self.assertEqual(datetime.fromtimestamp(timestamps[2]).hour, 15)

    def test_invalid_timestamps_are_rejected_without_shifting_frame_alignment(self):
        with tempfile.TemporaryDirectory() as temporary:
            video = Path(temporary) / "2026-10-14.hevc"
            companion = video.with_name("2026-10-14-timestamps")
            for content in ("", "12:00:00\n\n12:01:00\n", "not-a-time\n"):
                companion.write_text(content)
                with self.assertRaises(ValueError):
                    review.read_timestamps(video)


class ArchiveTests(unittest.TestCase):
    @classmethod
    def setUpClass(cls):
        cls.temporary = tempfile.TemporaryDirectory()
        cls.root = Path(cls.temporary.name)
        cls.video = cls.root / "2026-10-14.hevc"
        subprocess.run([
            "ffmpeg", "-nostdin", "-v", "error", "-f", "lavfi", "-i", "testsrc2=size=96x64:rate=2",
            "-frames:v", "8", "-c:v", "libx265", "-x265-params",
            "pools=none:frame-threads=1:log-level=error", str(cls.video),
        ], check=True, capture_output=True)
        cls.model = cls.root / "model.onnx"
        cls.model.write_bytes(b"test model; observation is stubbed")

    @classmethod
    def tearDownClass(cls):
        cls.temporary.cleanup()

    def arguments(self, cache):
        return SimpleNamespace(root=self.video, detector_script=detector_script, model=self.model,
                               presence_threshold=0.85, date=None, cache=cache)

    def write_timestamps(self, count=8):
        times = [f"12:00:{index * 10:02d}" for index in range(4)]
        times += [f"12:10:{index * 10:02d}" for index in range(4)]
        if count == 9:
            times.append("12:11:00")
        self.video.with_name(self.video.stem + "-timestamps").write_text("\n".join(times[:count]) + "\n")

    def test_archive_analysis_produces_exact_clips_and_reuses_cache(self):
        self.write_timestamps()
        observer = SimpleNamespace(observe_image=lambda image: detector.Observation("present"))
        with tempfile.TemporaryDirectory() as temporary:
            arguments = self.arguments(Path(temporary))
            analyzer = review.Review(arguments)
            with patch.object(analyzer.detector, "Observer", return_value=observer), \
                    patch.object(analyzer.detector, "notify") as notify, \
                    patch.object(analyzer.detector.Tracker, "save") as save, \
                    contextlib.redirect_stdout(io.StringIO()):
                analyzer.prepare(self.video.stem)
            day = analyzer.days[self.video.stem]
            self.assertEqual(day["status"], "ready", day.get("error"))
            manifest = analyzer.manifest(self.video.stem)
            self.assertEqual(manifest["playback_rate"], 48)
            self.assertEqual(len(manifest["frames"]), 8)
            self.assertEqual([segment["captures"] for segment in manifest["segments"]], [4, 4])
            self.assertTrue(all((day["directory"] / segment["file"]).is_file() for segment in manifest["segments"]))
            cached = review.Review(arguments)
            self.assertEqual(cached.days[self.video.stem]["status"], "ready")
            arguments.presence_threshold = 0.9
            self.assertEqual(review.Review(arguments).days[self.video.stem]["status"], "queued")
            notify.assert_not_called()
            save.assert_not_called()

    def test_speed_change_reuses_analysis_without_face_detection(self):
        self.write_timestamps()
        observer = SimpleNamespace(observe_image=lambda image: detector.Observation("present"))
        with tempfile.TemporaryDirectory() as temporary:
            arguments = self.arguments(Path(temporary))
            with patch.object(review, "PLAYBACK_RATE", 2):
                previous = review.Review(arguments)
                with patch.object(previous.detector, "Observer", return_value=observer), \
                        contextlib.redirect_stdout(io.StringIO()):
                    previous.prepare(self.video.stem)
            analyzer = review.Review(arguments)
            with patch.object(analyzer.detector, "Observer", side_effect=AssertionError("must reuse analysis")), \
                    contextlib.redirect_stdout(io.StringIO()):
                analyzer.prepare(self.video.stem)
            self.assertEqual(analyzer.days[self.video.stem]["status"], "ready")
            self.assertEqual(analyzer.manifest(self.video.stem)["frames"], previous.manifest(self.video.stem)["frames"])
            self.assertEqual(analyzer.manifest(self.video.stem)["playback_rate"], 48)

    def test_frame_timestamp_mismatch_never_publishes_partial_analysis(self):
        observer = SimpleNamespace(observe_image=lambda image: detector.Observation("present"))
        for count in (7, 9):
            with self.subTest(count=count), tempfile.TemporaryDirectory() as temporary:
                self.write_timestamps(count)
                analyzer = review.Review(self.arguments(Path(temporary)))
                with patch.object(analyzer.detector, "Observer", return_value=observer), \
                        contextlib.redirect_stdout(io.StringIO()), contextlib.redirect_stderr(io.StringIO()):
                    analyzer.prepare(self.video.stem)
                self.assertEqual(analyzer.days[self.video.stem]["status"], "error")
                self.assertIsNone(analyzer.manifest(self.video.stem))

    def test_concurrent_commands_share_one_analysis(self):
        self.write_timestamps()
        observer = SimpleNamespace(observe_image=lambda image: detector.Observation("present"))
        with tempfile.TemporaryDirectory() as temporary, \
                patch.object(review, "load_detector", return_value=detector):
            arguments = self.arguments(Path(temporary))
            first, second = review.Review(arguments), review.Review(arguments)
            with patch.object(detector, "Observer", return_value=observer) as constructor, \
                    contextlib.redirect_stdout(io.StringIO()), ThreadPoolExecutor(max_workers=2) as workers:
                futures = [workers.submit(analyzer.prepare, self.video.stem) for analyzer in (first, second)]
                for future in futures:
                    future.result()
            constructor.assert_called_once()
            self.assertEqual(first.days[self.video.stem]["status"], "ready")
            self.assertEqual(second.days[self.video.stem]["status"], "ready")

    def test_clip_export_preserves_frame_order_without_drops_or_duplicates(self):
        colors = np.array([(20, 20, 20), (200, 20, 20), (20, 200, 20), (20, 20, 200),
                           (200, 200, 20), (20, 200, 200), (200, 20, 200), (200, 200, 200)])
        images = np.broadcast_to(colors[:, None, None, :], (8, 64, 96, 3)).astype(np.uint8)
        with tempfile.TemporaryDirectory() as temporary:
            directory = Path(temporary)
            video = directory / "colors.hevc"
            subprocess.run([
                "ffmpeg", "-nostdin", "-v", "error", "-f", "rawvideo", "-pix_fmt", "bgr24",
                "-s", "96x64", "-r", "48", "-i", "pipe:0", "-c:v", "libx265",
                "-x265-params", "pools=none:frame-threads=1:log-level=error", str(video),
            ], input=images.tobytes(), check=True, capture_output=True)
            segments = [{"start_frame": start, "captures": end - start, "file": f"clip-{index:04d}.mp4"}
                        for index, (start, end) in enumerate(((0, 3), (3, 7), (7, 8)))]
            review.encode_clips(video, directory, segments)
            identities = []
            for segment in segments:
                capture = review.cv2.VideoCapture(str(directory / segment["file"]), review.cv2.CAP_FFMPEG,
                                                [review.cv2.CAP_PROP_N_THREADS, 1])
                try:
                    while True:
                        decoded, image = capture.read()
                        if not decoded:
                            break
                        identities.append(int(np.linalg.norm(colors - image.mean(axis=(0, 1)), axis=1).argmin()))
                finally:
                    capture.release()
            self.assertEqual(identities, list(range(8)))


class ServerTests(unittest.TestCase):
    def test_range_requests_head_and_routes(self):
        with tempfile.TemporaryDirectory() as temporary:
            directory = Path(temporary)
            page = directory / "index.html"
            page.write_text("<title>Review</title>")
            (directory / "clip-0000.mp4").write_bytes(b"0123456789")
            analyzer = SimpleNamespace(days={"2026-10-14": {"status": "ready", "directory": directory}},
                                       status=lambda: [{"date": "2026-10-14", "status": "ready"}],
                                       manifest=lambda date: {"date": date})
            request_handler = review.handler(analyzer, page)
            request_handler.log_message = lambda *arguments: None
            server = ThreadingHTTPServer(("127.0.0.1", 0), request_handler)
            worker = threading.Thread(target=server.serve_forever)
            worker.start()
            try:
                connection = http.client.HTTPConnection(*server.server_address)
                connection.request("GET", "/media/2026-10-14/clip-0000.mp4", headers={"Range": "bytes=2-5"})
                response = connection.getresponse()
                self.assertEqual(response.status, 206)
                self.assertEqual(response.getheader("Content-Range"), "bytes 2-5/10")
                self.assertEqual(response.read(), b"2345")
                connection.request("HEAD", "/media/2026-10-14/clip-0000.mp4")
                response = connection.getresponse()
                self.assertEqual(response.status, 200)
                self.assertEqual(response.getheader("Content-Length"), "10")
                self.assertEqual(response.read(), b"")
                connection.request("GET", "/media/2026-10-14/clip-0000.mp4", headers={"Range": "bytes=20-"})
                response = connection.getresponse()
                self.assertEqual(response.status, 416)
                response.read()
                connection.request("GET", "/api/days")
                response = connection.getresponse()
                self.assertEqual(json.loads(response.read())[0]["date"], "2026-10-14")
                connection.request("GET", "/media/2026-10-14/../../index.html")
                response = connection.getresponse()
                self.assertEqual(response.status, 404)
                response.read()
                connection.request("GET", "/api/days", headers={"Host": "another.example"})
                response = connection.getresponse()
                self.assertEqual(response.status, 403)
                response.read()
                connection.close()
            finally:
                server.shutdown()
                worker.join()
                server.server_close()

    def test_suffix_open_ended_and_invalid_ranges(self):
        self.assertEqual(review.byte_range("bytes=-3", 10), (7, 9))
        self.assertEqual(review.byte_range("bytes=3-", 10), (3, 9))
        self.assertEqual(review.byte_range("bytes=3-99", 10), (3, 9))
        for header in ("bytes=-0", "bytes=", "bytes=-", "bytes=8-2", "bytes=0-1,3-4"):
            with self.assertRaises(ValueError):
                review.byte_range(header, 10)


if __name__ == "__main__":
    unittest.main()
