#!/usr/bin/env python3
"""Estimate time in one spot from existing webcam JPEGs and remind you to move."""

import argparse
from dataclasses import dataclass, field
from datetime import datetime, timedelta
import fcntl
import json
import math
import os
from pathlib import Path
import subprocess
import sys
import time

import cv2
import numpy as np


THUMBNAIL_SIZE = (48, 27)
MAX_FRAME_INTERVAL = 90
MIN_FACE_FRACTION = 0.2


@dataclass
class Scene:
    pixels: np.ndarray
    background: np.ndarray

    def serialize(self):
        return {"pixels": self.pixels.tolist(), "background": self.background.tolist()}

    @classmethod
    def deserialize(cls, value):
        scene = cls(np.asarray(value["pixels"], dtype=np.uint8),
                    np.asarray(value["background"], dtype=bool))
        if scene.pixels.shape != THUMBNAIL_SIZE[::-1] or scene.background.shape != scene.pixels.shape:
            raise ValueError("invalid scene dimensions")
        return scene

    def similarity(self, other):
        best = None
        height, width = self.pixels.shape
        for vertical in range(-3, 4):
            for horizontal in range(-3, 4):
                first = (slice(max(0, vertical), min(height, height + vertical)),
                         slice(max(0, horizontal), min(width, width + horizontal)))
                second = (slice(max(0, -vertical), min(height, height - vertical)),
                          slice(max(0, -horizontal), min(width, width - horizontal)))
                common = self.background[first] & other.background[second]
                # Small overlaps and nearly blank walls cannot establish a location.
                if common.sum() < height * width / 4:
                    continue
                first_values = self.pixels[first][common].astype(np.float32)
                second_values = other.pixels[second][common].astype(np.float32)
                first_values -= first_values.mean()
                second_values -= second_values.mean()
                if min(first_values.std(), second_values.std()) < 8:
                    continue
                score = float(np.dot(first_values, second_values)
                              / (np.linalg.norm(first_values) * np.linalg.norm(second_values)))
                best = score if best is None else max(best, score)
        return best


@dataclass
class Observation:
    kind: str
    score: float = 0
    luminance: float = 0
    sharpness: float = 0
    scene: Scene | None = None
    face_height_fraction: float = 0


class Observer:
    def __init__(self, model, threshold, min_face_fraction=MIN_FACE_FRACTION):
        cv2.setNumThreads(1)
        self.threshold = threshold
        self.min_face_fraction = min_face_fraction
        self.detector = cv2.FaceDetectorYN.create(str(model), "", (640, 360), 0.3)
        self.contrast = cv2.createCLAHE(clipLimit=2, tileGridSize=(8, 8))

    def detect(self, image):
        self.detector.setInputSize((image.shape[1], image.shape[0]))
        faces = self.detector.detect(image)[1]
        return np.empty((0, 15)) if faces is None else faces

    def observe(self, path):
        return self.observe_image(cv2.imread(str(path)))

    def observe_image(self, image):
        if image is None:
            return Observation("uncertain")
        height, width = image.shape[:2]
        image = cv2.resize(image, (640, round(height * 640 / width)), interpolation=cv2.INTER_AREA)
        gray = cv2.cvtColor(image, cv2.COLOR_BGR2GRAY)
        luminance = float(gray.mean())
        sharpness = float(cv2.Laplacian(gray, cv2.CV_32F).var())
        faces = self.detect(image)
        def eligible_score(candidates):
            nearby = candidates[candidates[:, 3] >= gray.shape[0] * self.min_face_fraction]
            return float(nearby[:, -1].max()) if len(nearby) else 0

        score = eligible_score(faces)
        if score < self.threshold:
            enhanced = cv2.cvtColor(self.contrast.apply(gray), cv2.COLOR_GRAY2BGR)
            alternatives = self.detect(enhanced)
            alternative_score = eligible_score(alternatives)
            if alternative_score > score:
                faces, score = alternatives, alternative_score
        if score < self.threshold:
            kind = "no-face" if score < 0.3 and luminance >= 25 and sharpness >= 15 else "uncertain"
            face_height_fraction = max((float(face[3] / gray.shape[0]) for face in faces), default=0)
            return Observation(kind, score, luminance, sharpness,
                               face_height_fraction=face_height_fraction)

        background = np.ones(gray.shape, dtype=np.uint8)
        for face in faces:
            left, top, face_width, face_height = face[:4]
            # Exclude the head, shoulders, and torso, including uncertain face candidates.
            first_column = max(0, int(left - face_width))
            last_column = min(gray.shape[1], math.ceil(left + 2 * face_width))
            first_row = max(0, int(top - face_height / 2))
            background[first_row:, first_column:last_column] = 0
        pixels = cv2.resize(gray, THUMBNAIL_SIZE, interpolation=cv2.INTER_AREA)
        mask = cv2.resize(background.astype(np.float32), THUMBNAIL_SIZE,
                          interpolation=cv2.INTER_AREA) > 0.99
        nearby = faces[faces[:, 3] >= gray.shape[0] * self.min_face_fraction]
        face_height_fraction = float(nearby[nearby[:, -1].argmax(), 3] / gray.shape[0])
        return Observation("present", score, luminance, sharpness, Scene(pixels, mask),
                           face_height_fraction)


@dataclass
class Policy:
    first: float = 45 * 60
    repeat: float = 15 * 60
    break_duration: float = 5 * 60
    gap: float = 5 * 60
    scene_different: float = 0.35


@dataclass
class Tracker:
    session_start: float | None = None
    next_notification: float | None = None
    last_timestamp: float | None = None
    last_present: float | None = None
    absent_since: float | None = None
    interrupted: bool = False
    return_since: float | None = None
    different_since: float | None = None
    gallery: list = field(default_factory=list)
    notification_id: int = 0

    def reset(self):
        self.session_start = self.next_notification = self.last_present = None
        self.absent_since = self.return_since = self.different_since = None
        self.interrupted = False
        self.gallery.clear()

    def start(self, timestamp, policy):
        self.reset()
        self.session_start = timestamp
        self.next_notification = timestamp + policy.first

    def step(self, timestamp, observation, policy):
        event = None
        previous = self.last_timestamp
        if previous is not None and (timestamp <= previous or timestamp - previous > policy.gap):
            self.reset()
            event = "capture-gap"
        continuous = previous is not None and 0 < timestamp - previous <= MAX_FRAME_INTERVAL
        self.last_timestamp = timestamp
        similarity = None
        if observation.scene is not None:
            scores = [observation.scene.similarity(scene) for scene in self.gallery]
            similarity = max((score for score in scores if score is not None), default=None)

        if observation.kind != "present":
            self.interrupted = True
            self.return_since = self.different_since = None
            if observation.kind == "no-face":
                if self.absent_since is None or not continuous:
                    self.absent_since = timestamp
                if self.session_start is not None and timestamp - self.absent_since >= policy.break_duration:
                    self.reset()
                    event = "probable-break"
            else:
                self.absent_since = None
        else:
            if self.session_start is None:
                self.start(timestamp, policy)
                event = event or "started"
            elif self.last_present is not None and timestamp - self.last_present > policy.gap:
                self.start(timestamp, policy)
                event = "unobserved"
            elif self.interrupted or not continuous:
                self.interrupted = True
                self.return_since = timestamp if self.return_since is None else self.return_since
                if similarity is not None and similarity >= 0.65:
                    self.interrupted = False
                    event = "returned"
                else:
                    different = similarity is not None and similarity < policy.scene_different
                    if different:
                        self.different_since = timestamp if self.different_since is None else self.different_since
                    else:
                        self.different_since = None
                    if self.different_since is not None and timestamp - self.different_since >= 60:
                        self.start(timestamp, policy)
                        event = "moved"
                    elif self.different_since is None and timestamp - self.return_since >= 60:
                        self.interrupted = False
                        event = "returned-uncertain-scene"
            self.last_present = timestamp
            self.absent_since = None
            if not self.interrupted:
                self.return_since = self.different_since = None
                if observation.scene is not None and (not self.gallery or similarity is None or similarity < 0.9):
                    # Preserve the first view while rotating the remaining reference views.
                    self.gallery = (self.gallery[:1] + self.gallery[-6:] if len(self.gallery) >= 8
                                    else self.gallery)
                    self.gallery.append(observation.scene)

        due = (observation.kind == "present" and not self.interrupted
               and self.next_notification is not None and timestamp >= self.next_notification)
        return {"timestamp": timestamp, "observation": observation.kind, "score": round(observation.score, 3),
                "luminance": round(observation.luminance, 1), "sharpness": round(observation.sharpness, 1),
                "face_height_fraction": round(observation.face_height_fraction, 3),
                "similarity": round(similarity, 3) if similarity is not None else None,
                "event": event, "session_start": self.session_start,
                "minutes": round((timestamp - self.session_start) / 60, 1) if self.session_start is not None else None,
                "notification_due": due}

    def notified(self, timestamp, policy):
        # Skip missed reminders after a gap instead of delivering a backlog.
        periods = math.floor((timestamp - self.session_start - policy.first) / policy.repeat) + 1
        self.next_notification = self.session_start + policy.first + periods * policy.repeat

    def save(self, path):
        value = vars(self) | {"gallery": [scene.serialize() for scene in self.gallery]}
        temporary = path.with_suffix(".tmp")
        temporary.write_text(json.dumps({"version": 1, "tracker": value}, separators=(",", ":")) + "\n")
        temporary.replace(path)

    @classmethod
    def load(cls, path):
        if not path.exists():
            return cls()
        try:
            value = json.loads(path.read_text())
            if value["version"] != 1:
                raise ValueError("unsupported state version")
            values = value["tracker"]
            values["gallery"] = [Scene.deserialize(scene) for scene in values["gallery"]]
            tracker = cls(**values)
            for name in ("session_start", "next_notification", "last_timestamp", "last_present",
                         "absent_since", "return_since", "different_since"):
                timestamp = getattr(tracker, name)
                if timestamp is not None and (not isinstance(timestamp, (int, float)) or not math.isfinite(timestamp)):
                    raise ValueError(f"invalid {name}")
            if (tracker.session_start is None) != (tracker.next_notification is None) or len(tracker.gallery) > 8:
                raise ValueError("inconsistent session state")
            return tracker
        except (ValueError, KeyError, TypeError) as error:
            print(f"same-spot: ignoring invalid state: {error}", file=sys.stderr)
            return cls()


def capture_timestamp(path):
    try:
        return datetime.strptime(f"{path.parent.name} {path.stem}", "%Y-%m-%d %H-%M-%S").timestamp()
    except ValueError:
        return None


def captures(root, live=False):
    if live:
        today = datetime.now().date()
        directories = [root / str(today - timedelta(days=1)), root / str(today)]
    else:
        directories = [root] + sorted(path for path in root.iterdir() if path.is_dir())
    result = []
    for directory in directories:
        for path in directory.glob("*.jpg"):
            timestamp = capture_timestamp(path)
            if timestamp is not None:
                result.append((timestamp, path))
    return sorted(result)


def notifications_allowed():
    deadline = Path(os.environ.get("XDG_STATE_HOME", Path.home() / ".local/state")) / "penrose/webcam-inhibit-until"
    try:
        if float(deadline.read_text()) > time.time():
            return False
    except FileNotFoundError:
        pass
    except (OSError, ValueError):
        return False
    return subprocess.run(["pgrep", "-x", "slock|swaylock"], stdout=subprocess.DEVNULL,
                          check=False).returncode == 1


def notify(tracker, timestamp):
    minutes = int((timestamp - tracker.session_start) / 60)
    since = datetime.fromtimestamp(tracker.session_start).strftime("%H:%M")
    result = subprocess.run([
        "notify-send", "--print-id", "--replace-id", str(tracker.notification_id),
        "--app-name", "same-spot", "--urgency", "normal", "--expire-time", "15000",
        f"Same spot for about {minutes} minutes",
        f"Since {since}, with no detected break. Time to get up and move?",
    ], capture_output=True, text=True, check=True, timeout=10)
    tracker.notification_id = int(result.stdout.strip())


def replay(arguments, observer, policy):
    tracker = Tracker()
    for timestamp, path in captures(arguments.replay):
        if arguments.since is not None and timestamp < arguments.since:
            continue
        if arguments.until is not None and timestamp > arguments.until:
            continue
        started = time.perf_counter()
        record = tracker.step(timestamp, observer.observe(path), policy)
        record["processing_ms"] = round((time.perf_counter() - started) * 1000, 1)
        record["image"] = str(path)
        if record["notification_due"]:
            tracker.notified(timestamp, policy)
        print(json.dumps(record), flush=True)


def watch(arguments, observer, policy):
    arguments.state.parent.mkdir(parents=True, exist_ok=True, mode=0o700)
    with arguments.state.with_suffix(".lock").open("w") as lock:
        fcntl.flock(lock, fcntl.LOCK_EX | fcntl.LOCK_NB)
        tracker = Tracker.load(arguments.state)
        while True:
            if tracker.last_timestamp is not None and tracker.last_timestamp > time.time() + MAX_FRAME_INTERVAL:
                tracker = Tracker()
            available = captures(arguments.watch, live=True)
            if tracker.last_timestamp is None:
                pending = available[-1:]
            else:
                pending = [(timestamp, path) for timestamp, path in available if timestamp > tracker.last_timestamp]
            for timestamp, path in pending:
                record = tracker.step(timestamp, observer.observe(path), policy)
                if (record["notification_due"] and timestamp == available[-1][0]
                        and 0 <= time.time() - timestamp <= MAX_FRAME_INTERVAL):
                    if notifications_allowed():
                        try:
                            if not arguments.dry_run:
                                notify(tracker, timestamp)
                            tracker.notified(timestamp, policy)
                            record["notified"] = not arguments.dry_run
                        except (OSError, ValueError, subprocess.SubprocessError) as error:
                            print(f"same-spot: notification failed: {error}", file=sys.stderr)
                tracker.save(arguments.state)
                print(json.dumps(record), flush=True)
            if arguments.once:
                return
            time.sleep(arguments.poll_seconds)


def parse_arguments():
    parser = argparse.ArgumentParser(description=__doc__)
    mode = parser.add_mutually_exclusive_group(required=True)
    mode.add_argument("--watch", type=Path, help="watch YYYY-MM-DD/HH-MM-SS.jpg captures")
    mode.add_argument("--replay", type=Path, help="replay a capture root or a single date directory; never notify or save state")
    parser.add_argument("--model", type=Path, required=True, help="YuNet ONNX model")
    parser.add_argument("--state", type=Path, default=Path(os.environ.get(
        "XDG_STATE_HOME", Path.home() / ".local/state")) / "same-spot/state.json")
    parser.add_argument("--first-minutes", type=float, default=45)
    parser.add_argument("--repeat-minutes", type=float, default=15)
    parser.add_argument("--break-minutes", type=float, default=5)
    parser.add_argument("--gap-minutes", type=float, default=5,
                        help="start a fresh estimate after this long without reliable presence")
    parser.add_argument("--presence-threshold", type=float, default=0.85)
    parser.add_argument("--min-face-fraction", type=float, default=MIN_FACE_FRACTION,
                        help="minimum face height as a fraction of image height (default: 0.2)")
    parser.add_argument("--scene-different", type=float, default=0.35)
    parser.add_argument("--poll-seconds", type=float, default=10)
    parser.add_argument("--since", type=lambda value: datetime.fromisoformat(value).timestamp())
    parser.add_argument("--until", type=lambda value: datetime.fromisoformat(value).timestamp())
    parser.add_argument("--dry-run", action="store_true", help="watch and save state without sending notifications")
    parser.add_argument("--once", action="store_true", help="process pending images and exit")
    arguments = parser.parse_args()
    for name in ("first_minutes", "repeat_minutes", "break_minutes", "gap_minutes", "poll_seconds"):
        value = getattr(arguments, name)
        if not math.isfinite(value) or value <= 0:
            parser.error(f"--{name.replace('_', '-')} must be positive and finite")
    if (not 0.3 < arguments.presence_threshold <= 1 or not -1 <= arguments.scene_different < 0.65
            or not 0 <= arguments.min_face_fraction <= 1):
        parser.error("invalid presence, face size, or scene threshold")
    if not arguments.model.is_file():
        parser.error(f"model not found: {arguments.model}")
    if arguments.replay and not arguments.replay.is_dir():
        parser.error(f"capture directory not found: {arguments.replay}")
    if arguments.watch and (arguments.since is not None or arguments.until is not None):
        parser.error("--since and --until apply only to replay")
    return arguments


def main():
    os.umask(0o077)
    arguments = parse_arguments()
    policy = Policy(arguments.first_minutes * 60, arguments.repeat_minutes * 60,
                    arguments.break_minutes * 60, arguments.gap_minutes * 60, arguments.scene_different)
    observer = Observer(arguments.model, arguments.presence_threshold, arguments.min_face_fraction)
    (replay if arguments.replay else watch)(arguments, observer, policy)


if __name__ == "__main__":
    try:
        main()
    except (OSError, ValueError, cv2.error) as error:
        print(f"same-spot: {error}", file=sys.stderr)
        sys.exit(1)
