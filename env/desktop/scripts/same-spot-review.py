#!/usr/bin/env python3
"""Analyze archived webcam days and serve their detected intervals for local review."""

import argparse
from concurrent.futures import ThreadPoolExecutor
from dataclasses import asdict
from datetime import datetime
import fcntl
import hashlib
from http.server import BaseHTTPRequestHandler, ThreadingHTTPServer
import importlib.util
import json
import os
from pathlib import Path
import re
import shutil
import subprocess
import sys
import tempfile
import threading
import time
from urllib.parse import urlsplit

import cv2


ANALYSIS_VERSION = 1
PLAYBACK_RATE = 48
DATE_PATTERN = r"\d{4}-\d{2}-\d{2}"


def load_detector(path):
    specification = importlib.util.spec_from_file_location("same_spot", path)
    detector = importlib.util.module_from_spec(specification)
    sys.modules[specification.name] = detector
    specification.loader.exec_module(detector)
    return detector


def read_timestamps(video):
    date = datetime.strptime(video.stem, "%Y-%m-%d").date()
    companion = video.with_name(video.stem + "-timestamps")
    timestamps = [datetime.combine(date, datetime.strptime(line.strip(), "%H:%M:%S").time()).timestamp()
                  for line in companion.read_text().splitlines()]
    if not timestamps:
        raise ValueError(f"empty timestamp file: {companion}")
    return timestamps


def split_segments(records):
    segments = []
    for index, record in enumerate(records):
        if (not segments or record["session_start"] != records[index - 1]["session_start"]
                or record["event"] == "capture-gap"):
            segments.append({"index": len(segments), "start_frame": index, "end_frame": index,
                             "session_start": record["session_start"],
                             "kind": "streak" if record["session_start"] is not None else "untracked",
                             "reason": record["event"] or "day-start"})
        segments[-1]["end_frame"] = index + 1
    for segment in segments:
        first = records[segment["start_frame"]]
        last = records[segment["end_frame"] - 1]
        segment.update(start=first["timestamp"], end=last["timestamp"],
                       captures=segment["end_frame"] - segment["start_frame"],
                       reminders=sum(record["notification_due"] for record in
                                     records[segment["start_frame"]:segment["end_frame"]]),
                       file=f'clip-{segment["index"]:04d}.mp4')
    return segments


def encode_clips(video, destination, segments):
    boundaries = [segment["start_frame"] for segment in segments[1:]]
    command = ["ffmpeg", "-nostdin", "-v", "error", "-threads", "1", "-i", str(video),
               "-map", "0:v:0", "-an", "-frames:v", str(sum(segment["captures"] for segment in segments)),
               "-vf", f"scale=960:-2,setpts=N/({PLAYBACK_RATE}*TB)",
               "-c:v", "libx264", "-preset", "veryfast", "-crf", "25", "-pix_fmt", "yuv420p",
               "-threads", "2", "-bf", "0", "-g", "10", "-r", str(PLAYBACK_RATE),
               "-fps_mode", "cfr"]
    if boundaries:
        command += ["-force_key_frames", ",".join(str(frame / PLAYBACK_RATE) for frame in boundaries),
                    "-f", "segment", "-segment_frames", ",".join(map(str, boundaries)),
                    "-segment_format", "mp4", "-segment_format_options", "movflags=+faststart",
                    "-reset_timestamps", "1", str(destination / "clip-%04d.mp4")]
    else:
        command += ["-movflags", "+faststart", str(destination / "clip-0000.mp4")]
    subprocess.run(command, check=True, capture_output=True, text=True, timeout=300)
    files = sorted(destination.glob("clip-*.mp4"))
    if len(files) != len(segments):
        raise ValueError(f"expected {len(segments)} clips, encoded {len(files)}")
    # Every capture must belong to exactly one browser clip, including boundary frames.
    for segment in segments:
        result = subprocess.run([
            "ffprobe", "-v", "error", "-count_frames", "-select_streams", "v:0",
            "-show_entries", "stream=nb_read_frames", "-of", "csv=p=0", str(destination / segment["file"]),
        ], check=True, capture_output=True, text=True, timeout=60)
        if int(result.stdout.strip()) != segment["captures"]:
            raise ValueError(f'frame count mismatch in {segment["file"]}')


class Review:
    def __init__(self, arguments):
        self.arguments = arguments
        self.detector = load_detector(arguments.detector_script)
        self.policy = self.detector.Policy()
        self.lock = threading.Lock()
        self.days = {}
        configuration = {"version": ANALYSIS_VERSION, "policy": asdict(self.policy),
                         "threshold": arguments.presence_threshold, "playback_rate": PLAYBACK_RATE,
                         "min_face_fraction": arguments.min_face_fraction,
                         "detector": hashlib.sha256(arguments.detector_script.read_bytes()).hexdigest(),
                         "model": hashlib.sha256(arguments.model.read_bytes()).hexdigest()}
        self.configuration = json.dumps(configuration, sort_keys=True).encode()
        previous_configuration = json.dumps(configuration | {"playback_rate": 2}, sort_keys=True).encode()
        archives = ([arguments.root] if arguments.root.is_file()
                    else sorted(arguments.root.glob("*.hevc"), reverse=True))
        for video in archives:
            if not re.fullmatch(DATE_PATTERN + r"\.hevc", video.name):
                continue
            if arguments.date and video.stem not in arguments.date:
                continue
            source = video.stat()
            try:
                timestamps = read_timestamps(video)
                companion = video.with_name(video.stem + "-timestamps").read_bytes()
                identity = companion + f"{source.st_size}:{source.st_mtime_ns}".encode()
                key = hashlib.sha256(self.configuration + identity).hexdigest()[:20]
                previous_key = hashlib.sha256(previous_configuration + identity).hexdigest()[:20]
                directory = arguments.cache / video.stem / key
                ready = (directory / "manifest.json").is_file()
                self.days[video.stem] = {"date": video.stem, "video": video, "directory": directory,
                                         "previous_directory": arguments.cache / video.stem / previous_key,
                                         "key": key, "timestamps": timestamps, "captures": len(timestamps),
                                         "processed": len(timestamps) if ready else 0,
                                         "status": "ready" if ready else "queued"}
            except (OSError, ValueError) as error:
                self.days[video.stem] = {"date": video.stem, "status": "error", "error": str(error)}

    def status(self):
        with self.lock:
            return [{name: value for name, value in day.items()
                     if name in {"date", "key", "status", "error", "captures", "processed", "segments"}}
                    for day in self.days.values()]

    def update(self, date, **changes):
        with self.lock:
            self.days[date].update(changes)

    def prepare(self, date):
        day = self.days[date]
        if day["status"] != "queued":
            return
        try:
            directory = day["directory"]
            directory.parent.mkdir(parents=True, exist_ok=True)
            # The viewer and a separate batch command can share the same cache.
            with directory.with_suffix(".lock").open("a") as lock:
                self.update(date, status="waiting")
                fcntl.flock(lock, fcntl.LOCK_EX)
                if (directory / "manifest.json").is_file():
                    self.update(date, status="ready", processed=day["captures"])
                    return
                self.analyze(date)
        except OSError as error:
            self.update(date, status="error", error=str(error))

    def analyze(self, date):
        day = self.days[date]
        directory = day["directory"]
        temporary = None
        try:
            directory.parent.mkdir(parents=True, exist_ok=True)
            temporary = Path(tempfile.mkdtemp(prefix=day["key"] + ".", dir=directory.parent))
            self.update(date, status="analyzing")
            print(f"{date}: analyzing {day['captures']} captures", flush=True)
            records = []
            previous_manifest = day["previous_directory"] / "manifest.json"
            if previous_manifest.is_file():
                records = json.loads(previous_manifest.read_text())["frames"]
                print(f"{date}: reusing {len(records)} analyzed captures", flush=True)
            else:
                observer = self.detector.Observer(self.arguments.model, self.arguments.presence_threshold,
                                                   self.arguments.min_face_fraction)
                tracker = self.detector.Tracker()
                capture = cv2.VideoCapture(str(day["video"]), cv2.CAP_FFMPEG, [cv2.CAP_PROP_N_THREADS, 1])
                try:
                    if not capture.isOpened():
                        raise ValueError(f"cannot decode {day['video']}")
                    for index, timestamp in enumerate(day["timestamps"]):
                        decoded, image = capture.read()
                        if not decoded:
                            raise ValueError(f"video ended at frame {index}; expected {day['captures']}")
                        started = time.perf_counter()
                        record = tracker.step(timestamp, observer.observe_image(image), self.policy)
                        record["processing_ms"] = round((time.perf_counter() - started) * 1000, 1)
                        if record["notification_due"]:
                            tracker.notified(timestamp, self.policy)
                        records.append(record)
                        if index % 50 == 0:
                            self.update(date, processed=index + 1)
                    if capture.read()[0]:
                        raise ValueError("video has more frames than its timestamp file")
                finally:
                    capture.release()
            segments = split_segments(records)
            self.update(date, status="encoding", processed=len(records), segments=len(segments))
            print(f"{date}: encoding {len(segments)} clips", flush=True)
            encode_clips(day["video"], temporary, segments)
            manifest = {"version": ANALYSIS_VERSION, "date": date, "key": day["key"],
                        "playback_rate": PLAYBACK_RATE, "policy": asdict(self.policy),
                        "presence_threshold": self.arguments.presence_threshold,
                        "min_face_fraction": self.arguments.min_face_fraction,
                        "segments": segments, "frames": records}
            (temporary / "manifest.json").write_text(json.dumps(manifest, separators=(",", ":")) + "\n")
            if directory.exists():
                shutil.rmtree(directory)
            temporary.replace(directory)
            self.update(date, status="ready")
            print(f"{date}: ready ({len(records)} captures, {len(segments)} clips)", flush=True)
        except (OSError, ValueError, cv2.error, subprocess.SubprocessError) as error:
            detail = error.stderr.strip() if isinstance(error, subprocess.CalledProcessError) else str(error)
            self.update(date, status="error", error=detail)
            print(f"{date}: {detail}", file=sys.stderr, flush=True)
        finally:
            if temporary is not None and temporary.exists():
                shutil.rmtree(temporary)

    def manifest(self, date):
        with self.lock:
            day = self.days.get(date)
            if day is None or day["status"] != "ready":
                return None
            path = day["directory"] / "manifest.json"
        return json.loads(path.read_text())


def byte_range(header, size):
    if header is None:
        return 0, size - 1
    match = re.fullmatch(r"bytes=(\d*)-(\d*)", header)
    if match is None or not any(match.groups()):
        raise ValueError("invalid byte range")
    first, last = match.groups()
    start = int(first) if first else max(0, size - int(last))
    end = min(size - 1, int(last)) if first and last else size - 1
    if start >= size or end < start:
        raise ValueError("unsatisfiable byte range")
    return start, end


def handler(review, page):
    class Handler(BaseHTTPRequestHandler):
        def do_GET(self):
            self.respond(False)

        def do_HEAD(self):
            self.respond(True)

        def respond(self, head):
            if self.headers.get("Host", "").split(":")[0].lower() not in {"127.0.0.1", "localhost"}:
                self.send_error(403)
                return
            path = urlsplit(self.path).path
            try:
                if path == "/":
                    self.send_bytes(page.read_bytes(), "text/html; charset=utf-8", head)
                elif path == "/api/days":
                    self.send_bytes(json.dumps(review.status()).encode(), "application/json", head)
                elif match := re.fullmatch(r"/api/days/(" + DATE_PATTERN + r")", path):
                    manifest = review.manifest(match[1])
                    if manifest is None:
                        self.send_error(404, "Day is not ready")
                    else:
                        self.send_bytes(json.dumps(manifest).encode(), "application/json", head)
                elif match := re.fullmatch(r"/media/(" + DATE_PATTERN + r")/(clip-\d{4}\.mp4)", path):
                    day = review.days.get(match[1])
                    if day is None or day["status"] != "ready":
                        self.send_error(404)
                    else:
                        self.send_video(day["directory"] / match[2], head)
                else:
                    self.send_error(404)
            except (BrokenPipeError, ConnectionResetError):
                pass
            except FileNotFoundError:
                self.send_error(404)

        def send_bytes(self, content, content_type, head):
            self.send_response(200)
            self.send_header("Content-Type", content_type)
            self.send_header("Content-Length", str(len(content)))
            self.send_header("Cache-Control", "no-store")
            self.end_headers()
            if not head:
                self.wfile.write(content)

        def send_video(self, path, head):
            with path.open("rb") as source:
                size = path.stat().st_size
                requested = self.headers.get("Range")
                try:
                    start, end = byte_range(requested, size)
                except ValueError:
                    self.send_response(416)
                    self.send_header("Content-Range", f"bytes */{size}")
                    self.send_header("Content-Length", "0")
                    self.end_headers()
                    return
                self.send_response(206 if requested else 200)
                self.send_header("Content-Type", "video/mp4")
                self.send_header("Accept-Ranges", "bytes")
                self.send_header("Content-Length", str(end - start + 1))
                self.send_header("Cache-Control", "no-cache")
                if requested:
                    self.send_header("Content-Range", f"bytes {start}-{end}/{size}")
                self.end_headers()
                if not head:
                    source.seek(start)
                    remaining = end - start + 1
                    while remaining:
                        content = source.read(min(remaining, 64 * 1024))
                        if not content:
                            break
                        self.wfile.write(content)
                        remaining -= len(content)

        def log_message(self, format, *arguments):
            if arguments and str(arguments[0]).startswith("GET /api/days "):
                return
            super().log_message(format, *arguments)

    return Handler


def parse_arguments():
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument("root", type=Path, nargs="?", default=Path.home() / "pics/webcam")
    parser.add_argument("--model", type=Path, required=True)
    parser.add_argument("--detector-script", type=Path, default=Path(__file__).with_name("same-spot.py"))
    parser.add_argument("--page", type=Path, default=Path(__file__).parents[1] / "same-spot-review.html")
    parser.add_argument("--cache", type=Path, default=Path(os.environ.get(
        "XDG_CACHE_HOME", Path.home() / ".cache")) / "same-spot-review")
    parser.add_argument("--date", action="append", help="process only this YYYY-MM-DD (repeatable)")
    parser.add_argument("--presence-threshold", type=float, default=0.85)
    parser.add_argument("--min-face-fraction", type=float, default=0.2,
                        help="minimum face height as a fraction of image height (default: 0.2)")
    parser.add_argument("--workers", type=int, default=2)
    parser.add_argument("--port", type=int, default=8765)
    parser.add_argument("--analyze-only", action="store_true", help="prepare cached analysis and clips, then exit")
    arguments = parser.parse_args()
    if not arguments.root.exists() or not arguments.model.is_file() or not arguments.detector_script.is_file():
        parser.error("capture root, model, and detector script must exist")
    if not arguments.analyze_only and not arguments.page.is_file():
        parser.error(f"review page not found: {arguments.page}")
    if (not 0.3 < arguments.presence_threshold <= 1 or not 1 <= arguments.workers <= 8
            or not 0 <= arguments.min_face_fraction <= 1):
        parser.error("invalid presence, face size, or worker count")
    return arguments


def main():
    os.umask(0o077)
    arguments = parse_arguments()
    review = Review(arguments)
    if not review.days:
        raise ValueError("no matching YYYY-MM-DD.hevc archives found")
    workers = ThreadPoolExecutor(max_workers=arguments.workers)
    try:
        if arguments.analyze_only:
            list(workers.map(review.prepare, review.days))
            return int(any(day["status"] == "error" for day in review.days.values()))
        with ThreadingHTTPServer(("127.0.0.1", arguments.port), handler(review, arguments.page)) as server:
            for date in review.days:
                workers.submit(review.prepare, date)
            print(f"Review at http://127.0.0.1:{arguments.port} ({len(review.days)} days)", flush=True)
            server.serve_forever()
    except KeyboardInterrupt:
        workers.shutdown(wait=True, cancel_futures=True)
    finally:
        workers.shutdown(wait=True)
    return 0


if __name__ == "__main__":
    try:
        sys.exit(main())
    except (OSError, ValueError) as error:
        print(f"same-spot-review: {error}", file=sys.stderr)
        sys.exit(1)
