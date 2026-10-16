"""Exercise the bridge against Herdr and Zed sockets and a recording emacsclient."""

import json
import os
from pathlib import Path
import socket
import subprocess
import tempfile
import threading
import time
import unittest


class BridgeTest(unittest.TestCase):
    def setUp(self):
        self.temporary = tempfile.TemporaryDirectory(prefix="herdr-switch-")
        self.root = Path(self.temporary.name)
        self.desktop = self.root / "desktop.json"
        self.calls = self.root / "calls.jsonl"
        self.zed_calls = self.root / "zed.jsonl"
        self.publish(True)
        self.listener = socket.socket(socket.AF_UNIX)
        self.socket_path = self.root / "herdr.sock"
        self.listener.bind(str(self.socket_path))
        self.listener.listen()
        self.subscribed = threading.Event()
        self.subscription = None
        self.pane = {"pane_id": "first", "cwd": "/first"}
        threading.Thread(target=self.serve, daemon=True).start()
        client = self.root / "emacsclient"
        client.write_text(
            "#!/usr/bin/env python3\n"
            "import json, os, pathlib, time\n"
            "root = pathlib.Path(os.environ['BRIDGE_TEST_ROOT'])\n"
            "request = json.loads((root / 'desktop.request.json').read_text())\n"
            "with (root / 'calls.jsonl').open('a') as output:\n"
            "    output.write(json.dumps(request) + '\\n')\n"
            "time.sleep(0.2)\n"
        )
        client.chmod(0o755)
        self.zed_directory = self.root / "data" / "zed"
        self.zed_directory.mkdir(parents=True)
        self.zed_sockets = {}
        for channel in ("stable", "preview", "nightly", "dev"):
            listener = socket.socket(socket.AF_UNIX, socket.SOCK_DGRAM)
            listener.bind(str(self.zed_directory / f"zed-{channel}.sock"))
            listener.setblocking(False)
            self.zed_sockets[channel] = listener
        executable = Path(__file__).resolve().parents[1] / "target/debug/herdr-switch-sync"
        self.process = subprocess.Popen(
            [str(executable), str(self.desktop), str(os.getpid())],
            env={**os.environ, "HERDR_SOCKET_PATH": str(self.socket_path),
                 "PATH": f"{self.root}:{os.environ['PATH']}",
                 "XDG_DATA_HOME": str(self.zed_directory.parent),
                 "BRIDGE_TEST_ROOT": str(self.root)},
            stderr=subprocess.PIPE, text=True,
        )
        self.assertTrue(self.subscribed.wait(3))

    def tearDown(self):
        self.process.terminate()
        self.process.communicate(timeout=3)
        if self.subscription:
            self.subscription.close()
        self.listener.close()
        for listener in self.zed_sockets.values():
            listener.close()
        self.temporary.cleanup()

    def publish(self, visible, emacs=True, zed=False):
        temporary = self.desktop.with_suffix(".tmp")
        temporary.write_text(json.dumps({
            "owner": os.getpid(), "herdr": visible,
            "frames": [{"pid": os.getpid(), "title": "status - Emacs"}] if emacs else [],
            "zed": ["stable"] if zed is True else zed or [],
        }))
        temporary.replace(self.desktop)

    def serve(self):
        while True:
            try:
                connection, _ = self.listener.accept()
            except OSError:
                return
            with connection.makefile("r") as reader:
                request = json.loads(reader.readline())
            if request["method"] == "events.subscribe":
                self.subscription = connection
                connection.sendall(b'{"id":"magit","result":{"type":"subscription_started"}}\n')
                self.subscribed.set()
            else:
                connection.sendall((json.dumps({"id": "magit", "result": {
                    "type": "pane_current", "pane": self.pane,
                }}) + "\n").encode())
                connection.close()

    def focus(self, pane, directory):
        self.pane = {"pane_id": pane, "foreground_cwd": directory, "cwd": "/fallback"}
        self.subscription.sendall((json.dumps({
            "event": "pane_focused", "data": {"type": "pane_focused", "pane_id": pane, "workspace_id": "w1"},
        }) + "\n").encode())

    def recorded(self, path=None):
        path = path or self.calls
        if path == self.zed_calls:
            with path.open("a") as output:
                for channel, listener in self.zed_sockets.items():
                    while listener.fileno() >= 0:
                        try:
                            url = listener.recv(4096).decode()
                        except BlockingIOError:
                            break
                        output.write(json.dumps([channel, url]) + "\n")
        if not path.exists():
            return []
        return [json.loads(line) for line in path.read_text().splitlines()]

    def wait_calls(self, count, path=None):
        deadline = time.monotonic() + 3
        while time.monotonic() < deadline:
            calls = self.recorded(path)
            if len(calls) >= count:
                return calls
            time.sleep(0.01)
        self.fail(f"expected {count} calls, got {self.recorded(path)}")

    def test_switches_and_refreshes_same_repository(self):
        self.focus("one", "/checkout")
        self.assertEqual(self.wait_calls(1)[0]["directory"], "/checkout")
        self.focus("two", "/checkout")
        self.assertEqual(self.wait_calls(2)[1]["directory"], "/checkout")
        self.assertEqual(self.recorded(self.zed_calls), [])

    def test_zed_without_emacs_uses_worktree_root(self):
        repository = self.root / "repository"
        checkout = self.root / "worktree with spaces"
        environment = {key: value for key, value in os.environ.items()
                       if key not in ("GIT_DIR", "GIT_WORK_TREE", "GIT_INDEX_FILE")}
        def git(*arguments):
            subprocess.run(["git", *arguments], env=environment, check=True,
                           stdout=subprocess.DEVNULL, stderr=subprocess.PIPE)
        git("init", str(repository))
        git("-C", str(repository), "-c", "user.name=Test", "-c", "user.email=test@example.org",
            "-c", "commit.gpgsign=false", "commit", "--allow-empty", "-m", "initial")
        git("-C", str(repository), "worktree", "add", "-b", "test", str(checkout))
        nested = checkout / "nested"
        nested.mkdir()
        self.publish(True, emacs=False, zed=True)
        self.focus("zed", str(nested))
        self.assertEqual(self.wait_calls(1, self.zed_calls), [["stable", checkout.as_uri().replace("file://", "zed://herdr/")]])
        self.assertEqual(self.recorded(), [])

    def test_both_editors_and_non_repository_fallback(self):
        self.publish(True, zed=True)
        directory = str(self.root / 'directory with "quotes" % # ? λ')
        Path(directory).mkdir()
        self.focus("both", directory)
        self.assertEqual(self.wait_calls(1)[0]["directory"], directory)
        self.assertEqual(self.wait_calls(1, self.zed_calls), [["stable", Path(directory).as_uri().replace("file://", "zed://herdr/")]])

    def test_zed_requires_both_displayed_workspaces(self):
        for herdr, zed in [(False, True), (True, False)]:
            self.publish(herdr, emacs=False, zed=zed)
            self.focus("hidden", "/hidden")
            time.sleep(0.2)
        self.publish(True, emacs=False, zed=True)
        time.sleep(0.2)
        self.assertEqual(self.recorded(self.zed_calls), [])
        self.focus("visible", "/visible")
        self.assertEqual(self.wait_calls(1, self.zed_calls), [["stable", "zed://herdr//visible"]])

    def test_zed_channels_follow_only_displayed_windows(self):
        for index, channel in enumerate(self.zed_sockets, start=1):
            self.publish(True, emacs=False, zed=[channel])
            self.focus(channel, f"/{channel}")
            calls = self.wait_calls(index, self.zed_calls)
            self.assertEqual(calls[-1], [channel, f"zed://herdr//{channel}"])
            self.assertEqual(len(calls), index)

    def test_unavailable_zed_does_not_block_other_editors(self):
        self.zed_sockets["dev"].close()
        self.publish(True, zed=["dev", "stable"])
        self.focus("both", "/checkout")
        self.assertEqual(self.wait_calls(1)[0]["directory"], "/checkout")
        self.assertEqual(self.wait_calls(1, self.zed_calls), [["stable", "zed://herdr//checkout"]])

    def test_zed_rejects_urls_that_would_be_truncated(self):
        self.publish(True, emacs=False, zed=["dev"])
        self.focus("long", "/" + "a" * 1024)
        time.sleep(0.2)
        self.assertEqual(self.recorded(self.zed_calls), [])
        self.focus("short", "/short")
        self.assertEqual(self.wait_calls(1, self.zed_calls), [["dev", "zed://herdr//short"]])

    def test_rapid_switches_keep_latest(self):
        self.publish(True, zed=True)
        self.focus("one", "/one")
        self.focus("two", "/two")
        self.focus("three", "/three")
        self.assertEqual(self.wait_calls(1)[0]["directory"], "/three")
        time.sleep(0.3)
        self.assertEqual(len(self.recorded()), 1)
        self.assertEqual(self.wait_calls(1, self.zed_calls), [["stable", "zed://herdr//three"]])

    def test_hidden_switch_is_not_replayed(self):
        self.publish(False)
        self.focus("hidden", "/hidden")
        time.sleep(0.2)
        self.publish(True)
        time.sleep(0.2)
        self.assertEqual(self.recorded(), [])
        self.focus("visible", "/visible")
        self.assertEqual(self.wait_calls(1)[0]["directory"], "/visible")

    def test_pending_request_is_invalidated(self):
        self.focus("one", "/one")
        first = self.wait_calls(1)[0]
        self.focus("two", "/two")
        time.sleep(0.06)
        pending = json.loads(self.desktop.with_suffix(".request.json").read_text())
        self.assertGreater(pending["generation"], first["generation"])
        self.assertIsNone(pending["directory"])
        self.assertEqual(self.wait_calls(2)[1]["directory"], "/two")


if __name__ == "__main__":
    unittest.main()
