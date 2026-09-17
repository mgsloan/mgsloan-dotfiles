import importlib.machinery
import importlib.util
import os
from pathlib import Path
import shutil
import subprocess
import tempfile
import unittest
from unittest.mock import patch


loader = importlib.machinery.SourceFileLoader(
    "agent_sandbox", str(Path(__file__).resolve().parents[1] / "bin/agent-sandbox"))
specification = importlib.util.spec_from_loader(loader.name, loader)
agent_sandbox = importlib.util.module_from_spec(specification)
loader.exec_module(agent_sandbox)


class SandboxHomeTests(unittest.TestCase):
    def test_only_selected_credentials_are_forwarded_without_command_line_exposure(self):
        with patch.object(agent_sandbox.shutil, "which", return_value="/usr/bin/true"), \
             patch.dict(os.environ, {
                 "OPENAI_API_KEY": "test-openai-secret",
                 "ANTHROPIC_API_KEY": "test-anthropic-secret",
                 "AWS_SECRET_ACCESS_KEY": "test-aws-secret",
             }):
            command, environment = agent_sandbox.sandbox_command(
                Path("/project"), Path("/nonexistent-host"), Path("/sandbox"), "codex", [])
        self.assertEqual(environment["OPENAI_API_KEY"], "test-openai-secret")
        self.assertNotIn("ANTHROPIC_API_KEY", environment)
        self.assertNotIn("AWS_SECRET_ACCESS_KEY", environment)
        self.assertNotIn("test-openai-secret", " ".join(command))

    def test_credentials_are_copied_once_without_global_configuration(self):
        with tempfile.TemporaryDirectory() as temporary:
            home = Path(temporary) / "host"
            (home / ".claude").mkdir(parents=True)
            credentials = home / ".claude/.credentials.json"
            credentials.write_text("original login")
            (home / ".claude/settings.json").write_text("private settings")
            destination = Path(temporary) / "sandbox/home"
            agent_sandbox.initialize_home(destination, home, "claude")
            copied = destination / ".claude/.credentials.json"
            self.assertEqual(copied.read_text(), "original login")
            self.assertEqual(copied.stat().st_mode & 0o777, 0o600)
            self.assertFalse((destination / ".claude/settings.json").exists())
            copied.unlink()
            copied.symlink_to(credentials)
            agent_sandbox.initialize_home(destination, home, "claude")
            self.assertEqual(credentials.read_text(), "original login")


@unittest.skipUnless(shutil.which("bwrap"), "bubblewrap is not installed")
class SandboxBoundaryTests(unittest.TestCase):
    def setUp(self):
        temporary = tempfile.TemporaryDirectory(prefix="agent-sandbox-test-")
        self.addCleanup(temporary.cleanup)
        self.directory = Path(temporary.name)
        self.home = self.directory / "host"
        self.workspace = self.home / "project with spaces"
        self.workspace.mkdir(parents=True)
        self.sandbox_home = self.directory / "sandbox"
        self.sandbox_home.mkdir()
        self.secret = self.home / "secret"
        self.secret.write_text("outside workspace")
        self.bubblewrap = shutil.which("bwrap")

    def run_sandbox(self, script, offline=False):
        def which(name):
            return self.bubblewrap if name == "bwrap" else "/usr/bin/true"

        with patch.object(agent_sandbox.shutil, "which", side_effect=which), \
             patch.dict(os.environ, {"PRIVATE_TEST_TOKEN": "must not leak", "GIT_DIR": str(self.home)}):
            command, environment = agent_sandbox.sandbox_command(
                self.workspace, self.home, self.sandbox_home, "codex",
                ["-c", script, "sandbox-test", str(self.secret), str(os.getpid())],
                offline=offline, shell=True)
        return subprocess.run(command, env=environment, capture_output=True, text=True)

    def test_project_and_sandbox_home_writable_but_host_hidden(self):
        (self.workspace / "escape").symlink_to(self.secret)
        result = self.run_sandbox('''
set -eu
test "$PWD" = /workspace
test -z "${PRIVATE_TEST_TOKEN-}${GIT_DIR-}${SSH_AUTH_SOCK-}${DBUS_SESSION_BUS_ADDRESS-}"
test ! -e "$1"
test ! -e /workspace/escape
test ! -e "/proc/$2"
test ! -e /run/user
test ! -e /nix/var/nix/daemon-socket/socket
printf project > /workspace/created
printf session > "$HOME/session"
printf temporary > /tmp/created
if printf changed > /etc/passwd 2>/dev/null; then exit 1; fi
if printf changed > /opt/agent/bin/codex 2>/dev/null; then exit 1; fi
if printf changed > /workspace/escape 2>/dev/null; then exit 1; fi
''')
        self.assertEqual(result.returncode, 0, result.stderr)
        self.assertEqual((self.workspace / "created").read_text(), "project")
        self.assertEqual((self.sandbox_home / "session").read_text(), "session")
        self.assertEqual(self.secret.read_text(), "outside workspace")
        repeated = self.run_sandbox('test "$(cat "$HOME/session")" = session; test ! -e /tmp/created')
        self.assertEqual(repeated.returncode, 0, repeated.stderr)

    def test_offline_has_a_separate_network_namespace(self):
        host_namespace = os.readlink("/proc/self/ns/net")
        online = self.run_sandbox("readlink /proc/self/ns/net")
        self.assertEqual(online.returncode, 0, online.stderr)
        self.assertEqual(online.stdout.strip(), host_namespace)
        offline = self.run_sandbox("readlink /proc/self/ns/net", offline=True)
        self.assertEqual(offline.returncode, 0, offline.stderr)
        self.assertNotEqual(offline.stdout.strip(), host_namespace)

    def test_bubblewrap_failure_does_not_run_the_command(self):
        self.bubblewrap = "/usr/bin/false"
        result = self.run_sandbox("touch /workspace/should-not-exist")
        self.assertNotEqual(result.returncode, 0)
        self.assertFalse((self.workspace / "should-not-exist").exists())


if __name__ == "__main__":
    unittest.main()
