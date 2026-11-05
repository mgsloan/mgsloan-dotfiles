import importlib.machinery
import importlib.util
import os
from pathlib import Path
import subprocess
import tempfile
import unittest
from unittest.mock import patch


ROOT = Path(__file__).resolve().parents[1]
loader = importlib.machinery.SourceFileLoader("env_debian", str(ROOT / "bin/env-debian"))
specification = importlib.util.spec_from_loader(loader.name, loader)
env_debian = importlib.util.module_from_spec(specification)
loader.exec_module(env_debian)


class DebianPackageTests(unittest.TestCase):
    def setUp(self):
        self.temporary = tempfile.TemporaryDirectory()
        self.addCleanup(self.temporary.cleanup)
        self.directory = Path(self.temporary.name)

    def test_checkout_snapshot_keeps_working_changes_and_index(self):
        source = self.directory / "source"
        source.mkdir()
        subprocess.run(["git", "init", "--quiet", str(source)], check=True)
        tracked = source / "source.c"
        tracked.write_text("before\n")
        subprocess.run(["git", "-C", str(source), "add", "source.c"], check=True)
        index = (source / ".git/index").read_bytes()
        tracked.write_text("after\n")
        (source / "untracked.c").write_text("new source\n")
        (source / ".env").write_text("excluded\n")
        (source / "target").mkdir()
        (source / "target/artifact").write_text("excluded\n")
        destination = self.directory / "snapshot"
        destination.mkdir()
        env_debian.copy_checkout(source, destination)
        self.assertEqual((destination / "source.c").read_text(), "after\n")
        self.assertEqual((destination / "untracked.c").read_text(), "new source\n")
        self.assertFalse((destination / ".env").exists())
        self.assertFalse((destination / "target").exists())
        self.assertEqual((source / ".git/index").read_bytes(), index)
        self.assertEqual(tracked.read_text(), "after\n")

    def test_snapshot_rejects_links_outside_the_checkout(self):
        source = self.directory / "source"
        source.mkdir()
        subprocess.run(["git", "init", "--quiet", str(source)], check=True)
        outside = self.directory / "outside"
        outside.write_text("outside\n")
        (source / "link").symlink_to(outside)
        destination = self.directory / "snapshot"
        destination.mkdir()
        with self.assertRaisesRegex(ValueError, "escapes checkout"):
            env_debian.copy_checkout(source, destination)

    def test_rollback_restores_links_and_backend(self):
        state = self.directory / "state"
        state.mkdir()
        backend = state / "backend"
        backend.write_text("debian\n")
        backup = state / "saved-link"
        backup.symlink_to("/nix/store/previous/bin/program")
        destination = self.directory / "program"
        record = {"previous_backend": None, "environment": {"PATH": "/previous/bin"},
                  "links": [{"path": str(destination), "backup": str(backup)}]}
        with patch.object(env_debian, "STATE", state), patch.object(env_debian, "run"):
            env_debian.restore_activation(record)
        self.assertTrue(destination.is_symlink())
        self.assertEqual(os.readlink(destination), "/nix/store/previous/bin/program")
        self.assertFalse(backend.exists())
        self.assertFalse(backup.is_symlink())

    def test_activation_preserves_independent_nix_launchers(self):
        binaries = self.directory / ".local/bin"
        binaries.mkdir(parents=True)
        managed = binaries / "waynav"
        managed.symlink_to("/nix/store/managed/bin/waynav")
        independent = binaries / "zathura"
        independent.symlink_to("/nix/store/independent/bin/zathura")
        native = binaries / "local-tool"
        native.symlink_to("/usr/bin/true")
        self.assertEqual(env_debian.nix_launch_links(self.directory, {"waynav", "local-tool"}), [managed])
        self.assertTrue(independent.is_symlink())

    def test_rollback_preserves_a_new_user_file(self):
        state = self.directory / "state"
        state.mkdir()
        (state / "backend").write_text("debian\n")
        backup = state / "saved-link"
        backup.symlink_to("/nix/store/previous/bin/program")
        destination = self.directory / "program"
        destination.write_text("user replacement\n")
        record = {"previous_backend": None,
                  "links": [{"path": str(destination), "backup": str(backup)}]}
        with patch.object(env_debian, "STATE", state), patch.object(env_debian, "run") as command:
            with self.assertRaisesRegex(ValueError, "overwriting"):
                env_debian.restore_activation(record)
            command.assert_not_called()
        self.assertEqual(destination.read_text(), "user replacement\n")
        self.assertTrue(backup.is_symlink())
        self.assertEqual((state / "backend").read_text(), "debian\n")

    def test_session_reloads_selection_and_clears_nix_graphics(self):
        home = self.directory
        (home / "env").symlink_to(ROOT, target_is_directory=True)
        state = home / ".local/state/debian-packages"
        state.mkdir(parents=True)
        profile = home / ".local/state/nix/profiles/env"
        (profile / "bin").mkdir(parents=True)
        environment = os.environ.copy()
        environment.update(HOME=str(home), NIX_ENV_PROFILE=str(profile),
                           PATH="/nix/store/compiler/bin:/usr/bin:/bin",
                           XDG_DATA_DIRS="/nix/store/environment/share:/usr/share",
                           LD_LIBRARY_PATH="/nix/store/mesa/lib",
                           LIBGL_DRIVERS_PATH="/nix/store/mesa/dri",
                           GST_PLUGIN_SYSTEM_PATH_1_0="/nix/store/gstreamer/lib",
                           GHOSTTY_BIN_DIR="/nix/store/ghostty/bin", TERMINFO="/nix/store/ghostty/share/terminfo")
        environment.pop("ENV_PACKAGE_BACKEND", None)
        result = subprocess.run(["sh", "-eu", "-c", '''
. "$HOME/env/debian/session.sh"
printf 'debian\\n' > "$HOME/.local/state/debian-packages/backend"
. "$HOME/env/debian/session.sh"
printf '%s\\n' "$PATH" "$XDG_DATA_DIRS" "${LD_LIBRARY_PATH-unset}" "${LIBGL_DRIVERS_PATH-unset}" "${GST_PLUGIN_SYSTEM_PATH_1_0-unset}" "${GHOSTTY_BIN_DIR-unset}" "${TERMINFO-unset}"
'''], env=environment, capture_output=True, text=True, check=True)
        self.assertNotIn("/nix/", result.stdout)
        self.assertNotIn(str(profile), result.stdout)
        self.assertEqual(result.stdout.splitlines()[-5:], ["unset"] * 5)


if __name__ == "__main__":
    unittest.main()
