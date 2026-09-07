import importlib.machinery
import importlib.util
import json
from pathlib import Path
import subprocess
import tempfile
import unittest
from unittest.mock import patch


loader = importlib.machinery.SourceFileLoader(
    "env_nix", str(Path(__file__).resolve().parents[1] / "bin/env-nix")
)
specification = importlib.util.spec_from_loader(loader.name, loader)
env_nix = importlib.util.module_from_spec(specification)
loader.exec_module(env_nix)


class SnapshotTests(unittest.TestCase):
    def setUp(self):
        self.temporary = tempfile.TemporaryDirectory()
        self.addCleanup(self.temporary.cleanup)
        self.directory = Path(self.temporary.name)
        self.home = self.directory / "home"
        self.root = self.home / "env"
        (self.root / "nix").mkdir(parents=True)
        self.git = ["git", "-C", str(self.home)]
        self.command("init", "--quiet")
        self.command("config", "user.email", "test@example.invalid")
        self.command("config", "user.name", "Test")
        (self.root / "flake.nix").write_text("committed recipe\n")
        (self.root / "flake.lock").write_text("{}\n")
        (self.root / "nix/packages.nix").write_text("packages\n")
        self.command("add", ".")
        self.command("commit", "--quiet", "-m", "fixture")

    def command(self, *arguments):
        subprocess.run([*self.git, *arguments], check=True, stdout=subprocess.DEVNULL)

    def snapshot(self, name, **options):
        destination = self.directory / name
        env_nix.snapshot(self.root, destination, self.git, "HEAD", prefix="env/",
                         flake=True, **options)
        return destination

    def test_committed_build_ignores_working_files_and_artifacts(self):
        first = self.snapshot("first")
        (self.root / "flake.nix").write_text("dirty recipe\n")
        (self.root / "nix/secret.txt").write_text("excluded\n")
        (self.root / "penrose/target").mkdir(parents=True)
        (self.root / "penrose/target/binary").write_text("artifact\n")
        second = self.snapshot("second")
        self.assertEqual(env_nix.digest(first), env_nix.digest(second))
        self.assertEqual((second / "flake.nix").read_text(), "committed recipe\n")
        self.assertFalse((second / "penrose").exists())
        self.assertFalse((second / "nix/secret.txt").exists())

    def test_working_build_selects_unstaged_contents_and_explicit_files(self):
        (self.root / "flake.nix").write_text("dirty recipe\n")
        (self.root / "nix/new.nix").write_text("new recipe\n")
        first = self.snapshot("first", working=True)
        self.assertEqual((first / "flake.nix").read_text(), "dirty recipe\n")
        self.assertFalse((first / "nix/new.nix").exists())
        second = self.snapshot("second", working=True, include=["nix/new.nix"])
        self.assertTrue((second / "nix/new.nix").exists())
        self.assertNotEqual(env_nix.digest(first), env_nix.digest(second))

    def test_working_deletions_are_preserved(self):
        (self.root / "nix/packages.nix").unlink()
        selected = self.snapshot("selected", working=True)
        self.assertFalse((selected / "nix/packages.nix").exists())

    def test_errlog_source_allowlist_excludes_state_and_build_outputs(self):
        directory = self.root / "errlog-filter"
        (directory / "src").mkdir(parents=True)
        (directory / "src/main.rs").write_text("fn main() {}\n")
        (directory / "rules.toml").write_text("rules = []\n")
        selected = self.snapshot("selected", working=True,
                                 include=["errlog-filter/src/main.rs", "errlog-filter/rules.toml"])
        self.assertTrue((selected / "errlog-filter/src/main.rs").is_file())
        for filename in ("audit.json", "private.toml", "target/output", "src/private.txt"):
            with self.subTest(filename=filename):
                self.assertFalse(env_nix.flake_file(Path("errlog-filter") / filename))

    def test_file_removed_from_index_is_not_silently_included(self):
        self.command("rm", "--cached", "env/nix/packages.nix")
        selected = self.snapshot("selected", working=True)
        self.assertTrue((self.root / "nix/packages.nix").exists())
        self.assertFalse((selected / "nix/packages.nix").exists())

    def test_optional_submodule_contents_are_not_copied(self):
        revision = subprocess.check_output([*self.git, "rev-parse", "HEAD"]).decode().strip()
        self.command("update-index", "--add", "--cacheinfo", "160000", revision,
                     "env/vendor/optional-tests")
        (self.root / "vendor/optional-tests").mkdir(parents=True)
        (self.root / "vendor/optional-tests/private.txt").write_text("not a source input")
        destination = self.directory / "selected"
        env_nix.snapshot(self.root, destination, self.git, "HEAD", working=True, prefix="env/")
        self.assertTrue((destination / "vendor/optional-tests").is_dir())
        self.assertEqual(list((destination / "vendor/optional-tests").iterdir()), [])

    def test_snapshot_is_independent_of_git_caller_directory(self):
        detached_git = ["git", f"--git-dir={self.home / '.git'}",
                        f"--work-tree={self.home}"]
        with patch.object(env_nix.os, "environ", dict(env_nix.os.environ)):
            destination = self.directory / "selected"
            env_nix.snapshot(self.root, destination, detached_git, "HEAD",
                             prefix="env/", flake=True)
        self.assertTrue((destination / "flake.nix").exists())

    def test_escaping_link_is_rejected_before_reading(self):
        (self.root / "nix/packages.nix").unlink()
        (self.root / "nix/packages.nix").symlink_to("/etc/passwd")
        with self.assertRaisesRegex(ValueError, "escapes snapshot"):
            self.snapshot("selected", working=True)

    def test_parent_symlink_is_rejected(self):
        (self.root / "nix/packages.nix").unlink()
        (self.root / "nix").rmdir()
        (self.root / "nix").symlink_to(self.directory)
        with self.assertRaisesRegex(ValueError, "parent is a symlink"):
            self.snapshot("selected", working=True)

    def test_explicit_paths_cannot_escape_allowlist(self):
        for filename in ("../secret", "/etc/passwd", "target/output.nix", "private.txt"):
            with self.subTest(filename=filename), self.assertRaises(ValueError):
                self.snapshot("selected-" + str(len(list(self.directory.iterdir()))),
                              working=True, include=[filename])

    def test_local_patch_changes_snapshot_but_build_artifacts_do_not(self):
        checkout = self.directory / "checkout"
        checkout.mkdir()
        git = ["git", "-C", str(checkout)]
        subprocess.run([*git, "init", "--quiet"], check=True)
        (checkout / "source.c").write_text("original\n")
        subprocess.run([*git, "add", "."], check=True)
        subprocess.run([*git, "-c", "user.name=Test", "-c",
                        "user.email=test@example.invalid", "commit", "--quiet", "-m", "fixture"],
                       check=True)
        first = self.directory / "first"
        env_nix.snapshot(checkout, first, git, "HEAD", working=True)
        (checkout / "target").mkdir()
        (checkout / "target/binary").write_text("artifact")
        second = self.directory / "second"
        env_nix.snapshot(checkout, second, git, "HEAD", working=True)
        self.assertEqual(env_nix.digest(first), env_nix.digest(second))
        (checkout / "source.c").write_text("patched\n")
        third = self.directory / "third"
        env_nix.snapshot(checkout, third, git, "HEAD", working=True)
        self.assertNotEqual(env_nix.digest(first), env_nix.digest(third))


class LockTests(unittest.TestCase):
    def setUp(self):
        self.base = {"root": "root", "version": 7, "nodes": {
            "root": {"inputs": {"package-src": "package-src", "nixpkgs": "nixpkgs"}},
            "package-src": {"flake": False, "locked": {"rev": "original"}},
            "nixpkgs": {"locked": {"rev": "pinned"}},
        }}

    def test_only_requested_input_can_change(self):
        changed = json.loads(json.dumps(self.base))
        changed["nodes"]["package-src"]["locked"] = {"path": "/tmp/source"}
        env_nix.check_override(self.base, changed, "package-src")
        changed["nodes"]["nixpkgs"]["locked"]["rev"] = "unexpected"
        with self.assertRaisesRegex(ValueError, "unrelated"):
            env_nix.check_override(self.base, changed, "package-src")


class ActivationTests(unittest.TestCase):
    def setUp(self):
        self.temporary = tempfile.TemporaryDirectory()
        self.addCleanup(self.temporary.cleanup)
        self.directory = Path(self.temporary.name)
        self.previous = self.directory / "previous-output"
        self.previous.mkdir()
        self.output = self.directory / "output"
        self.output.mkdir()
        self.profile = self.directory / "profile"
        self.profile.symlink_to(self.previous)
        self.record = {"output": str(self.output), "directory": self.directory}

    def test_failed_smoke_test_never_switches_profile(self):
        with patch.object(env_nix, "smoke_test", side_effect=ValueError("failed")), \
                patch.object(env_nix, "run") as command:
            with self.assertRaises(ValueError):
                env_nix.activate(self.profile, self.record)
            command.assert_not_called()
        self.assertEqual(self.profile.resolve(), self.previous)

    def test_activates_exact_output_and_records_recovery(self):
        with patch.object(env_nix, "smoke_test"), patch.object(env_nix, "run") as command:
            env_nix.activate(self.profile, self.record)
        self.assertEqual(command.call_args.args,
                         ("nix-env", "--profile", self.profile, "--set", self.output))
        recovery = json.loads((self.directory / "activation.json").read_text())
        self.assertEqual(recovery["previous"], str(self.previous))
        self.assertEqual(recovery["status"], "active")

    def test_incompatible_profile_is_preserved(self):
        (self.previous / "manifest.json").write_text("{}")
        with self.assertRaisesRegex(ValueError, "nix profile"):
            env_nix.profile_target(self.profile)
        self.assertEqual(self.profile.resolve(), self.previous)

    def test_failed_post_activation_check_restores_previous_output(self):
        def switch(*arguments):
            if arguments[0] == "nix-env":
                self.profile.unlink()
                self.profile.symlink_to(arguments[-1])

        with patch.object(env_nix, "smoke_test", side_effect=[None, ValueError("failed")]), \
                patch.object(env_nix, "run", side_effect=switch):
            with self.assertRaisesRegex(ValueError, "failed"):
                env_nix.activate(self.profile, self.record)
        self.assertEqual(self.profile.resolve(), self.previous)
        recovery = json.loads((self.directory / "activation.json").read_text())
        self.assertEqual(recovery["status"], "failed")

    def test_first_installation_rollback_only_removes_profile_link(self):
        self.profile.unlink()
        self.profile.symlink_to(self.output)
        recovery = self.directory / "activation.json"
        recovery.write_text(json.dumps({"profile": str(self.profile),
                                        "output": str(self.output), "previous": None}))
        env_nix.rollback(recovery, self.directory / "state")
        self.assertFalse(self.profile.is_symlink())
        self.assertTrue(self.output.is_dir())

    def test_rollback_refuses_to_overwrite_a_later_activation(self):
        recovery = self.directory / "activation.json"
        recovery.write_text(json.dumps({"profile": str(self.profile),
                                        "output": str(self.output),
                                        "previous": str(self.previous)}))
        with patch.object(env_nix, "run") as command:
            with self.assertRaisesRegex(ValueError, "changed since"):
                env_nix.rollback(recovery, self.directory / "state")
            command.assert_not_called()


class ArgumentTests(unittest.TestCase):
    def test_documented_option_order(self):
        with patch("sys.argv", ["env-nix", "build", "--working-tree", "bat"]):
            arguments = env_nix.parse_arguments()
        self.assertEqual(arguments.package, "bat")
        self.assertTrue(arguments.working_tree)


class LocalActivationTests(unittest.TestCase):
    def test_override_and_recovery_preserve_previous_link(self):
        with tempfile.TemporaryDirectory() as temporary:
            home = Path(temporary)
            (home / ".local/bin").mkdir(parents=True)
            output = home / "output"
            (output / "bin").mkdir(parents=True)
            (output / "bin/waynav").write_text("binary")
            (output / "bin/waynav").chmod(0o755)
            link = home / ".local/bin/waynav"
            link.symlink_to("../../oss/waynav/old-binary")
            with patch.object(env_nix, "run", return_value=b""):
                env_nix.activate_local(home, "waynav", {"output": str(output), "directory": home}, [])
            self.assertEqual(link.resolve(), output / "bin/waynav")
            env_nix.rollback(home / "activation.json", home / "state")
            self.assertEqual(env_nix.os.readlink(link), "../../oss/waynav/old-binary")

    def test_tracked_link_is_never_replaced(self):
        with tempfile.TemporaryDirectory() as temporary:
            home = Path(temporary)
            (home / "bin").mkdir()
            (home / "bin/waynav").write_text("binary")
            (home / "bin/waynav").chmod(0o755)
            with patch.object(env_nix, "run", return_value=b".local/bin/waynav\0"):
                with self.assertRaisesRegex(ValueError, "tracked file"):
                    env_nix.activate_local(home, "waynav", {"output": str(home), "directory": home}, [])


if __name__ == "__main__":
    unittest.main()
