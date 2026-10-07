import importlib.machinery
import importlib.util
import json
import os
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

    def test_nix_patches_are_flake_files(self):
        self.assertTrue(env_nix.flake_file(Path("nix/patches/river.patch")))

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


class UpdateTests(unittest.TestCase):
    def test_latest_stable_tag_uses_numeric_order_and_excludes_prereleases(self):
        tags = b"first\trefs/tags/v1.9.0\nsecond\trefs/tags/v1.10.0\nthird\trefs/tags/v2.0.0-rc1\n"
        with patch.object(env_nix, "run", return_value=tags) as command:
            self.assertEqual(env_nix.latest_source_url("github:owner/project/old-commit"),
                             "github:owner/project/v1.10.0")
        self.assertEqual(command.call_args.args,
                         ("git", "ls-remote", "--refs", "https://github.com/owner/project.git", "refs/tags/*"))

    def test_no_stable_tags_follows_default_branch(self):
        with patch.object(env_nix, "run", return_value=b"first\trefs/tags/v1.0.0-beta\n"):
            self.assertEqual(env_nix.latest_source_url("github:owner/project/old-commit"),
                             "github:owner/project")

    def test_gitlab_host_and_git_transport_are_preserved(self):
        with patch.object(env_nix, "run", return_value=b"first\trefs/tags/0.20.2\n") as command:
            self.assertEqual(env_nix.latest_source_url(
                "gitlab:wlroots/wlroots/0.20.1?host=gitlab.freedesktop.org"),
                "gitlab:wlroots/wlroots/0.20.2?host=gitlab.freedesktop.org")
            self.assertIn("https://gitlab.freedesktop.org/wlroots/wlroots.git", command.call_args.args)
            self.assertEqual(env_nix.latest_source_url("git+https://codeberg.org/river/river.git?rev=old"),
                             "git+https://codeberg.org/river/river.git?ref=refs/tags/0.20.2")

    def test_named_update_preserves_other_inputs(self):
        with tempfile.TemporaryDirectory() as temporary:
            flake = Path(temporary)
            recipe = flake / "flake.nix"
            original = ('{ inputs = { spotifast = { url = "github:crmne/spotifast/v0.9.1"; }; '
                        'ghostty-src = { url = "github:ghostty-org/ghostty/old"; flake = false; }; }; }\n')
            recipe.write_text(original)
            with patch.object(env_nix, "latest_source_url", return_value="github:crmne/spotifast/v0.10.0"):
                env_nix.refresh_sources(flake, "spotifast")
            self.assertEqual(recipe.read_text(), original.replace("v0.9.1", "v0.10.0"))

    def test_update_refuses_concurrent_recipe_or_lock_edits(self):
        with tempfile.TemporaryDirectory() as temporary:
            root = Path(temporary) / "root"
            flake = Path(temporary) / "snapshot"
            root.mkdir()
            flake.mkdir()
            for directory in (root, flake):
                (directory / "flake.nix").write_bytes(b"original recipe")
                (directory / "flake.lock").write_bytes(b"original lock")
            (flake / "flake.nix").write_bytes(b"updated recipe")
            (flake / "flake.lock").write_bytes(b"updated lock")
            for filename in ("flake.nix", "flake.lock"):
                with self.subTest(filename=filename):
                    path = root / filename
                    original = path.read_bytes()
                    path.write_bytes(b"concurrent edit")
                    with self.assertRaisesRegex(ValueError, "refusing to overwrite"):
                        env_nix.record_update(root, flake, b"original recipe", b"original lock")
                    self.assertEqual(path.read_bytes(), b"concurrent edit")
                    path.write_bytes(original)
            env_nix.record_update(root, flake, b"original recipe", b"original lock")
            self.assertEqual((root / "flake.nix").read_bytes(), b"updated recipe")
            self.assertEqual((root / "flake.lock").read_bytes(), b"updated lock")


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
    def test_update_can_refresh_all_inputs(self):
        with patch("sys.argv", ["env-nix", "update"]):
            arguments = env_nix.parse_arguments()
        self.assertIsNone(arguments.package)

    def test_activation_can_update_with_public_option(self):
        with patch("sys.argv", ["env-nix", "activate", "--update", "--public"]):
            arguments = env_nix.parse_arguments()
        self.assertTrue(arguments.update)
        self.assertTrue(arguments.public)

    def test_documented_option_order(self):
        with patch("sys.argv", ["env-nix", "build", "--working-tree", "bat"]):
            arguments = env_nix.parse_arguments()
        self.assertEqual(arguments.package, "bat")
        self.assertTrue(arguments.working_tree)


class PrivateEnvironmentTests(unittest.TestCase):
    command = SnapshotTests.command

    def setUp(self):
        SnapshotTests.setUp(self)
        self.private = self.home / "ep"
        self.private.mkdir()
        self.private_git = ["git", "-C", str(self.private)]
        for arguments in (("init", "--quiet"), ("config", "user.email", "test@example.invalid"),
                          ("config", "user.name", "Test")):
            subprocess.run([*self.private_git, *arguments], check=True)
        (self.private / "flake.nix").write_text("private recipe\n")
        (self.private / "flake.lock").write_text("{}\n")
        (self.private / "credentials.txt").write_text("excluded\n")
        subprocess.run([*self.private_git, "add", "."], check=True)
        subprocess.run([*self.private_git, "commit", "--quiet", "-m", "fixture"], check=True)

    def test_private_snapshot_excludes_unrelated_files_and_honors_source_mode(self):
        (self.private / "flake.nix").write_text("working recipe\n")
        for working in (False, True):
            destination = self.directory / str(working)
            record = env_nix.private_snapshot(self.home, destination, working)
            self.assertEqual(record["files"], ["flake.lock", "flake.nix"])
            self.assertEqual((destination / "flake.nix").read_text(),
                             "working recipe\n" if working else "private recipe\n")

    def test_activation_defaults_to_private_with_explicit_public_option(self):
        original_run = env_nix.run
        for public in (False, True):
            calls = []

            def run(*arguments, **options):
                if arguments[0] != "nix":
                    # The fixture uses an ordinary repository instead of ~/.home.git.
                    if arguments[0] == "git" and str(arguments[1]).startswith("--git-dir="):
                        arguments = (*self.git, *arguments[3:])
                    return original_run(*arguments, **options)
                calls.append(arguments)
                if arguments[1] == "build":
                    return json.dumps([{"outputs": {"out": str(self.directory / "output")}}]).encode()

            with patch("sys.argv", ["env-nix", "activate", *(["--public"] if public else [])]), \
                 patch.object(env_nix.Path, "home", return_value=self.home), \
                 patch.object(env_nix, "run", side_effect=run), \
                 patch.object(env_nix, "metadata", return_value={}), \
                 patch.object(env_nix.shutil, "which", return_value="nix"), \
                 patch.object(env_nix, "activate") as activate:
                env_nix.main()
            build = next(call for call in calls if call[1] == "build")
            self.assertTrue(build[2].endswith("#environment" if public else "#private-environment"))
            self.assertEqual("--override-input" in build, not public)
            activate.assert_called_once()

    def test_failed_check_after_update_does_not_activate(self):
        original_run = env_nix.run
        calls = []

        def run(*arguments, **options):
            if arguments[0] != "nix":
                if arguments[0] == "git" and str(arguments[1]).startswith("--git-dir="):
                    arguments = (*self.git, *arguments[3:])
                return original_run(*arguments, **options)
            calls.append(arguments)
            if arguments[1:3] == ("flake", "check"):
                raise subprocess.CalledProcessError(1, arguments)

        with patch("sys.argv", ["env-nix", "activate", "--update"]), \
             patch.object(env_nix.Path, "home", return_value=self.home), \
             patch.object(env_nix, "run", side_effect=run), \
             patch.object(env_nix, "refresh_sources"), \
             patch.object(env_nix, "metadata", return_value={}), \
             patch.object(env_nix.shutil, "which", return_value="nix"), \
             patch.object(env_nix, "activate") as activate:
            with self.assertRaises(subprocess.CalledProcessError):
                env_nix.main()
        self.assertEqual([call[1:3] for call in calls], [("flake", "update"), ("flake", "check")])
        activate.assert_not_called()


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


class LocalOverrideTests(unittest.TestCase):
    def setUp(self):
        self.temporary = tempfile.TemporaryDirectory()
        self.addCleanup(self.temporary.cleanup)
        self.home = Path(self.temporary.name)
        self.state = self.home / ".local/state/env-nix"
        self.activations = 0

    def activate(self, package, name):
        output = self.home / name
        executable = output / "bin" / package
        executable.parent.mkdir(parents=True, exist_ok=True)
        executable.write_text("binary")
        executable.chmod(0o755)
        self.activations += 1
        directory = self.state / "builds" / f"{self.activations:04}"
        directory.mkdir(parents=True)
        with patch.object(env_nix, "run", return_value=b""), patch.object(env_nix, "report"):
            env_nix.activate_local(self.home, package,
                                   {"output": str(output), "directory": directory}, [])
        return directory / "activation.json"

    def summary(self):
        with patch.object(env_nix, "report") as report:
            env_nix.report_local_overrides(self.home, self.state)
        return "\n".join(call.args[0] for call in report.call_args_list)

    def test_command_works_without_git_nix_or_existing_state(self):
        with patch("sys.argv", ["env-nix", "overrides"]), \
             patch.object(env_nix.Path, "home", return_value=self.home), \
             patch.object(env_nix, "run") as command, \
             patch.object(env_nix, "report") as report:
            env_nix.main()
        command.assert_not_called()
        report.assert_called_once_with("Local overrides: none")
        self.assertFalse(self.state.exists())

    def test_summary_follows_replacement_and_rollback(self):
        previous = self.activate("ghostty", "previous-output")
        current = self.activate("ghostty", "current-output")
        waynav = self.activate("waynav", "waynav-output")
        summary = self.summary()
        self.assertNotIn(str(previous), summary)
        self.assertIn(str(current), summary)
        self.assertIn(str(waynav), summary)
        self.assertLess(summary.index("ghostty ->"), summary.index("waynav ->"))
        env_nix.rollback(current, self.state)
        summary = self.summary()
        self.assertIn(str(previous), summary)
        self.assertNotIn(str(current), summary)
        env_nix.rollback(previous, self.state)
        self.assertNotIn("ghostty ->", self.summary())

    def test_removed_or_manually_replaced_links_are_not_overrides(self):
        self.activate("ghostty", "ghostty-output")
        self.activate("waynav", "waynav-output")
        (self.home / ".local/bin/ghostty").unlink()
        link = self.home / ".local/bin/waynav"
        link.unlink()
        link.symlink_to("/another/binary")
        self.assertEqual(self.summary(), "Local overrides: none")

    def test_repeated_activation_lists_latest_recovery_once(self):
        previous = self.activate("ghostty", "same-output")
        current = self.activate("ghostty", "same-output")
        summary = self.summary()
        self.assertEqual(summary.count("ghostty ->"), 1)
        self.assertNotIn(str(previous), summary)
        self.assertIn(str(current), summary)


class FreshenNixTests(unittest.TestCase):
    def test_override_summary_runs_after_activation_and_services(self):
        with tempfile.TemporaryDirectory() as temporary:
            home = Path(temporary)
            (home / "env/bin").mkdir(parents=True)
            helper = home / "env/bin/env-nix"
            helper.write_text('#!/bin/sh\nprintf "%s\\n" "$*"\n')
            helper.chmod(0o755)
            (home / "env/setup").mkdir()
            (home / "env/setup/050-nix-services.py").write_text('print("services")\n')
            script = Path(__file__).resolve().parents[1] / "nix/scripts/freshen-nix.sh"
            output = subprocess.check_output(
                ["bash", str(script), "--public"], env={**os.environ, "HOME": str(home)},
                text=True,
            )
        self.assertEqual(output.splitlines(), [
            "activate --working-tree --update --include nix/patches/river-keystroke-histograms.patch --public",
            "services",
            "overrides",
        ])


if __name__ == "__main__":
    unittest.main()
