import importlib.util
from pathlib import Path
import tempfile
import unittest


specification = importlib.util.spec_from_file_location(
    "archive_nix", Path(__file__).resolve().parents[1] / "scripts/archive-nix-replacements.py")
archive_nix = importlib.util.module_from_spec(specification)
specification.loader.exec_module(archive_nix)


class ArchiveTests(unittest.TestCase):
    def setUp(self):
        self.temporary = tempfile.TemporaryDirectory()
        self.addCleanup(self.temporary.cleanup)
        self.home = Path(self.temporary.name)
        self.local = self.home / ".local"
        self.archive = self.home / ".local-old"
        (self.local / "bin").mkdir(parents=True)

    def test_selection_preserves_personal_scripts_desktop_and_uv_data(self):
        for name in ("just", "uv", "uvx", "cfg", "freshen", "dunst", "ghostty"):
            (self.local / "bin" / name).write_text(name)
        (self.local / "share/uv/tools").mkdir(parents=True)
        completion = self.local / "share/bash-completion/completions/just"
        completion.parent.mkdir(parents=True)
        completion.write_text("completion")
        selected = archive_nix.candidates(self.local)
        self.assertEqual(set(selected), {Path("bin/just"), Path("bin/uv"),
                                         Path("bin/uvx"), completion.relative_to(self.local)})

    def test_move_preserves_relative_symlink_and_its_target(self):
        target = self.home / "old-just"
        target.write_text("binary")
        (self.local / "bin/just").symlink_to("../../old-just")
        archive_nix.move_files(self.local, self.archive, {Path("bin/just"): "just"})
        self.assertFalse((self.local / "bin/just").is_symlink())
        self.assertEqual((self.archive / "bin/just").resolve(), target)
        self.assertEqual(target.read_text(), "binary")

    def test_new_cli_installations_preserve_package_stores(self):
        commands = ("stack", "pnpm", "pnpx", "wasm-pack", "gcloud", "gsutil", "bq")
        for command in commands:
            (self.local / "bin" / command).write_text("binary")
        (self.local / "google-cloud-sdk/bin").mkdir(parents=True)
        (self.local / "google-cloud-sdk/bin/gcloud").write_text("sdk")
        (self.local / "share/pnpm/store").mkdir(parents=True)
        (self.local / "share/pnpm/store/package").write_text("keep")
        (self.local / "share/pnpm/pnpm").write_text("launcher")
        (self.local / "share/pnpm/pnpx").write_text("launcher")
        selected = archive_nix.candidates(self.local)
        self.assertEqual(set(selected), {*(Path("bin") / command for command in commands),
                                         Path("google-cloud-sdk"), Path("share/pnpm/pnpm"),
                                         Path("share/pnpm/pnpx")})

    def test_desktop_is_opt_in_and_preserves_shared_libraries(self):
        for command in ("ghostty", "river"):
            (self.local / "bin" / command).write_text("binary")
        assets = (
            "share/ghostty/shell-integration/bash/ghostty.bash",
            "share/terminfo/x/xterm-ghostty",
            "share/icons/hicolor/128x128/apps/com.mitchellh.ghostty.png",
            "share/wayland-sessions/river.desktop",
            "lib/libwlroots-0.20.so",
        )
        for filename in assets:
            path = self.local / filename
            path.parent.mkdir(parents=True, exist_ok=True)
            path.write_text("asset")
        self.assertEqual(archive_nix.candidates(self.local), {})
        self.assertEqual(set(archive_nix.candidates(self.local, desktop=True)), {
            Path("bin/ghostty"), Path("bin/river"), Path("share/ghostty"),
            *(Path(filename) for filename in assets[1:-1]),
        })

    def test_move_installation_directory_preserves_contents(self):
        directory = self.local / "google-cloud-sdk/bin"
        directory.mkdir(parents=True)
        (directory / "gcloud").write_text("sdk")
        archive_nix.move_files(self.local, self.archive,
                               {Path("google-cloud-sdk"): "gcloud"})
        self.assertFalse(directory.exists())
        self.assertEqual((self.archive / "google-cloud-sdk/bin/gcloud").read_text(), "sdk")

    def test_desktop_symlink_parent_is_rejected(self):
        (self.local / "share").symlink_to(self.home, target_is_directory=True)
        with self.assertRaisesRegex(ValueError, "symlink"):
            archive_nix.candidates(self.local, desktop=True)

    def test_collision_prevents_all_moves(self):
        for command in ("just", "uv"):
            (self.local / "bin" / command).write_text("original")
        (self.archive / "bin").mkdir(parents=True)
        (self.archive / "bin/uv").write_text("previous archive")
        with self.assertRaisesRegex(ValueError, "already exists"):
            archive_nix.move_files(self.local, self.archive,
                                   {Path("bin/just"): "just", Path("bin/uv"): "uv"})
        self.assertTrue((self.local / "bin/just").exists())
        self.assertEqual((self.archive / "bin/uv").read_text(), "previous archive")

    def test_archive_symlink_is_rejected(self):
        self.archive.symlink_to(self.local, target_is_directory=True)
        with self.assertRaisesRegex(ValueError, "symlink"):
            archive_nix.check_destinations(self.local, self.archive, [Path("bin/just")])


if __name__ == "__main__":
    unittest.main()
