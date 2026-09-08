#!/usr/bin/env python3
"""Archive Nix-replaced installations to ~/.local-old; preview unless --apply is given."""

import argparse
import os
from pathlib import Path
import subprocess
import sys


# Keep this list aligned with commandLinePackages in nix/packages.nix.
COMMANDS = (
    "bat", "joshuto", "just", "lychee", "pandoc", "qpdf", "rg",
    "shellcheck", "uv", "uvx", "xidlehook", "stack", "pnpm", "pnpx", "wasm-pack",
    "gcloud", "gsutil", "bq", "zig", "typst", "git-credential-libsecret",
)
DESKTOP_COMMANDS = ("asdcontrol", "darkman", "dunst", "dunstctl", "ghostty", "keynav", "river", "waynav")
INSTALLATION_DIRECTORIES = {
    "google-cloud-sdk": "gcloud",
}
DESKTOP_DIRECTORIES = {
    "share/ghostty": "ghostty",
}
DESKTOP_FILES = {
    "share/applications/com.mitchellh.ghostty.desktop": "ghostty",
    "share/dbus-1/services/com.mitchellh.ghostty.service": "ghostty",
    "share/metainfo/com.mitchellh.ghostty.metainfo.xml": "ghostty",
    "share/terminfo/x/xterm-ghostty": "ghostty",
    "share/terminfo/78/xterm-ghostty": "ghostty",
    "share/wayland-sessions/river.desktop": "river",
}


def exists(path):
    return path.exists() or path.is_symlink()


def check_parents(path, root):
    for parent in (path.parent, *path.parent.parents):
        if parent.is_symlink():
            raise ValueError(f"refusing to traverse a symlink: {parent}")
        if parent == root:
            return
    raise ValueError(f"path is outside {root}: {path}")


def candidates(local, desktop=False):
    selected = {Path("share/pnpm/pnpm"): "pnpm", Path("share/pnpm/pnpx"): "pnpx"}
    for command in COMMANDS + (DESKTOP_COMMANDS if desktop else ()):
        selected[Path("bin") / command] = command
        for section in range(1, 10):
            for suffix in ("", ".gz", ".xz", ".bz2", ".zst"):
                selected[Path(f"share/man/man{section}/{command}.{section}{suffix}")] = command
        for filename in (
            f"share/bash-completion/completions/{command}",
            f"share/bash-completion/completions/{command}.bash",
            f"share/zsh/site-functions/_{command}",
            f"share/fish/vendor_completions.d/{command}.fish",
        ):
            selected[Path(filename)] = command
    directories = dict(INSTALLATION_DIRECTORIES)
    if desktop:
        directories.update(DESKTOP_DIRECTORIES)
        selected.update((Path(filename), command) for filename, command in DESKTOP_FILES.items())
        for path in (local / "share/icons/hicolor").glob("*/apps/com.mitchellh.ghostty.*"):
            selected[path.relative_to(local)] = "ghostty"
    selected.update((Path(filename), command) for filename, command in directories.items())
    result = {}
    for relative, command in selected.items():
        path = local / relative
        check_parents(path, local)
        if not exists(path):
            continue
        if path.is_symlink() and path.resolve().is_relative_to("/nix/store"):
            print(f"Keeping Nix link: {path}", file=sys.stderr)
            continue
        if str(relative) in directories and path.is_dir() and not path.is_symlink():
            # Move only dedicated installation trees, never shared stores or config.
            result[relative] = command
            continue
        if not path.is_symlink() and not path.is_file():
            raise ValueError(f"expected a file or symlink: {path}")
        result[relative] = command
    return result


def check_destinations(local, archive, selected):
    for relative in selected:
        check_parents(local / relative, local)
        destination = archive / relative
        check_parents(destination, archive)
        if exists(destination):
            raise ValueError(f"archive destination already exists: {destination}")


def move_files(local, archive, selected):
    check_destinations(local, archive, selected)
    for relative in selected:
        source = local / relative
        destination = archive / relative
        destination.parent.mkdir(parents=True, exist_ok=True)
        # Never dereference source links or overwrite an earlier archive.
        subprocess.run(["mv", "--no-clobber", "--no-target-directory", "--",
                        str(source), str(destination)], check=True)
        if exists(source):
            raise ValueError(f"move skipped; source preserved: {source}")
        print(f"Moved {source} -> {destination}")


def main():
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument("--apply", action="store_true", help="perform the listed moves")
    parser.add_argument("--desktop", action="store_true",
                        help="include Ghostty/river runtime files; use only after graphics/login validation")
    parser.add_argument("--package", choices=COMMANDS + DESKTOP_COMMANDS,
                        help="archive only this command's installation (including desktop files)")
    parser.add_argument("--profile", type=Path, default=Path(os.environ.get(
        "NIX_ENV_PROFILE", str(Path.home() / ".local/state/nix/profiles/env"))))
    arguments = parser.parse_args()
    home = Path.home()
    local = home / ".local"
    archive = home / ".local-old"
    selected = candidates(local, desktop=arguments.desktop or arguments.package in DESKTOP_COMMANDS)
    if arguments.package:
        selected = {relative: command for relative, command in selected.items()
                    if command == arguments.package}
    if not selected:
        print("No selected old installations found in ~/.local.")
        return
    check_destinations(local, archive, selected)
    tracked = subprocess.check_output([
        "git", f"--git-dir={home / '.home.git'}", f"--work-tree={home}",
        "ls-files", "--full-name", "-z", "--",
        *(str(Path(".local") / relative) for relative in selected),
    ], cwd=home).split(b"\0")
    if any(tracked):
        raise ValueError("refusing to archive tracked files: " + ", ".join(
            os.fsdecode(filename) for filename in tracked if filename))

    for relative in selected:
        print(f"{local / relative} -> {archive / relative}")
    required = set(selected.values())
    if Path("google-cloud-sdk") in selected:
        required.update(("gcloud", "gsutil", "bq"))
    missing = sorted(command for command in required
                     if not os.access(arguments.profile / "bin" / command, os.X_OK)
                     or not (arguments.profile / "bin" / command).resolve().is_relative_to("/nix/store"))
    profile_on_path = str(arguments.profile / "bin") in os.environ.get("PATH", "").split(os.pathsep)
    if missing or not profile_on_path:
        problem = (f"Nix replacements not installed: {', '.join(missing)}. " if missing else "")
        problem += "Activate the Nix profile and source ~/env/nix/scripts/nix-session.sh first."
        if arguments.apply:
            raise ValueError(problem)
        print(problem, file=sys.stderr)
    if arguments.apply:
        move_files(local, archive, selected)
        print("Archive complete. Run 'hash -r' in your shell to clear cached command paths.")
    else:
        print("Preview only. Run again with --apply to move these files.")
    if Path("google-cloud-sdk") in selected:
        print("Remove old google-cloud-sdk path/completion sourcing from shell startup files.")
    if arguments.desktop:
        print("Shared libraries and headers are retained. Nix river must not inherit the old LD_LIBRARY_PATH.")


if __name__ == "__main__":
    try:
        main()
    except (OSError, ValueError, subprocess.CalledProcessError) as error:
        print(f"archive-nix-replacements: {error}", file=sys.stderr)
        sys.exit(1)
