#!/usr/bin/env python3
"""Link Nix notification/theme services without replacing existing user files."""

import argparse
import os
from pathlib import Path
import subprocess
import sys


SERVICE_FILES = {
    ".config/systemd/user/darkman.service": "share/systemd/user/darkman.service",
    ".config/systemd/user/dunst.service": "lib/systemd/user/dunst.service",
    ".local/share/dbus-1/services/nl.whynothugo.darkman.service":
        "share/dbus-1/services/nl.whynothugo.darkman.service",
    ".local/share/dbus-1/services/org.freedesktop.impl.portal.desktop.darkman.service":
        "share/dbus-1/services/org.freedesktop.impl.portal.desktop.darkman.service",
    ".local/share/dbus-1/services/org.freedesktop.Notifications.service":
        "share/dbus-1/services/org.knopwob.dunst.service",
}


def link_plan(home, profile):
    return {home / destination: profile / source for destination, source in SERVICE_FILES.items()}


def check_links(links, store=Path("/nix/store")):
    for destination, source in links.items():
        if not source.is_file() or not source.resolve().is_relative_to(store):
            raise ValueError(f"activate the Nix desktop packages first: {source}")
        if destination.is_symlink() and os.readlink(destination) == str(source):
            continue
        if destination.exists() or destination.is_symlink():
            raise ValueError(f"preserve the existing user file before linking: {destination}")


def reload_services():
    subprocess.run(["systemctl", "--user", "daemon-reload"], check=True)
    subprocess.run(["busctl", "--user", "call", "org.freedesktop.DBus",
                    "/org/freedesktop/DBus", "org.freedesktop.DBus", "ReloadConfig"], check=True)


def main():
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument("--profile", type=Path, default=Path(os.environ.get(
        "NIX_ENV_PROFILE", str(Path.home() / ".local/state/nix/profiles/env"))))
    arguments = parser.parse_args()
    home = Path.home()
    links = link_plan(home, arguments.profile.absolute())
    check_links(links)
    created = []
    try:
        for destination, source in links.items():
            if destination.is_symlink():
                continue
            destination.parent.mkdir(parents=True, exist_ok=True)
            destination.symlink_to(source)
            created.append(destination)
        reload_services()
        # Do not start a second daemon when the session spawned dunst directly.
        for service in ("darkman", "dunst"):
            process = subprocess.check_output([
                "systemctl", "--user", "show", f"{service}.service", "--property=MainPID", "--value",
            ], text=True).strip()
            # dunst's launcher execs .dunst-wrapped in the same package output.
            package = (arguments.profile / "bin" / service).resolve().parent.parent
            if process != "0" and not Path(f"/proc/{process}/exe").resolve().is_relative_to(package):
                subprocess.run(["systemctl", "--user", "try-restart", f"{service}.service"], check=True)
    except (OSError, subprocess.CalledProcessError):
        for destination in reversed(created):
            if destination.is_symlink() and os.readlink(destination) == str(links[destination]):
                destination.unlink()
        reload_services()
        subprocess.run(["systemctl", "--user", "try-restart", "darkman.service"], check=False)
        raise
    for destination in created:
        print(f"Linked {destination} -> {links[destination]}")
    print("User service/D-Bus links updated; running services switched if their binary changed.")
    print("On first setup, enable darkman with: systemctl --user enable --now darkman.service")
    print("An unmanaged dunst process must be stopped before starting dunst.service.")


if __name__ == "__main__":
    try:
        main()
    except (OSError, ValueError, subprocess.CalledProcessError) as error:
        print(f"nix-services: {error}", file=sys.stderr)
        sys.exit(1)
