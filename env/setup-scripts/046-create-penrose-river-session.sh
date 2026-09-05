#!/bin/bash -ev

# Installs the GDM session entry for penrose running on river.
#
# The fourth session: xmonad on X11 (040), river with xmonad-river (043),
# penrose on X11 (044), and this one. It is the river session entry with a
# different init script, since which window manager river gets is decided by
# which init it is told to run.
#
# The window manager itself is built by 045-build-penrose.sh, which builds both
# backends from the one config.

[ "$UID" -eq 0 ] || exec sudo USER_HOME="$HOME" bash -ex "$0" "$@"

# Re-execing above is the intended route and passes USER_HOME through; running
# this as `sudo 046-...` instead skips that line with USER_HOME unset.
if [ -z "${USER_HOME:-}" ]; then
  USER_HOME="$(getent passwd "${SUDO_USER:-$USER}" | cut -d: -f6)"
  export USER_HOME
fi

TEMPLATE="$USER_HOME/env/wayland-sessions/penrose-river.desktop.template"
DESTINATION="/usr/share/wayland-sessions/penrose-river.desktop"

mkdir -p "$(dirname "$DESTINATION")"
envsubst < "$TEMPLATE" > "$DESTINATION"

echo ""
echo "Contents of $DESTINATION is now:"
echo ""
cat "$DESTINATION"

OWNER="$(stat -c %U "$USER_HOME")"

# A *login* shell, because the build below needs the user's PATH: cargo lives in
# ~/.cargo/bin and sudo replaces PATH with its own secure_path, so a plain
# `sudo -u ... bash` gets as far as "cargo: not found". ~/.profile is what puts
# it back, and it is the same file GDM's Xsession sources.
sudo -u "$OWNER" bash -le <<INNER
# The target the init script starts in place of graphical-session.target, shared
# with the xmonad-river session and linked by whichever setup script runs first.
mkdir -p "$USER_HOME/.config/systemd/user"
ln -sfn "$USER_HOME/env/systemd/river-session.target" \
  "$USER_HOME/.config/systemd/user/river-session.target"
echo "Linked river-session.target"

# systemctl --user needs to find the user's own systemd, which it locates through
# XDG_RUNTIME_DIR -- unset under sudo, hence the explicit one. Still best effort:
# with no session running there is no user systemd to reload, and the unit is
# read at the next login regardless.
XDG_RUNTIME_DIR="/run/user/\$(id -u)" systemctl --user daemon-reload || true

# The init script execs this directly, and a missing binary shows up only as a
# blank screen, so build it now rather than at first boot. Both backends, since
# nothing is running to restart into.
"$USER_HOME/env/penrose/scripts/rebuild-penrose.sh"
INNER
