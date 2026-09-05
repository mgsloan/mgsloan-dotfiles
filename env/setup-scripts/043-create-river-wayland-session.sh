#!/bin/bash -ev

# Installs the GDM session entry for river.
#
# The Wayland counterpart of 040-create-xmonad-xsession.sh. Note the different
# directory: display managers look for Wayland sessions in wayland-sessions,
# not xsessions, and a session placed in the wrong one will either not appear
# or appear and fail.

[ "$UID" -eq 0 ] || exec sudo USER_HOME="$HOME" bash -ex "$0" "$@"

TEMPLATE="$USER_HOME/env/wayland-sessions/river.desktop.template"
DESTINATION="/usr/share/wayland-sessions/river.desktop"

mkdir -p "$(dirname "$DESTINATION")"
envsubst < "$TEMPLATE" > "$DESTINATION"

echo ""
echo "Contents of $DESTINATION is now:"
echo ""
cat "$DESTINATION"

sudo -u "$(stat -c %U "$USER_HOME")" bash -e <<INNER
# The target the init script starts in place of graphical-session.target,
# which refuses to be started by hand.
mkdir -p "$USER_HOME/.config/systemd/user"
ln -sfn "$USER_HOME/env/systemd/river-session.target" \
  "$USER_HOME/.config/systemd/user/river-session.target"
systemctl --user daemon-reload || true
echo "Linked river-session.target"

# river execs this directly, and a missing binary shows up only as a blank
# screen, so build and install it now rather than at first boot.
"$USER_HOME/env/scripts/rebuild-river.sh"
INNER
