#!/bin/bash
# Launches a river session with xmonad-river as the window manager.
#
# This is the Exec target of the GDM session entry installed by
# setup-scripts/043-create-river-wayland-session.sh.
#
# Deliberately not `set -e`: a failure in the environment plumbing below should
# not stop the session from starting, since a session that refuses to launch
# leaves nothing to debug it with.

set -u

# GDM runs X11 sessions through /etc/gdm3/Xsession, which sources ~/.profile
# (line 38 there). Wayland sessions get no such wrapper -- GDM execs the
# desktop entry directly -- so without this the whole session inherits only
# systemd's default PATH of /usr/local/sbin:/usr/local/bin:/usr/sbin:/usr/bin.
#
# That misses ~/.local/bin, ~/.cargo/bin and ~/env/bin, which is where dunst,
# xidlehook, keynav, spotify and river itself live. The symptom is a session
# that starts but is quietly missing half of what the startup hook launches.
#
# Sourced with "." rather than "source" for the same reason ~/.profile itself
# notes: this needs to keep working under a /bin/sh that is dash.
#
# `set -u` has to come off for this. ~/.profile is written against the looser
# convention gdm's Xsession runs it under -- plain /bin/sh with no -u -- and it
# reads several variables that are simply absent here: HIDPI, DISPLAY (Xwayland
# has not started yet) and XDG_DATA_DIRS. Under -u the first of those aborts
# the script at once, before the logging block below is ever reached, so the
# session dies with GDM bouncing straight back to the login screen and not one
# line written to explain why.
if [ -f "$HOME/.profile" ]; then
  set +u
  . "$HOME/.profile"
  set -u
fi

PREFIX="$HOME/.local"

# wlroots is installed under ~/.local, which is not on the loader's default
# path.
export LD_LIBRARY_PATH="$PREFIX/lib${LD_LIBRARY_PATH:+:$LD_LIBRARY_PATH}"

# Identifies the session to xdg-desktop-portal, which is what screen sharing,
# file pickers and the like consult. "river" has no portal of its own; wlr is
# the backend that implements screencopy for wlroots compositors.
export XDG_CURRENT_DESKTOP=river
export XDG_SESSION_TYPE=wayland

# Ask toolkits to use Wayland natively rather than falling back to Xwayland.
# Chrome needs the ozone flags passed on its command line instead, so
# Constants.hs is where that has to change.
export MOZ_ENABLE_WAYLAND=1
export QT_QPA_PLATFORM="wayland;xcb"
export SDL_VIDEODRIVER=wayland
export _JAVA_AWT_WM_NONREPARENTING=1

# Electron apps (Spotify, Obsidian) read this; without it they use Xwayland.
export ELECTRON_OZONE_PLATFORM_HINT=auto

LOG="$HOME/.local/state/river-session.log"
mkdir -p "$(dirname "$LOG")"

{
  echo "=== river session starting at $(date --iso-8601=seconds) ==="

  # Under X11 this config called gnomeRegister to register with the session
  # manager. The Wayland equivalent is to hand the session's environment to
  # systemd and dbus, so that user services started later -- and anything
  # launched through a portal -- can find the compositor.
  #
  # This has to happen after river sets WAYLAND_DISPLAY, which it does not do
  # until it starts, so the real import is done from the river init script.
  systemctl --user import-environment \
    XDG_CURRENT_DESKTOP XDG_SESSION_TYPE PATH \
    2>&1 || echo "warning: systemctl import-environment failed"

  exec "$PREFIX/bin/river" -log-level info
} >>"$LOG" 2>&1
