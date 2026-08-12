#!/bin/bash
# Launches a river session, with whichever window manager the init script names.
#
# Usage: river-session.sh [init-name]
#
# The init script is river's only entry point: river runs it and waits, and the
# window manager it launches is what actually manages windows. So which window
# manager a session gets is which init it is given -- `init` for xmonad-river,
# `init-penrose` for penrose. Both are installed into ~/.config/river by their
# setup scripts.
#
# This is the Exec target of the GDM session entries installed by
# setup-scripts/043-create-river-wayland-session.sh and
# setup-scripts/046-create-penrose-river-session.sh.
#
# Deliberately not `set -e`: a failure in the environment plumbing below should
# not stop the session from starting, since a session that refuses to launch
# leaves nothing to debug it with.

set -u

# Which init river runs is which window manager the session gets. With no
# argument river finds its own -- $XDG_CONFIG_HOME/river/init, which 043 links to
# env/river/init -- and is started exactly as it always was: the session that
# already works is not worth changing to make room for a second one.
#
# Named inits go through river's -c, which is documented as running
# `sh -c <command>` rather than taking a path, hence the exec and the quoting.
INIT_NAME=${1:-}
CONFIG_DIR="${XDG_CONFIG_HOME:-$HOME/.config}/river"

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
# Chrome needs nothing here and nothing on its command line: its ozone platform
# hint defaults to auto, so it picks Wayland when WAYLAND_DISPLAY is set and X11
# when it is not -- which is what a config spawning `google-chrome` for both
# backends wants. Passing --ozone-platform would break the X11 session.
export MOZ_ENABLE_WAYLAND=1
export QT_QPA_PLATFORM="wayland;xcb"
export SDL_VIDEODRIVER=wayland
export _JAVA_AWT_WM_NONREPARENTING=1

# Electron apps (Spotify, Obsidian) read this; without it they use Xwayland.
export ELECTRON_OZONE_PLATFORM_HINT=auto

LOG="$HOME/.local/state/river-session${INIT_NAME:+-$INIT_NAME}.log"
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

  if [ -z "$INIT_NAME" ]; then
    exec "$PREFIX/bin/river" -log-level info
  fi

  INIT="$CONFIG_DIR/$INIT_NAME"

  # river fatals on an init that is not executable, which from the login screen
  # looks like the session bouncing straight back with nothing said.
  if [ ! -x "$INIT" ]; then
    echo "river-session: $INIT is missing or not executable."
    echo "river-session: run the setup script for this session."
    exit 1
  fi

  echo "starting river with init $INIT"
  exec "$PREFIX/bin/river" -log-level info -c "exec '$INIT'"
} >>"$LOG" 2>&1
