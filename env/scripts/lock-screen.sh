#!/bin/sh
#
# Lock the screen, behind a random picture from the background collection.
#
# Turning the backlight off is swayidle's job, a few seconds after the seat goes
# quiet: see start_idle_daemon in the penrose config. Doing it from here would
# mean darkening a screen with nothing armed to undo it, and every key press
# after that pushes the arming further away -- so typing at it, the one thing
# anybody would try, is what keeps it dark.
#
# Called by `M-s` in the window manager configs and by swayidle's before-sleep
# hook, which is what makes a lid close and a keypress put up the same thing.
#
# It has to return once the screen is locked rather than once it is unlocked:
# swayidle holds a logind sleep inhibitor until this exits, so a locker that
# stayed in the foreground would keep the machine awake. Hence `-f`, which forks
# as soon as the lock surface is up.

set -u

BACKGROUNDS="$HOME/env/untracked/backgrounds"

if [ -n "${WAYLAND_DISPLAY:-}" ]; then
    if ! command -v swaylock > /dev/null; then
        notify-send "No screen lock: install swaylock" 2> /dev/null
        echo "lock-screen: swaylock is not installed" >&2
        exit 1
    fi

    # Empty if the directory is missing or holds nothing: swaylock without an
    # image is a plain grey lock, which is still a lock.
    image=$(find "$BACKGROUNDS" -type f -name '*.jpg' 2> /dev/null | shuf -n 1)

    if [ -n "$image" ]; then
        swaylock -f --image "$image" --scaling fill
    else
        swaylock -f
    fi
    locked=$?

    exit "$locked"
fi

if ! command -v slock > /dev/null; then
    notify-send "No screen lock: install slock" 2> /dev/null
    echo "lock-screen: slock is not installed" >&2
    exit 1
fi

# slock draws a solid colour and takes no image, and has no forking mode: under
# X11 nothing waits on this.
exec slock
