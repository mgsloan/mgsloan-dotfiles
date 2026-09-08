#!/bin/sh
#
# Turn the backlight off, and put it back where it was.
#
# Usage: screen-backlight.sh off|on
#
# The panel rather than the output: powering an output down goes through the
# compositor, and river cannot currently survive that happening while the
# session is locked -- see vendor/penrose/todo.md. Nothing here reaches the
# compositor at all. A locked session goes on rendering and taking input with
# the backlight at zero, so the worst a missed restore can do is leave the screen
# dark on a machine that still works: the password still unlocks it, and the
# brightness keys still turn it up.
#
# The saved level lives in the runtime dir, so it is gone by the next login.

set -u

DEV=/sys/class/backlight/intel_backlight
SAVED="${XDG_RUNTIME_DIR:-/tmp}/penrose-backlight"

if [ ! -w "$DEV/brightness" ]; then
    echo "screen-backlight: $DEV/brightness is not writable" >&2
    exit 1
fi

case "${1:-}" in
    off)
        # Saved once, not on every off: a second one with no on between would
        # otherwise remember the zero the first one wrote.
        [ -e "$SAVED" ] || cat "$DEV/brightness" > "$SAVED"
        echo 0 > "$DEV/brightness"
        ;;
    on)
        # Nothing saved means nothing here turned it off, and whatever it is set
        # to now is what somebody wanted.
        [ -e "$SAVED" ] || exit 0
        cat "$SAVED" > "$DEV/brightness"
        rm -f "$SAVED"
        ;;
    *)
        echo "usage: screen-backlight.sh off|on" >&2
        exit 2
        ;;
esac
