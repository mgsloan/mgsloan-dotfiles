#!/bin/bash

# Copy modified from
# https://gist.github.com/teolandon/f6e9a3d6b584f7287b4f05d2a1d9b968

# My laptop's brightness control doesn't automatically work
# by itself, and xbacklight is looking in the wrong directory
# to change my backlight settings, so I quickly wrote up these
# scripts to bindsym them to my brightness control keys. Simple
# and clean.

# Make sure that whatever you're using to call them has sufficient
# perimissions.

# Takes an input percentage to add to the current brightness.

DIR=/sys/class/backlight/intel_backlight
PCT=$1

CURR=$(head -n 1 "$DIR/brightness")
MAX=$(head -n 1 "$DIR/max_brightness")
CURR_PCT=$(( CURR * 100 / MAX ))

# The mirror of brightness-decrease.sh's tail. Anything below 1% rounds to 0%,
# so without this a press from the dimmest rung jumped straight to 2% -- a
# sixteen-fold step, and no way back up through the rungs it just came down.
# Doubling lands on 1% exactly, where the percentage ladder takes over again.
if [ "$CURR_PCT" -eq 0 ]; then
	exec "$(dirname "$0")/brightness-set.sh" --raw $(( CURR * 2 ))
fi

$(dirname $0)/brightness-set.sh $(( CURR_PCT + PCT ))
