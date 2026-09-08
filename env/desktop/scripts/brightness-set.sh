#!/bin/bash

# Copy modified from
# https://gist.github.com/teolandon/f6e9a3d6b584f7287b4f05d2a1d9b968

DIR=/sys/class/backlight/intel_backlight

MAX=$(head -n 1 "$DIR/max_brightness")

# The dimmest the keys will go. Raw units, not a percentage: this panel reports
# max_brightness 192000, so 1% is 1920 and there is no integer percentage
# between that and nothing at all. It used to be 10, which is 0.005% and is
# indistinguishable from the backlight being off -- so the bottom of the ladder
# was a cliff rather than a step.
#
# brightness-decrease.sh halves in raw units once it runs out of percentages,
# which with this floor gives three rungs below 1%: 960, 480, 240. Raise this
# number to make the dimmest setting brighter; the rungs follow from it.
#
# Not the same thing as the backlight being off: screen-backlight.sh writes 0
# to the device directly when the session locks, and is meant to.
MIN=240

# `--raw N` sets the device value; a bare number is a percentage of max. The
# raw form exists for the sub-1% tail, which a percentage cannot express.
if [ "$1" = "--raw" ]; then
	NEW=$2
	PCT=$(( NEW * 100 / MAX ))
else
	PCT=$1
	NEW=$(( PCT * MAX / 100 ))
fi

if [ "$NEW" -gt "$MAX" ]; then
	tee "$DIR/brightness" <<< $MAX > /dev/null
elif [ "$NEW" -lt "$MIN" ]; then
	tee "$DIR/brightness" <<< $MIN > /dev/null
else
	tee "$DIR/brightness" <<< $NEW > /dev/null
fi

# Also sync the Apple Studio Display (if connected) to the same percentage.
# Clamp to 0-100 for asdcontrol's percentage syntax. Never let a missing
# display or insufficient permissions break the laptop backlight control above.
ASD_PCT=$PCT
[ "$ASD_PCT" -gt 100 ] 2>/dev/null && ASD_PCT=100
[ "$ASD_PCT" -lt 0 ] 2>/dev/null && ASD_PCT=0
"$(dirname "$0")/asd-brightness.sh" "${ASD_PCT}%" > /dev/null 2>&1 || true
