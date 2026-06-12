#!/bin/bash

# Copy modified from
# https://gist.github.com/teolandon/f6e9a3d6b584f7287b4f05d2a1d9b968

DIR=/sys/class/backlight/intel_backlight
PCT=$1

MIN=10
MAX=$(head -n 1 "$DIR/max_brightness")
NEW=$(( PCT * MAX / 100 ))

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
