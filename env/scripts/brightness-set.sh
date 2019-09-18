#!/bin/bash

# Copy modified from
# https://gist.github.com/teolandon/f6e9a3d6b584f7287b4f05d2a1d9b968

DIR=/sys/class/backlight/intel_backlight
NEW=$1

MIN=10
MAX=$(head -n 1 "$DIR/max_brightness")

if [ "$NEW" -gt "$MAX" ]; then
	tee "$DIR/brightness" <<< $MAX > /dev/null
elif [ "$NEW" -lt "$MIN" ]; then
	tee "$DIR/brightness" <<< $MIN > /dev/null
else
	tee "$DIR/brightness" <<< $NEW > /dev/null
fi
