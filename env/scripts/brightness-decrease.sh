#!/bin/bash

# Copy modified from
# https://gist.github.com/teolandon/f6e9a3d6b584f7287b4f05d2a1d9b968

# Takes an input percentage to subtract from the current brightness.

DIR=/sys/class/backlight/intel_backlight
PCT=$1

CURR=$(head -n 1 "$DIR/brightness")
MAX=$(head -n 1 "$DIR/max_brightness")
CURR_PCT=$(( CURR * 100 / MAX ))
NEW_PCT=$(( CURR_PCT - PCT ))

if [ "$NEW_PCT" -le 0 ]; then
	NEW_PCT=$(( CURR_PCT / 2 ))
fi

$(dirname $0)/brightness-set.sh $NEW_PCT
