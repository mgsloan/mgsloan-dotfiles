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

# Below 1% there are no percentages left to step through: 1% of this panel is
# 1920 raw and the integer below it is 0. So the tail of the ladder halves the
# device value instead, giving 960, 480 and then the floor brightness-set.sh
# clamps at. That used to be `CURR_PCT / 2`, which floored to 0% and landed on
# a backlight that was for practical purposes off, one keypress below 1%.
if [ "$NEW_PCT" -le 0 ]; then
	exec "$(dirname "$0")/brightness-set.sh" --raw $(( CURR / 2 ))
fi

$(dirname $0)/brightness-set.sh $NEW_PCT
