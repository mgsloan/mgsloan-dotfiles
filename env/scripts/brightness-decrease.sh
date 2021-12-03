#!/bin/bash

# Copy modified from
# https://gist.github.com/teolandon/f6e9a3d6b584f7287b4f05d2a1d9b968

# Takes an input of a number, to subtract from the current brightness.

DIR=/sys/class/backlight/intel_backlight
LVL=$(expr $(cat "$DIR/max_brightness") / $1)

CURR=$(head -n 1 "$DIR/brightness")

$(dirname $0)/brightness-set.sh $(($CURR-$LVL))
