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

# Takes an input of a number, to add to the current brightness.

DIR=/sys/class/backlight/intel_backlight
LVL=$1

CURR=$(head -n 1 "$DIR/brightness")

$(dirname $0)/brightness-set.sh $(($CURR+$LVL))
