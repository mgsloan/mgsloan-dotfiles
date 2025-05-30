#!/bin/bash -ex

[ "$UID" -eq 0 ] || exec sudo USER_HOME="$HOME" USER_NAME="$LOGNAME" bash -e "$0" "$@"

gem install mustache

../udev-rules/generate.sh
../udev-rules/apply.sh
usermod -a -G video "$USER_NAME"
