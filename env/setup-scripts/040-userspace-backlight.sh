#!/bin/bash -ex

[ "$UID" -eq 0 ] || exec sudo USER_HOME="$HOME" USER_NAME="$LOGNAME" bash -e "$0" "$@"

cp "$HOME/env/udev-rules/90-backlight.rules" /etc/udev/rules.d/
usermod -a -G video "$USER_NAME"
