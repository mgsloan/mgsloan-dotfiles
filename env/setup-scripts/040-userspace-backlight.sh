#!/bin/bash -ex

[ "$UID" -eq 0 ] || exec sudo bash -e "$0" "$@"

cp ~/env/udev-rules/90-backlight.rules /etc/udev/rules.d/
usermod -a -G video "$LOGNAME"
