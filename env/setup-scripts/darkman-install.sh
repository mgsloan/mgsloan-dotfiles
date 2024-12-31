#!/bin/bash -ex

[ "$UID" -eq 0 ] || exec sudo USER_HOME="$HOME" USER_NAME="$LOGNAME" bash -e "$0" "$@"

make install PREFIX=/usr
systemctl --user enable --now darkman.service
