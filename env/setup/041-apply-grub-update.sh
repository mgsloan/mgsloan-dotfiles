#!/bin/bash -ex

[ "$UID" -eq 0 ] || exec sudo USER_HOME="$HOME" bash -ex "$0" "$@"

cp -f "$USER_HOME/env/system/grub/grub" /etc/default/grub
