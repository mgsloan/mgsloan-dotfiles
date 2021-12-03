#!/bin/bash -ex

[ "$UID" -eq 0 ] || exec sudo USER_HOME="$HOME" bash -ex "$0" "$@"

cp -f "$USER_HOME/env/grub/grub" /etc/default/grub
