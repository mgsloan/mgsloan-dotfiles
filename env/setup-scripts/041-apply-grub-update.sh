#!/bin/bash -ex

[ "$UID" -eq 0 ] || exec sudo bash -ex "$0" "$@"

cp -f ~/env/grub/grub /etc/default/grub
