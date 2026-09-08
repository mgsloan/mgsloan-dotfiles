#!/bin/bash -ex

[ "$UID" -eq 0 ] || exec sudo USER_HOME="$HOME" USER_NAME="$LOGNAME" bash -e "$0" "$@"

parent_path=$( cd "$(dirname "${BASH_SOURCE[0]}")" ; pwd -P )

cd "$parent_path"

cp --force *.rules --target-directory=/etc/udev/rules.d/
install -o root -g root -m 0755 batify-notify /usr/local/bin/batify-notify
udevadm control --reload-rules
