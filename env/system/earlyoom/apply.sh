#!/bin/bash -ex

[ "$UID" -eq 0 ] || exec sudo USER_HOME="$HOME" bash -e "$0" "$@"

parent_path=$( cd "$(dirname "${BASH_SOURCE[0]}")" ; pwd -P )

cd "$parent_path"

cp --force default /etc/default/earlyoom
systemctl enable earlyoom
systemctl restart earlyoom

systemctl status earlyoom --no-pager -l | head -5
