#!/bin/bash -ex

[ "$UID" -eq 0 ] || exec sudo bash -e "$0" "$@"

parent_path=$( cd "$(dirname "${BASH_SOURCE[0]}")" ; pwd -P )

cd "$parent_path"

mkdir -p /etc/polkit-1/rules.d
cp --force *.rules --target-directory=/etc/polkit-1/rules.d/
systemctl restart polkit
