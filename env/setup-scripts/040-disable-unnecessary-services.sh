#!/bin/bash -ex

[ "$UID" -eq 0 ] || exec sudo bash -ex "$0" "$@"

systemctl disable postfix.service
systemctl disable avahi-daemon.service
systemctl disable cups-browsed.service
