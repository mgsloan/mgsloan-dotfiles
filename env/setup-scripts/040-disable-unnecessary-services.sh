#!/bin/bash -ex

[ "$UID" -eq 0 ] || exec sudo bash -ex "$0" "$@"

systemctl disable postfix.service || true
systemctl disable avahi-daemon.service || true
systemctl disable cups-browsed.service || true
