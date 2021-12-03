#!/bin/bash -ex

[ "$UID" -eq 0 ] || exec sudo bash -ex "$0" "$@"

apt install git libsecret-1-0 libsecret-1-dev build-essential
cd /usr/share/doc/git/contrib/credential/libsecret
make
