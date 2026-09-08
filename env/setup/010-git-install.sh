#!/bin/bash -ex

[ "$UID" -eq 0 ] || exec sudo bash -ex "$0" "$@"

# Git is needed to bootstrap the home repo before Nix is available.
apt install git
