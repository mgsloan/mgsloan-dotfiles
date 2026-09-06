#!/bin/bash -ex

set -euo pipefail

if [ "$UID" -eq 0 ]; then
  echo "Run setup as yourself; system operations use sudo." >&2
  exit 1
fi

. "$HOME/env/scripts/nix-session.sh"
export USER_NAME="$(id -un)"
"$HOME/env/udev-rules/generate.sh"
"$HOME/env/udev-rules/apply.sh"
sudo usermod -a -G video "$USER_NAME"
