#!/bin/bash -ex

set -euo pipefail

if [ "$UID" -eq 0 ]; then
  echo "Run setup as yourself; system operations use sudo." >&2
  exit 1
fi

# udev's notification helper uses /usr/bin/notify-send without a user profile.
sudo apt install --yes libnotify-bin

. "$HOME/env/nix/scripts/nix-session.sh"
export USER_NAME="$(id -un)"
"$HOME/env/system/udev-rules/generate.sh"
"$HOME/env/system/udev-rules/apply.sh"
sudo usermod -a -G video "$USER_NAME"
