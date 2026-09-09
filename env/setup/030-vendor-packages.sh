#!/bin/bash
set -euo pipefail

if [ "$UID" -eq 0 ]; then
  echo "Run setup as yourself; system operations use sudo." >&2
  exit 1
fi

if dpkg-query -W -f='${db:Status-Abbrev}' google-chrome-stable 2>/dev/null | grep -q '^ii'; then
  exit
fi

temporary_directory=$(mktemp -d)
trap 'rm -rf "$temporary_directory"' EXIT

curl -fL https://dl.google.com/linux/direct/google-chrome-stable_current_amd64.deb \
  -o "$temporary_directory/google-chrome-stable.deb"
sudo apt-get update
sudo apt-get install --yes "$temporary_directory/google-chrome-stable.deb"
