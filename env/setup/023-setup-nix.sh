#!/bin/bash
set -euo pipefail

if [ "$UID" -eq 0 ]; then
  echo "Run setup as yourself; system operations use sudo." >&2
  exit 1
fi

username=$(id -un)

# Debian's package configures the multi-user daemon and store. Keeping Nix in
# apt also gives the installer the same trust and update path as the base OS.
if ! command -v nix >/dev/null; then
  sudo apt install --yes nix-bin
fi

if getent group nix-users >/dev/null; then
  if ! id -nG "$username" | tr ' ' '\n' | grep -qx nix-users; then
    sudo usermod --append --groups nix-users "$username"
  fi
fi

sudo systemctl enable --now nix-daemon.socket

configuration=/etc/nix/nix.conf

# Keep the distro's settings and add one independently managed fragment.
sudo mkdir -p /etc/nix/nix.conf.d
if ! sudo grep -qF '!include nix.conf.d/env.conf' "$configuration" 2>/dev/null; then
  echo '!include nix.conf.d/env.conf' | sudo tee -a "$configuration" >/dev/null
fi

printf '%s\n' \
  'experimental-features = nix-command flakes' \
  'auto-optimise-store = true' \
  | sudo tee /etc/nix/nix.conf.d/env.conf >/dev/null

sudo systemctl restart nix-daemon.service

if getent group nix-users >/dev/null && ! id -nG | tr ' ' '\n' | grep -qx nix-users; then
  echo 'Nix is configured. Log in again to acquire nix-users membership, then rerun this script.'
  exit 1
fi

"$HOME/env/bin/env-nix" activate "$@"

# The home path is expanded by the future login shell.
# shellcheck disable=SC2016
session_line='. "$HOME/env/nix/scripts/nix-session.sh"'
if ! grep -qxF "$session_line" "$HOME/.profile"; then
  printf '\n%s\n' "$session_line" >> "$HOME/.profile"
fi

echo
nix --version
echo 'Nix profile enabled for subsequent login sessions.'
