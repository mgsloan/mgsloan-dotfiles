#!/bin/bash
set -euo pipefail

if [ "$UID" -eq 0 ]; then
  echo "Run setup as yourself; system operations use sudo." >&2
  exit 1
fi

temporary_directory=$(mktemp -d)
trap 'rm -rf "$temporary_directory"' EXIT

declare -a package_files=()

download_if_missing() {
  local package_name="$1" url="$2"
  if dpkg-query -W -f='${db:Status-Abbrev}' "$package_name" 2>/dev/null | grep -q '^ii'; then
    return
  fi

  local package_file="$temporary_directory/$package_name.deb"
  curl -fL "$url" -o "$package_file"
  package_files+=("$package_file")
}

download_if_missing google-chrome-stable \
  https://dl.google.com/linux/direct/google-chrome-stable_current_amd64.deb
download_if_missing keybase https://prerelease.keybase.io/keybase_amd64.deb
download_if_missing chatgpt \
  https://persistent.oaistatic.com/codex-app-prod/linux/deb/latest/chatgpt_amd64.deb
download_if_missing zoom https://zoom.us/client/latest/zoom_amd64.deb

if ! dpkg-query -W -f='${db:Status-Abbrev}' tailscale 2>/dev/null | grep -q '^ii'; then
  # shellcheck disable=SC1091
  . /etc/os-release
  case ${ID:-} in
    debian|ubuntu) distribution=$ID ;;
    *) echo "Unsupported distribution for Tailscale: ${ID:-unknown}" >&2; exit 1 ;;
  esac
  codename=${VERSION_CODENAME:?VERSION_CODENAME is missing from /etc/os-release}
  tailscale_url="https://pkgs.tailscale.com/stable/$distribution/$codename"

  curl -fsSL "$tailscale_url.noarmor.gpg" \
    -o "$temporary_directory/tailscale-archive-keyring.gpg"
  curl -fsSL "$tailscale_url.tailscale-keyring.list" \
    -o "$temporary_directory/tailscale.list"
  sudo install -D -m 644 "$temporary_directory/tailscale-archive-keyring.gpg" \
    /usr/share/keyrings/tailscale-archive-keyring.gpg
  sudo install -D -m 644 "$temporary_directory/tailscale.list" \
    /etc/apt/sources.list.d/tailscale.list
fi

sudo apt-get update
if (( ${#package_files[@]} )); then
  sudo apt-get install --yes "${package_files[@]}"
fi
sudo apt-get install --yes tailscale

if ! sudo tailscale status >/dev/null 2>&1; then
  echo
  echo "Tailscale is installed but not connected. Run: sudo tailscale up"
fi
