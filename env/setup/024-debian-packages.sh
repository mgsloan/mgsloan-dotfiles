#!/bin/bash
set -euo pipefail

if [ "$UID" -eq 0 ]; then
  echo "Run setup as yourself; system operations use sudo." >&2
  exit 1
fi

temporary_directory=$(mktemp -d)
trap 'rm -rf "$temporary_directory"' EXIT

install_source() {
  local name=$1 key_url=$2 repository=$3
  local keyring="/usr/share/keyrings/env-$name.gpg"
  local source="/etc/apt/sources.list.d/env-$name.list"
  if [ ! -f "$keyring" ]; then
    curl -fsSL "$key_url" -o "$temporary_directory/$name.key"
    gpg --batch --dearmor --output "$temporary_directory/$name.gpg" "$temporary_directory/$name.key"
    sudo install -Dm644 "$temporary_directory/$name.gpg" "$keyring"
  fi
  printf 'deb [arch=%s signed-by=%s] %s\n' "$(dpkg --print-architecture)" "$keyring" "$repository" \
    > "$temporary_directory/$name.list"
  if ! cmp -s "$temporary_directory/$name.list" "$source"; then
    sudo install -Dm644 "$temporary_directory/$name.list" "$source"
  fi
}

install_source github-cli https://cli.github.com/packages/githubcli-archive-keyring.gpg \
  'https://cli.github.com/packages stable main'
install_source google-cloud https://packages.cloud.google.com/apt/doc/apt-key.gpg \
  'https://packages.cloud.google.com/apt cloud-sdk main'

cat > "$temporary_directory/debian-source.sources" <<'SOURCES'
Types: deb-src
URIs: https://deb.debian.org/debian
Suites: forky
Components: main
Signed-By: /usr/share/keyrings/debian-archive-keyring.gpg
SOURCES
if ! cmp -s "$temporary_directory/debian-source.sources" /etc/apt/sources.list.d/env-debian-source.sources; then
  sudo install -Dm644 "$temporary_directory/debian-source.sources" /etc/apt/sources.list.d/env-debian-source.sources
fi
