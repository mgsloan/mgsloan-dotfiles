#!/bin/bash -e

latest=$(curl -fsSLI -o /dev/null -w '%{url_effective}' \
         https://github.com/typst/typst/releases/latest)
latest=${latest##*/}                                     # v0.15.0
current=v$(typst --version 2>/dev/null | awk '{print $2}')

if [[ $latest != "$current" ]]; then
  echo "typst $current -> $latest"
  arch=$(uname -m); case $arch in
    x86_64) t=x86_64-unknown-linux-musl ;;
    aarch64|arm64) t=aarch64-unknown-linux-musl ;;
    armv7l) t=armv7-unknown-linux-musleabi ;;
    *) echo "typst: unhandled arch $arch" >&2; t= ;;
  esac
  if [[ -n $t ]]; then
    tmp=$(mktemp -d)
    curl -fsSL "https://github.com/typst/typst/releases/download/$latest/typst-$t.tar.xz" \
      | tar -xJf - -C "$tmp"
    sudo install -m0755 "$(find "$tmp" -name typst -type f)" /usr/local/bin/typst
    rm -rf "$tmp"
  fi
fi
