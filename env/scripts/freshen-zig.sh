#!/usr/bin/env bash
# Installs the latest stable Zig for x86_64 Linux into ~/.local/zig,
# and symlinks the binary into ~/.local/bin/zig.
set -euo pipefail

INSTALL_DIR="${USER_HOME}/.local/zig"
BIN_LINK="${USER_HOME}/.local/bin/zig"
INDEX_URL="https://ziglang.org/download/index.json"

mkdir -p "$(dirname "$BIN_LINK")"

# Pull latest stable (first non-master key) tarball URL + version
read -r VERSION TARBALL_URL < <(
  curl -fsSL "$INDEX_URL" \
    | jq -r 'to_entries
             | map(select(.key != "master"))
             | .[0]
             | "\(.key) \(.value["x86_64-linux"].tarball)"'
)

# Skip if already installed
if [[ -x "${INSTALL_DIR}/${VERSION}/zig" ]]; then
  echo "Zig ${VERSION} already installed."
else
  echo "Installing Zig ${VERSION}..."
  TMP="$(mktemp -d)"
  trap 'rm -rf "$TMP"' EXIT
  curl -fSL "$TARBALL_URL" -o "$TMP/zig.tar.xz"
  mkdir -p "${INSTALL_DIR}/${VERSION}"
  tar -xJf "$TMP/zig.tar.xz" -C "${INSTALL_DIR}/${VERSION}" --strip-components=1
fi

ln -sfn "${INSTALL_DIR}/${VERSION}/zig" "$BIN_LINK"
echo "zig -> ${BIN_LINK}"
"$BIN_LINK" version
