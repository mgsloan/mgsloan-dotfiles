#!/usr/bin/env bash
# Installs Zig for x86_64 Linux into ~/.local/zig,
# and symlinks the binary into ~/.local/bin/zig.
# Usage: freshen-zig.sh [version]
#   If version is omitted, installs the latest stable.
set -euo pipefail

INSTALL_DIR="${USER_HOME}/.local/zig"
BIN_LINK="${USER_HOME}/.local/bin/zig"
INDEX_URL="https://ziglang.org/download/index.json"

REQUESTED_VERSION="${1:-}"

mkdir -p "$(dirname "$BIN_LINK")"

INDEX_JSON="$(curl -fsSL "$INDEX_URL")"

# Pull latest stable (first non-master key) version + tarball URL
read -r LATEST_VERSION LATEST_TARBALL_URL < <(
  jq -r 'to_entries
         | map(select(.key != "master"))
         | .[0]
         | "\(.key) \(.value["x86_64-linux"].tarball)"' <<<"$INDEX_JSON"
)

if [[ -z "$REQUESTED_VERSION" ]]; then
  VERSION="$LATEST_VERSION"
  TARBALL_URL="$LATEST_TARBALL_URL"
else
  VERSION="$REQUESTED_VERSION"
  if [[ "$VERSION" != "$LATEST_VERSION" ]]; then
    older="$(printf '%s\n%s\n' "$VERSION" "$LATEST_VERSION" | sort -V | head -n1)"
    if [[ "$older" == "$VERSION" ]]; then
      echo "Warning: requested Zig ${VERSION} is older than latest stable ${LATEST_VERSION}." >&2
    fi
  fi
  TARBALL_URL="$(jq -r --arg v "$VERSION" '.[$v]["x86_64-linux"].tarball // empty' <<<"$INDEX_JSON")"
  if [[ -z "$TARBALL_URL" ]]; then
    TARBALL_URL="${LATEST_TARBALL_URL//$LATEST_VERSION/$VERSION}"
  fi
fi

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
