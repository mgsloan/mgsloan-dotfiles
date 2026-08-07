#!/bin/bash -e

# Builds river and its wlroots dependency from source into ~/.local.
#
# Neither is packaged by Debian at the versions needed. river 0.5 is
# unreleased -- it is the rewrite that moves all window management policy out
# of the compositor into a separate process, which is what makes
# xmonad-river possible -- and it needs wlroots 0.20, which Debian does not
# carry either. So both are built from git checkouts under ~/oss.
#
# Note that the river Debian *may* eventually ship is river-classic, the old
# dynamic-tiling compositor. That one cannot run xmonad-river; it has no
# river-window-management-v1.

RIVER_DIR="$HOME/oss/river"
WLROOTS_DIR="$HOME/oss/wlroots"

# Matches the zig-wlroots binding version river pins in build.zig.zon. Any
# 0.20.x is ABI compatible, but staying on the exact version keeps one
# variable out of the picture when something breaks.
WLROOTS_TAG="0.20.1"

PREFIX="$HOME/.local"

# river requires exactly this zig; 0.15 will not build it.
#
# freshen-zig.sh reads USER_HOME rather than HOME, since the scripts that
# normally call it have re-executed themselves under sudo. Nothing here needs
# root, so pass the invoking user's home directly.
USER_HOME="$HOME" "$HOME/env/scripts/freshen-zig.sh" 0.16.0

#-------------------------------------------------------------------------------
# wlroots

if [ ! -d "$WLROOTS_DIR" ]; then
  git clone https://gitlab.freedesktop.org/wlroots/wlroots.git "$WLROOTS_DIR"
fi
cd "$WLROOTS_DIR"
git fetch --tags
git checkout "$WLROOTS_TAG"

# --libdir=lib rather than Debian's multiarch default, so that the pkg-config
# and library paths below are predictable.
rm -rf build
meson setup build \
  --prefix="$PREFIX" \
  --libdir=lib \
  --buildtype=release \
  -Dexamples=false
ninja -C build
ninja -C build install

#-------------------------------------------------------------------------------
# river

export PKG_CONFIG_PATH="$PREFIX/lib/pkgconfig:${PKG_CONFIG_PATH:-}"

if [ ! -d "$RIVER_DIR" ]; then
  git clone https://codeberg.org/river/river.git "$RIVER_DIR"
fi
cd "$RIVER_DIR"

# -Dxwayland matters here: Chrome, Spotify and Obsidian are Electron and
# default to Xwayland, and Emacs needs a pure-GTK build to avoid it.
zig build \
  -Doptimize=ReleaseSafe \
  -Dxwayland \
  --prefix "$PREFIX" \
  install

echo
echo "Installed:"
LD_LIBRARY_PATH="$PREFIX/lib" "$PREFIX/bin/river" -version
echo
echo "wlroots is in $PREFIX/lib, which is not on the default loader path."
echo "scripts/river-session.sh sets LD_LIBRARY_PATH accordingly; running"
echo "river by hand needs the same."
