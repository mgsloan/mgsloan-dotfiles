#!/bin/bash -ev

# penrose is a fork rather than the upstream crate: this config drives both of
# its backends and the river one does not exist upstream. It is a submodule, so
# the commit is pinned by the home repo.
#
# Builds both window managers -- penrose-wm for X11 and penrose-river-wm for
# river -- since one config produces both and the sessions that use them are
# installed by 044 and 046.

cd "$HOME"

cfg submodule update --init env/penrose/vendor/penrose

cd "$HOME/env/penrose"

./scripts/rebuild-penrose.sh
