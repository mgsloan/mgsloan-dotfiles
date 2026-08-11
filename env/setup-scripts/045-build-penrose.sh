#!/bin/bash -ev

# The commit this window manager is built against is a submodule. The other
# checkout, vendor/penrose, is a plain clone for hacking on the library itself:
# untracked and not needed to build, so it is not created here.

cd "$HOME"

cfg submodule update --init env/penrose/vendor/penrose-pinned

cd "$HOME/env/penrose"

./scripts/rebuild-penrose.sh
