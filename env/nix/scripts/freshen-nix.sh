#!/bin/bash -e

"$HOME/env/bin/env-nix" activate --working-tree \
  --include nix/patches/river-keystroke-histograms.patch "$@"
python3 "$HOME/env/setup/050-nix-services.py"
