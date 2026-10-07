#!/bin/bash -e

"$HOME/env/bin/env-nix" activate --working-tree --update \
  --include nix/patches/river-keystroke-histograms.patch "$@"
python3 "$HOME/env/setup/050-nix-services.py"
"$HOME/env/bin/env-nix" overrides
