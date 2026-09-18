#!/bin/bash -e

"$HOME/env/bin/env-nix" activate --working-tree "$@"
python3 "$HOME/env/setup/050-nix-services.py"
