#!/bin/bash -e

"$HOME/env/bin/env-nix" activate "$@"
python3 "$HOME/env/setup-scripts/050-nix-services.py"
