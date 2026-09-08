#!/bin/bash -e

"$HOME/env/bin/env-nix" activate "$@"
python3 "$HOME/env/setup/050-nix-services.py"
