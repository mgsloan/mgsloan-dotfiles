#!/bin/bash -e

"$HOME/env/bin/env-nix" activate --working-tree --update \
  --include nix/patches/river-keystroke-histograms.patch \
  --include desktop/scripts/same-spot.py --include tests/test_same_spot.py \
  --include desktop/scripts/same-spot-review.py --include desktop/same-spot-review.html \
  --include tests/test_same_spot_review.py "$@"
python3 "$HOME/env/setup/050-nix-services.py"
"$HOME/env/bin/env-nix" overrides
