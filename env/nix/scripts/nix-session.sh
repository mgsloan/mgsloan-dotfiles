# Source after language-manager setup so personal commands retain precedence.
NIX_ENV_PROFILE=${NIX_ENV_PROFILE:-"$HOME/.local/state/nix/profiles/env"}
if [ -d "$NIX_ENV_PROFILE/bin" ]; then
    PATH="$HOME/env/bin:$HOME/.local/bin:$NIX_ENV_PROFILE/bin:$PATH"
    XDG_DATA_DIRS="$NIX_ENV_PROFILE/share:${XDG_DATA_DIRS:-/usr/local/share:/usr/share}"
    export PATH XDG_DATA_DIRS
fi
