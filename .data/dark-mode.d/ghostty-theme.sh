#!/bin/bash

cp -f ~/.config/ghostty/theme-dark ~/.config/ghostty/theme

# Alacritty watches its config file; ghostty reads it once and then on SIGUSR2,
# so the terminals already up have to be told. Nothing running is not a
# failure, which is what pkill's exit code would otherwise make it.
# Nix's .ghostty-wrapped process name is truncated to 15 bytes by Linux.
pkill -USR2 -u "$UID" -x 'ghostty|\.ghostty-wrappe' || true
