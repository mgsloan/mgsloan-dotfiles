#!/bin/bash

cp -f ~/.config/ghostty/theme-light ~/.config/ghostty/theme

# Alacritty watches its config file; ghostty reads it once and then on SIGUSR2,
# so the terminals already up have to be told. Nothing running is not a
# failure, which is what pkill's exit code would otherwise make it.
pkill -USR2 -x ghostty || true
