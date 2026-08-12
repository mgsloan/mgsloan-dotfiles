#!/bin/sh -e
# Build the window manager and install it where run-penrose.sh looks for it.
#
# Invoked by M-q from inside the running window manager, so it must not assume
# a terminal is attached: output goes to the caller, which logs it.
#
# Installing over a running binary is fine -- `install` replaces the directory
# entry rather than writing through it, so the running process keeps its own
# copy until it exits.

cd "$(dirname "$0")/.."

cargo build --release

mkdir -p "$HOME/.local/bin"
install -m 755 target/release/penrose-wm "$HOME/.local/bin/penrose-wm"
