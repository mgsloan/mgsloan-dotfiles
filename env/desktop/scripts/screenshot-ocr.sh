#!/usr/bin/env bash
# Read text off a selected region of the screen and put it on the clipboard.
# Based loosely on https://news.ycombinator.com/item?id=39713402
#
# Shared with both window manager configs, so it picks its own tools rather
# than being told: maim and xsel are X11 only, and under Wayland maim opens on
# the Xwayland root, which under a rootless server holds none of the session.
# tesseract is the same on both sides -- it reads a PNG on stdin either way.
set -o pipefail

if [ -n "$WAYLAND_DISPLAY" ]; then
    # A cancelled selection exits slurp non-zero and is not worth a
    # notification saying nothing was read.
    GEOMETRY=$(slurp) || exit 0
    TEXT="$(grim -g "$GEOMETRY" - | tesseract --dpi 145 -l eng+eng - -)"
    COPY=(wl-copy)
else
    TEXT="$(maim --hidecursor --select --nodrag | tesseract --dpi 145 -l eng+eng - -)"
    COPY=(xsel -bi)
fi

if [ -z "${TEXT//[[:space:]]/}" ]; then
    notify-send "screenshot-ocr.sh" "No text found in the selection"
    exit 1
fi

notify-send "screenshot-ocr.sh" "Copied OCR result to clipboard: $TEXT"
printf '%s\n' "$TEXT" | "${COPY[@]}"
