#!/usr/bin/env bash
# Based loosely on https://news.ycombinator.com/item?id=39713402
TEXT="$(maim --hidecursor --select --nodrag | tesseract --dpi 145 -l eng+eng - -)"
notify-send "screenshot-ocr.sh" "Copied OCR result to clipboard: $TEXT"
echo "$TEXT" | xsel -bi
