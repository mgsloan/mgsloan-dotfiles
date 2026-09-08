#!/bin/sh
# Hourly-triggered: if we're on AC power, the machine is idle (load avg < 5),
# and yesterday's webcam capture directory exists, encode it to HEVC and
# delete the source JPEGs. Skips (exit 0) if the output already exists, so
# it's safe to be retried hourly until conditions are met.
#
# hqdn3d denoises before encoding: capture noise differs frame to frame even
# where the scene is static, which both looks like shimmer and wastes bits
# that x265 would otherwise spend treating an unchanged background as
# changing. hqdn3d's temporal component backs off on pixels that genuinely
# change (real motion), so it doesn't ghost across the 30s gaps between
# frames.
#
# -framerate 48 (playback speed) with -fps_mode passthrough guarantees the
# output frame count exactly matches the input JPEG count -- no frames are
# duplicated or dropped to hit that rate.

set -eu

AC=/sys/class/power_supply/AC/online
BASE="$HOME/pics/webcam"
DATE=$(date -d yesterday +%Y-%m-%d)
INDIR="$BASE/$DATE"
OUTFILE="$BASE/$DATE.hevc"
TSFILE="$BASE/$DATE-timestamps"

[ "$(cat "$AC" 2>/dev/null)" = 1 ] || { echo "webcam-encode-daily: not on AC power, skipping"; exit 0; }

load=$(cut -d' ' -f1 /proc/loadavg)
awk "BEGIN { exit !($load < 5) }" || { echo "webcam-encode-daily: load average $load >= 5, skipping"; exit 0; }

[ -d "$INDIR" ] || { echo "webcam-encode-daily: $INDIR does not exist, skipping"; exit 0; }

if [ -e "$OUTFILE" ]; then
    echo "webcam-encode-daily: $OUTFILE already exists, skipping"
    exit 0
fi

jpg_count=$(find "$INDIR" -maxdepth 1 -name '*.jpg' | wc -l)
if [ "$jpg_count" -eq 0 ]; then
    echo "webcam-encode-daily: no jpgs in $INDIR, skipping"
    exit 0
fi

for f in "$INDIR"/*.jpg; do
    basename "$f" .jpg | tr '-' ':'
done > "$TSFILE"

ffmpeg -y -loglevel error \
    -framerate 48 -pattern_type glob -i "$INDIR/*.jpg" \
    -vf 'hqdn3d=8:6:20:15' \
    -c:v libx265 -preset slow -crf 24 -pix_fmt yuv420p -fps_mode passthrough \
    "$OUTFILE"

out_frames=$(ffprobe -v error -count_frames -select_streams v:0 \
    -show_entries stream=nb_read_frames -of csv=p=0 "$OUTFILE")

if [ "$out_frames" != "$jpg_count" ]; then
    echo "webcam-encode-daily: frame count mismatch ($out_frames video frames vs $jpg_count jpgs); leaving $INDIR in place" >&2
    rm -f "$OUTFILE" "$TSFILE"
    exit 1
fi

rm -rf "$INDIR"
