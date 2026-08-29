#!/bin/sh
# Grab one frame from the webcam in its native MJPEG format (stream copy, no
# re-encode) and drop it into a per-day directory, named to the second so
# frame count and timing can be reconstructed exactly by webcam-encode-daily.sh.
#
# Skips while the screen is locked, and while `M-x inhibit-webcam` has left an
# unexpired deadline in webcam-inhibit-until (see
# ~/env/penrose/src/actions/webcam.rs).

set -eu

STATE_FILE="${XDG_STATE_HOME:-$HOME/.local/state}/penrose/webcam-inhibit-until"
if [ -f "$STATE_FILE" ]; then
    inhibit_until=$(cat "$STATE_FILE")
    now=$(date +%s)
    [ "$inhibit_until" -gt "$now" ] 2> /dev/null && exit 0
fi

if [ -n "${WAYLAND_DISPLAY:-}" ]; then
    pgrep -x swaylock > /dev/null && exit 0
else
    pgrep -x slock > /dev/null && exit 0
fi

OUTDIR="$HOME/pics/webcam/$(date +%Y-%m-%d)"
mkdir -p "$OUTDIR"

# Grab 10 frames and keep only the last one: the camera needs a handful of
# frames to run auto-exposure after opening the device, so the 1st frame is
# often too dark/bright.
TMPDIR=$(mktemp -d)
trap 'rm -rf "$TMPDIR"' EXIT

ffmpeg -y -loglevel error \
    -f v4l2 -input_format mjpeg -video_size 1280x720 -i /dev/video0 \
    -frames:v 10 -c:v copy \
    "$TMPDIR/frame-%02d.jpg"

mv "$TMPDIR/frame-10.jpg" "$OUTDIR/$(date +%H-%M-%S).jpg"
