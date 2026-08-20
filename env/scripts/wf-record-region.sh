#!/bin/bash -e

# The Wayland counterpart of byzanz-record-region.sh: record a region as a gif.
#
# Usage: wf-record-region.sh DURATION OUTPUT.gif
#
# Same two arguments and same result, because the same binding calls both --
# but nothing in between is shared. byzanz reads the X11 damage extension and
# writes the gif itself; wf-recorder reads wlr-screencopy and writes a video,
# so the gif is ffmpeg's job afterwards.
#
# The duration is still predicted up front rather than ended on a keypress,
# which byzanz-record-region.sh complains about and is right to. wf-recorder
# stops on SIGINT, so ending it on a second keypress is possible here in a way
# it was not there; it needs somewhere to keep the pid, which is a change to
# the binding rather than to this script.

DELAY=2

DURATION=$1
OUTPUT=$2
OUTPUT_TMP=/tmp/wf-record-region.mp4

if [ -z "$DURATION" ] || [ -z "$OUTPUT" ]; then
    echo "usage: $(basename "$0") DURATION OUTPUT.gif" >&2
    exit 2
fi

if [ -f "$OUTPUT" ]; then
    echo "$OUTPUT already exists."
    exit 1
fi

for cmd in slurp wf-recorder ffmpeg; do
    if ! command -v $cmd > /dev/null; then
        notify-send "wf-record-region.sh" "$cmd is not installed"
        exit 1
    fi
done

# Cancelling the selection exits slurp non-zero, which -e would turn into a
# failure the caller reports as an error. It is not one.
GEOMETRY=$(slurp) || exit 3

echo "Delaying $DELAY seconds. After that, wf-recorder will start"
for (( i=DELAY; i>0; --i )) ; do
    echo $i
    sleep 1
done

rm -f "$OUTPUT_TMP"
notify-send "wf-recorder started recording" "(for $DURATION seconds)"

# -s INT rather than the default TERM: wf-recorder traps SIGINT to finalise the
# file, and a TERM leaves an mp4 with no moov atom that ffmpeg then refuses.
#
# `timeout` reports 124 whenever it had to signal the command, which here is
# every run that is not cut short, so its status says nothing about whether the
# recording worked. What the file looks like afterwards does.
timeout -s INT "$DURATION" wf-recorder -g "$GEOMETRY" -f "$OUTPUT_TMP" || true

if [ ! -s "$OUTPUT_TMP" ]; then
    notify-send "wf-record-region.sh" "wf-recorder produced nothing"
    exit 1
fi

notify-send "wf-recorder finished recording" "Converting to gif"

# Two passes over one input: palettegen reads the whole clip to pick 256
# colours, paletteuse then maps onto them. The single-pass alternative picks a
# palette per frame and the result shimmers.
#
# 10fps because this is a gif. No scaling, unlike the byzanz script, which
# halved everything for a HiDPI screen -- on a display that is not one, halving
# is how a recording of a terminal comes out unreadable.
ffmpeg -y -loglevel warning -i "$OUTPUT_TMP" \
    -vf "fps=10,split[s0][s1];[s0]palettegen=stats_mode=diff[p];[s1][p]paletteuse=dither=bayer" \
    "$OUTPUT"

rm -f "$OUTPUT_TMP"
notify-send "wf-record-region.sh finished converting" "$OUTPUT"
