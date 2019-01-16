#!/bin/bash -e

# NOTE: I'm not entirely happy with this script, mostly because I
# don't like predicting ahead of time the duration of the
# recording. It would also be nice if the recording rectangle was
# visible after setting it.

# Delay before starting
DELAY=5

DURATION=$1
OUTPUT=$2
OUTPUT_TMP=/tmp/byzanz-output.gif

if [ -f $OUTPUT ]; then
    echo "$OUTPUT already exists."
    exit 1
fi

# Duration and output file
if [ $# -gt 0 ]; then
    D="--duration=$@"
else
    echo Default recording duration 10s to /tmp/recorded.gif
    D="--duration=10 /tmp/recorded.gif"
fi

# xrectsel from https://github.com/lolilolicon/FFcast2/blob/master/xrectsel.c
ARGUMENTS=$(xrectsel "--x=%x --y=%y --width=%w --height=%h") || exit -1

echo Delaying $DELAY seconds. After that, byzanz will start
for (( i=$DELAY; i>0; --i )) ; do
    echo $i
    sleep 1
done
notify-send "GIFRecorder" "Started recording"
if [ -n HIDPI ]; then
    rm -f $OUTPUT_TMP
    GDK_SCALE=1 byzanz-record --verbose --delay=0 ${ARGUMENTS} --duration=$DURATION $OUTPUT_TMP
    notify-send "GIFRecorder" "Finished recording"
    convert $OUTPUT_TMP -layers coalesce -resize 50% -layers optimize $OUTPUT
    notify-send "GIFRecorder" "Finished converting"
else
    GDK_SCALE=1 byzanz-record --verbose --delay=0 ${ARGUMENTS} --duration=$DURATION
    notify-send "GIFRecorder" "Finished recording"
fi
