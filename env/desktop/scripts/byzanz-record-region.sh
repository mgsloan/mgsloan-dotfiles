#!/bin/bash -e

# NOTE: I'm not entirely happy with this script, mostly because I
# don't like predicting ahead of time the duration of the
# recording. It would also be nice if the recording rectangle was
# visible after setting it.

# Delay before starting
DELAY=2

DURATION=$1
OUTPUT=$2
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

notify-send "Byzanz started recording" "(for $DURATION seconds)"
GDK_SCALE=1 byzanz-record --verbose --delay=0 ${ARGUMENTS} --duration=$DURATION $OUTPUT
notify-send "Byzanz finished recording" $OUTPUT
