#!/bin/bash

# The motivation for this is that for me, and many others, xmodmap
# does not affect plugged in keyboards.
#
# See, for example https://bugs.launchpad.net/ubuntu/+source/xorg-server/+bug/287215

# Solution inspired by https://superuser.com/a/988625

while true ; do
  inotifywait -q -e create --exclude '.*tmp.*' /dev/input
  sleep 1
  echo "Something in /dev/input changed so running xmodmap"
  xmodmap ~/.Xmodmap
done
