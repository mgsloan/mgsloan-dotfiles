#!/bin/bash -e

# Build asdcontrol, used by env/scripts/asd-brightness.sh to set the Apple
# Studio Display's brightness. The display speaks a USB-HID protocol rather
# than DDC/CI, so ddcutil cannot drive it; asdcontrol talks to its hiddev node.
#
# Non-root access to that node is granted by the `video` group via
# env/udev-rules/90-apple-studio-display.rules (installed by 040-udev-rules.sh).
#
# The submodule itself is checked out by 022-homedir-revive.sh.

if ! dpkg -s build-essential >/dev/null 2>&1; then
  sudo apt install --yes build-essential
fi

make -C ~/oss/asdcontrol
