#!/bin/sh -e

# Note that this script requires superuser privileges. So, on Ubuntu
# use "sudo".

cp --force *.rules --target-directory=/etc/udev/rules.d/
udevadm control --reload-rules
