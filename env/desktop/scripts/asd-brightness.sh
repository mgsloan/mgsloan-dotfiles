#!/usr/bin/env bash
# Apple Studio Display brightness control via asdcontrol.
# Device access is granted by system/udev-rules/90-apple-studio-display.rules.
#
#   asd-brightness.sh           # detect + report current brightness
#   asd-brightness.sh 30000     # set absolute brightness (range ~400-60000)
#   asd-brightness.sh 50%        # set by percentage
#   asd-brightness.sh +5960      # increase
#   asd-brightness.sh -- -5960   # decrease (note the --)

set -euo pipefail

if ! ASD=$(command -v asdcontrol); then
    echo "asdcontrol not on PATH; activate env-nix and source nix/scripts/nix-session.sh" >&2
    exit 1
fi

DEV=""

# Primary: ask asdcontrol which hiddev node is a SUPPORTED Apple display.
# Detect output looks like:  /dev/usb/hiddev2: USB Monitor - SUPPORTED. ...
# (UNSUPPORTED lines are excluded by matching "- SUPPORTED" exactly.)
echo "Detecting Apple Studio Display among /dev/usb/hiddev* ..."
DETECT_OUT="$("$ASD" --detect /dev/usb/hiddev* 2>/dev/null || true)"
DEV="$(printf '%s\n' "$DETECT_OUT" | grep -- '- SUPPORTED' | head -n1 | cut -d: -f1 | tr -d '[:space:]')"

# Fallback: locate the hiddev node by USB id 05ac:1114 via sysfs.
if [ -z "$DEV" ]; then
    echo "  asdcontrol detect inconclusive; trying sysfs fallback (USB 05ac:1114)..."
    for hd in /sys/class/usbmisc/hiddev*; do
        [ -e "$hd" ] || continue
        node="/dev/usb/$(basename "$hd")"
        # Walk up to the USB device and read idVendor/idProduct.
        dpath="$(readlink -f "$hd/device")"
        while [ -n "$dpath" ] && [ "$dpath" != "/" ]; do
            if [ -r "$dpath/idVendor" ] && [ -r "$dpath/idProduct" ]; then
                v="$(cat "$dpath/idVendor")"; p="$(cat "$dpath/idProduct")"
                if [ "$v" = "05ac" ] && { [ "$p" = "1114" ] || [ "$p" = "9243" ]; }; then
                    DEV="$node"; break
                fi
            fi
            dpath="$(dirname "$dpath")"
        done
        [ -n "$DEV" ] && break
    done
fi

if [ -z "$DEV" ]; then
    echo "Could not identify the Studio Display. Full --detect output:" >&2
    printf '%s\n' "$DETECT_OUT" >&2
    exit 1
fi

echo "  -> Using display at: $DEV"

if [ "$#" -eq 0 ]; then
    echo "Current brightness:"
    "$ASD" "$DEV"
else
    echo "Setting brightness to: $*"
    "$ASD" "$DEV" "$@"
    echo "New brightness:"
    "$ASD" --brief "$DEV"
fi
