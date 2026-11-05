#!/bin/bash -e

# The notify-hourly and move-downloads timers are enabled *declaratively*:
# their unit files and the timers.target.wants/ symlinks live in
# ~/.config/systemd/user/ and are versioned in the home-dir git repo. So a
# fresh checkout + login auto-starts them with no setup step.
#
# The same-spot service reads the existing webcam captures. Its dependencies
# must be installed by freshen before enabling it.
#
# This script applies changes within an ALREADY-running session
# (e.g. after editing a unit) without having to log out and back in.

systemctl --user daemon-reload
systemctl --user start move-downloads.timer notify-hourly.timer
systemctl --user enable --now same-spot.service

echo ""
systemctl --user list-timers move-downloads.timer notify-hourly.timer --no-pager
