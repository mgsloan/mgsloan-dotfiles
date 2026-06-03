#!/bin/bash -e

# The notify-hourly and move-downloads timers are enabled *declaratively*:
# their unit files and the timers.target.wants/ symlinks live in
# ~/.config/systemd/user/ and are versioned in the home-dir git repo. So a
# fresh checkout + login auto-starts them with no setup step.
#
# This script only exists to apply changes within an ALREADY-running session
# (e.g. after editing a unit) without having to log out and back in.

systemctl --user daemon-reload
systemctl --user start move-downloads.timer notify-hourly.timer

echo ""
systemctl --user list-timers move-downloads.timer notify-hourly.timer --no-pager
