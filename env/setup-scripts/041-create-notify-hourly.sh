#!/bin/bash -e

# Installs a systemd *user* timer that fires ~/env/scripts/notify-hourly.sh
# at the top of every hour. This must be a user timer (not /etc/cron.hourly):
# notify-send needs the user's session D-Bus to reach the notification daemon,
# which a root cron job does not have.

TEMPLATE_DIR="$HOME/env/templates"
UNIT_DIR="$HOME/.config/systemd/user"

mkdir -p "$UNIT_DIR"
cp "$TEMPLATE_DIR/notify-hourly.service" "$UNIT_DIR/notify-hourly.service"
cp "$TEMPLATE_DIR/notify-hourly.timer"   "$UNIT_DIR/notify-hourly.timer"

systemctl --user daemon-reload
systemctl --user enable --now notify-hourly.timer

echo ""
echo "notify-hourly.timer is now active:"
echo ""
systemctl --user list-timers notify-hourly.timer --no-pager
