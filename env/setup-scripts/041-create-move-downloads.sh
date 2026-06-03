#!/bin/bash -e

# Installs a systemd *user* timer that moves stale ~/dl files into ~/.dlo
# hourly. A user timer (rather than /etc/cron.hourly) is what we want here:
# it runs in the context of the logged-in user, with $HOME set correctly.

TEMPLATE_DIR="$HOME/env/templates"
UNIT_DIR="$HOME/.config/systemd/user"

mkdir -p "$UNIT_DIR"
cp "$TEMPLATE_DIR/move-downloads.service" "$UNIT_DIR/move-downloads.service"
cp "$TEMPLATE_DIR/move-downloads.timer"   "$UNIT_DIR/move-downloads.timer"

systemctl --user daemon-reload
systemctl --user enable --now move-downloads.timer

echo ""
echo "move-downloads.timer is now active:"
echo ""
systemctl --user list-timers move-downloads.timer --no-pager
