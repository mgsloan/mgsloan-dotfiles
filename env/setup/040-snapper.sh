#!/bin/bash
set -euo pipefail

[ "$UID" -eq 0 ] || exec sudo bash "$0" "$@"

if [ "$(findmnt -n -o FSTYPE /)" != btrfs ]; then
  echo 'The root filesystem must be Btrfs.' >&2
  exit 1
fi

# Use Debian's package for its system services and APT integration.
apt-get install --yes snapper btrfs-progs
systemctl stop snapper-timeline.timer snapper-cleanup.timer
systemctl disable --now snapper-boot.timer

# APT and boot snapshots would accumulate outside the daily timeline policy.
sed -i 's/^DISABLE_APT_SNAPSHOT=.*/DISABLE_APT_SNAPSHOT="yes"/' /etc/default/snapper

if [ ! -f /etc/snapper/configs/root ]; then
  snapper --config root create-config /
fi

snapper --config root set-config \
  TIMELINE_CREATE=yes \
  TIMELINE_CLEANUP=yes \
  TIMELINE_MIN_AGE=0 \
  TIMELINE_LIMIT_HOURLY=0 \
  TIMELINE_LIMIT_DAILY=7 \
  TIMELINE_LIMIT_WEEKLY=0 \
  TIMELINE_LIMIT_MONTHLY=0 \
  TIMELINE_LIMIT_YEARLY=0

mkdir -p /etc/systemd/system/snapper-timeline.{timer,service}.d
cat > /etc/systemd/system/snapper-timeline.timer.d/daily.conf <<'EOF'
[Timer]
OnCalendar=
OnCalendar=daily
Persistent=true
EOF

# Wait for snapshot creation before enforcing the retention limit.
cat > /etc/systemd/system/snapper-timeline.service.d/cleanup.conf <<'EOF'
[Service]
Type=oneshot
ExecStartPost=/usr/bin/snapper --config root cleanup timeline
EOF

systemctl daemon-reload
systemctl enable --now snapper-timeline.timer snapper-cleanup.timer
systemctl start snapper-timeline.service
snapper --config root list
systemctl list-timers snapper-timeline.timer snapper-cleanup.timer --no-pager
