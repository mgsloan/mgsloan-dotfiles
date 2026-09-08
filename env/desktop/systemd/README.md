# Desktop session units

- `river-session.target` groups river session services. Setup links it into
  `~/.config/systemd/user/`; see
  [the river session installer](../../setup/046-create-penrose-river-session.sh).
- `slock@.service` locks X11 sessions before sleep. It skips Wayland sessions,
  where swayidle handles locking. This is a system unit, installed under
  `/etc/systemd/system/`, despite belonging to the desktop configuration.

After changing an installed unit, update its copy and reload the appropriate
systemd manager (`systemctl --user daemon-reload` for the river target,
`sudo systemctl daemon-reload` for slock).
