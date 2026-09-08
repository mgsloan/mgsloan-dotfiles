# Machine services

Install only the unit needed by this machine, then run
`sudo systemctl daemon-reload`.

- `powertop.service` shadows the unit the powertop package ships in
  `/lib/systemd/system/`, for as long as the copy exists and across upgrades.
  This one names `/usr/bin/powertop`, where Debian installs `/usr/sbin/powertop`.
- `nvidia-persistenced.service` is for a machine with the NVIDIA driver on it.
- `cpu-governor.service` re-applies the stored CPU mode at boot and after
  suspend/hibernate. The AC-change udev rule queues this same service without
  waiting. `auto` means performance on AC and power-saver on battery; explicit
  modes survive charger changes and sleep. Power-profiles-daemon owns the actual
  governor/EPP settings. The service does no work before sleep and exits after
  applying the profile, so each resume can start it again.
  Run `../../setup/041-cpu-governor.sh` to install or update it. This also
  removes the old `multi-user.target` enablement: that target cannot wait for
  this service because power-profiles-daemon is ordered after it. Boot uses
  `graphical.target`; resume is ordered after the systemd sleep services.

Two things to know about the copy itself. Overwriting a unit that is already
enabled keeps it enabled: `systemctl enable` symlinks from `<target>.wants/` to
the path, so only a change to `[Install]` needs a re-enable. And `cp --force`
writes *through* a symlink at the destination, so copying onto a masked unit —
which is a symlink to `/dev/null` — silently writes nothing.

Desktop session units live in [../../desktop/systemd/](../../desktop/systemd/).
