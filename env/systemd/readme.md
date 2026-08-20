See [../setup.md] for the reasoning behind these systemd service
files.  To copy all of them to your system, you could do:

```
sudo cp --force *.service --target-directory=/etc/systemd/system/
sudo systemctl daemon-reload
```

That is a bootstrap, not a way to apply one edit. Copy the single file you
changed instead, because the glob is not all of a piece:

- `slock@.service` is installed and enabled here. It locks the X11 sessions
  before sleep, and skips itself in a Wayland one — the river sessions lock from
  swayidle instead.
- `powertop.service` shadows the unit the powertop package ships in
  `/lib/systemd/system/`, for as long as the copy exists and across upgrades.
  This one names `/usr/bin/powertop`, where Debian installs `/usr/sbin/powertop`.
- `nvidia-persistenced.service` is for a machine with the NVIDIA driver on it.

Two things to know about the copy itself. Overwriting a unit that is already
enabled keeps it enabled: `systemctl enable` symlinks from `<target>.wants/` to
the path, so only a change to `[Install]` needs a re-enable. And `cp --force`
writes *through* a symlink at the destination, so copying onto a masked unit —
which is a symlink to `/dev/null` — silently writes nothing.
