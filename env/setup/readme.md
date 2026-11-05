# Setup and maintenance

Numbered scripts cover bootstrap, installation, and later maintenance. Select
steps individually; these scripts are not a fully tested unattended installer.
Some require root and invoke `sudo` themselves. Script numbers indicate order.

- `010`–`022`: Git and the [home-directory repository](home-dir-git.md).
- `023`: retained Nix installation and profile activation for recovery.
- `024`: native vendor APT sources for GitHub CLI and Google Cloud CLI.
- `030`: Chrome.
- `040`–`041`: machine configuration and user timers.
- `044`–`046`: Penrose builds and display-manager sessions.
- `050`: Emacs checkout and remaining installs. Nix user-service setup is retained
  for recovery; native service selection happens through `env-debian activate`.

Optional applications and personal project checkouts live in `~/ep/setup.sh`.

`040-snapper.sh` installs Snapper for the Btrfs root filesystem, takes a daily
snapshot at midnight (or the next boot), and retains seven daily snapshots.

System and desktop files live in `../system/` and `../desktop/`. Component
`apply.sh` scripts stay with their configuration; these setup steps call them.
`prune-gitignores.py` reports unused ignore rules, or removes them with `--apply`.
