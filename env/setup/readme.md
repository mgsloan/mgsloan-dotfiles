# Setup and maintenance

Numbered scripts cover bootstrap, installation, and later maintenance. Select
steps individually; these scripts are not a fully tested unattended installer.
Some require root and invoke `sudo` themselves. Script numbers indicate order.

- `010`–`022`: Git and the [home-directory repository](home-dir-git.md).
- `023`: Nix installation and profile activation.
- `030`: vendor packages.
- `040`–`041`: machine configuration and user timers.
- `044`–`046`: Penrose builds and display-manager sessions.
- `050`: source checkouts, remaining installs, and Nix user services.

System and desktop files live in `../system/` and `../desktop/`. Component
`apply.sh` scripts stay with their configuration; these setup steps call them.
`prune-gitignores.py` reports unused ignore rules, or removes them with `--apply`.
