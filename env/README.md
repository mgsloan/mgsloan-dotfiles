# Computer environment

Public packages, desktop configuration, machine configuration, and setup for
this home-directory repository. Use `cfg` for Git operations; see
[the repository setup](setup/home-dir-git.md).

| Area | Files |
| --- | --- |
| Packages | [flake.nix](flake.nix), [nix/](nix/), [Nix design and usage](nix/design.md) |
| Desktop | [desktop/](desktop/): session entries, desktop scripts, session units, browser styles, and backgrounds |
| System | [system/](system/): services, kernel settings, device permissions, boot configuration, and administration scripts |
| Hardware | [hardware/keyboard/](hardware/keyboard/readme.md): keyboard layouts and firmware |
| Setup and maintenance | [setup/](setup/readme.md): ordered installation steps and repository maintenance |
| Window manager | [penrose/](penrose/design.md): source and design for the X11 and river window managers |
| Logging tools | [errlog-filter/](errlog-filter/README.md): journal filtering, auditing, and process logging |
| Cross-cutting designs | [docs/](docs/), including the [development environment](docs/dev-environment.md) |
| Tests | [tests/](tests/): environment integration tests; Rust tests live with their projects |

## Entry points

- `env-nix` in `bin/` builds and activates the package environment.
- `freshen` in `~/.local/bin/` refreshes installed software and calls
  `nix/scripts/freshen-nix.sh`.
- `setup/` contains individually selected installation and maintenance steps.
  Component-specific `apply.sh` scripts stay beside their configuration.
- `penrose/scripts/rebuild-penrose.sh` checks, builds, and installs both window
  managers. `M-q` rebuilds and restarts the current window manager.
- `python3 -m unittest discover -s tests` runs the environment tests.

Keep designs beside their subject: [Nix](nix/design.md),
[Penrose](penrose/design.md), and [workspaces](desktop/workspaces-design.md).
`docs/` is for designs spanning several areas. `untracked/` holds local state
and is excluded from Git.

Temporary compatibility links for the directory migration are described in
[cleanup.md](cleanup.md).
