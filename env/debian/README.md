# Native Debian packages

`freshen` installs `~/env/apt-packages.md` and the private `~/ep/apt-packages.md`,
then builds and installs the public and private Debian catalogs. It configures
the vendor APT sources for GitHub CLI and Google Cloud CLI. Private vendor
applications and language-managed tools remain under `~/ep`.

The Nix flakes, helper, scripts, daemon, profiles, and store outputs are retained.
Native selection happens only after all required packages are installed. Until
then, login and terminal setup continue selecting the existing Nix environment.
Activation migrates only launchers provided by the Debian catalogs; independent
applications such as the separately managed Zathura installation retain their launchers.
Catalog packages select exact versions; their installation transaction permits
downgrades to those artifacts. No global APT downgrade policy or holds are added.

## Building

```sh
env-debian list
env-debian updates ghostty
env-debian build waynav
env-debian build waynav --checkout ~/oss/waynav
env-debian install waynav
```

Builds use git-buildpackage and sbuild with an unshare build root for forky/amd64.
`packages.toml` records source URLs, checksums, build dependencies, build commands,
and patches. It is the source of truth for generated packaging. The helper
maintains independent gbp repositories under
`~/.local/state/debian-packages/managed/<name>/repository`; it never stages or
commits the home-directory repository. Preserve changes to those generated
repositories before asking the helper to regenerate them.

`updates` delegates upstream discovery to uscan without changing source pins or
rebasing patches. Update the catalog's URL, version, and checksum to select a
release, then build and test it before installation.

Cargo and Go dependencies are vendored before the clean build. Zig dependencies
are fetched into the source snapshot. Package compilation uses Debian libraries;
Zig itself is supplied as versioned compiler packages. River uses 0.16 and
Ghostty 1.4 uses 0.17. The default `zig` command remains 0.16.

Applications available as upstream Debian packages use those packages directly.
Portable CLI releases are packaged with their upstream libraries, with specific
lintian overrides for their bundled/static runtimes. Source recipes remain
available for the CLI applications that have a source entry. An explicit
`--checkout` selects that source route instead of an upstream binary.

A local checkout is copied without its build caches or Git metadata. Working
changes remain unstaged. Already-applied catalog patches are omitted from that
snapshot's patch series. Broadly different checkout versions can require updates
to the catalog's build dependencies or commands.

An installed local checkout selection is retained by subsequent `freshen` runs.
`env-debian build NAME` without `--checkout` explicitly selects the pinned default
again. Package versions include an increasing local snapshot number. Changed
builds retain their own source, binary packages, `.changes`, `.buildinfo`, and
logs under `~/.local/state/debian-packages/builds/<name>/<version>/forky-amd64`.

For fast incremental development, use the project's ordinary build directory.
Install its declared host dependencies from the generated packaging tree with
`sudo apt-get build-dep .`; sbuild continues provisioning dependencies separately
for clean package builds. Keep development libraries out of the application lists.

Older `.deb` artifacts can be installed explicitly with
`sudo apt-get install --allow-downgrades /path/to/package.deb`. That is a temporary
selection: `freshen` reinstalls the helper's current build. To retain an older
upstream release across refreshes, change its catalog pin and rebuild it. To
leave a local checkout, explicitly build the pinned default without `--checkout`.

## Selection and recovery

```sh
env-debian check
env-debian activate
```

Activation verifies the installed package versions and APT requirements before
selecting Debian. It saves Nix launch links and the previous session environment
under `~/.local/state/debian-packages/recovery/activation-*/`, reloads user service
and D-Bus configuration, and restarts the managed theme/notification/reminder
services. Existing terminal and compositor processes continue running. Log in
again to give the compositor and window manager the native environment.

The session selector removes inherited Nix graphics settings and Nix profile
search paths in native mode. It preserves the old Nix session script for
recovery. To restore the saved selection, use the activation record printed by
the helper:

```sh
env-debian rollback ~/.local/state/debian-packages/recovery/activation-<timestamp>/activation.json
. ~/env/debian/session.sh
```

Rollback restores launch links and selection; it leaves installed Debian
packages in place. Nix profile generations remain rooted and can still be
managed with `env-nix`. `ENV_PACKAGE_BACKEND=nix` selects the Nix session paths
for a recovery shell without deleting the native configuration.
The first migration record restores Nix selection; later refresh records may
restore an already-native session.

The original pilot results are in [the adoption plan](../docs/debian-packaging-plan.md).
