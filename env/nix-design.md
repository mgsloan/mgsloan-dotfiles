# Nix migration design

## Goals

* Build user tools from pinned inputs.
* Share build results across projects and users through the machine's Nix store.
* Switch one package to a patched checkout under `~/oss` without editing pins.
* Keep the home-dir repository, `cfg`, and existing session setup working.
* Migrate incrementally without replacing Debian or making the whole home
  directory declarative.

Use multi-user Nix, one flake maintained in `~/env`, and package expressions in
`~/env/nix`. Home Manager is not required.

This is the target design. The existing expressions, helper, and setup scripts
implement only part of it; command examples below describe the target interface.

## Implementation status and bootstrap

The helper implements selected source snapshots, explicit working-tree and local
builds, lock updates, exact-output profile activation, and profile rollback. Builds
print their store path and retain an output root and build record under
`~/.local/state/env-nix/builds`. `env-nix snapshot` lists the selected flake files
without requiring Nix; `env-nix check` checks the selected flake.

The installed `environment` contains command-line tools, Ghostty, river, asdcontrol,
keynav, waynav, dunst, and darkman. `env-nix activate-local <package>` installs a
rooted build through an untracked executable link and records how to restore the
previous link. It refuses tracked files, existing regular binaries, and darkman,
which requires service activation. Ghostty and river/wlroots are pinned to the
clean development checkouts and buildable individually or through `desktop`.
Service recovery, Penrose packaging, and full desktop login validation remain pending.
The old navigation/notification executable links are removed; commands resolve
through the Nix profile on PATH. The brightness script also resolves asdcontrol
through PATH. Source checkouts remain for
development; the old system darkman installation remains for recovery.

`setup-scripts/050-nix-services.py` links user systemd and D-Bus service files to
the active profile, refusing to overwrite unrelated user files. It reloads both
managers and restarts systemd-managed daemons only if their executable changed.
`freshen-nix.sh` runs it after activation. After a manual `env-nix activate` or
profile rollback, run the service setup explicitly to synchronize running daemons.
It does not replace an unmanaged dunst process; stop that instance before starting
`dunst.service`. First-time setup also requires `systemctl --user enable --now darkman`.

Before rolling back to a profile without these packages, remove only the five
links installed by the service setup (listed in its `SERVICE_FILES` table), then
reload systemd and D-Bus. Restart darkman to use its retained `/usr` installation.
For dunst, stop its user service before removing the links and launch
`~/oss/dunst/dunst` from the graphical session. The tracked executable link deletions
can also be reverted separately when abandoning the migration.

The CLI environment also supplies Stack, pnpm, wasm-pack, and Google Cloud CLI.
Zig 0.16, Typst, gettext, and Git's libsecret credential helper are also Nix-managed.
Debian Git remains the bootstrap dependency; its credential helper is selected by
name (`libsecret`) through PATH rather than a binary compiled under `/usr/share/doc`.
The old helper remains available there for recovery. Zig's old toolchain directory
and `/usr/local/bin/typst` are retained, but refresh no longer installs them.
Udev templates use gettext's `envsubst`; no Ruby gem installation is needed.
Rustup remains outside Nix as a project toolchain manager. Existing Stack caches,
pnpm packages, Rust toolchains, and Google Cloud configuration are not removed.
Google Cloud components must be selected through Nix, not `gcloud components install`.

Build with `env-nix build river` or `env-nix build ghostty`; neither installs into `~/.local`.
Use `--working-tree` while reviewing uncommitted recipes. Ghostty is activated
with the environment; `archive-nix-replacements.py --package ghostty --apply`
archives its old installation without touching river. The river launcher retains
the local library path for the old binary and clears it for a Nix store binary.
River is activated for the next login; the current compositor is not restarted.
Its nested Wayland test passed with Intel GLES rendering and a Debian X11 client
connecting to Nix Xwayland. Full GDM/device/portal validation remains a login test.
Ghostty's executable and desktop/D-Bus launchers use the pinned nixGLIntel wrapper
for Debian graphics discovery, clearing the inherited library path first.
River also uses nixGLIntel. The session launcher strips its graphics variables
from the init process so Debian applications do not inherit Nix driver paths.

If the next river login fails, switch to a TTY with Ctrl+Alt+F3, log in, and restore
the archived binary (provided `~/.local/bin/river` is still absent):

```sh
cp -p --no-clobber ~/.local-old/bin/river ~/.local/bin/river
```

The launcher will select that binary and restore the old wlroots library path.
Its libraries remain in `~/.local/lib`; do not remove them until login validation
succeeds. The Nix profile can remain installed during this recovery.
For graphics testing, build `desktop`, enter `env-nix shell nixgl`, and run
`env -u LD_LIBRARY_PATH nixGLIntel <desktop-output>/bin/ghostty` (or `river`).
Use the printed output path: PATH may still select the old executable. Clearing
the inherited library path prevents Nix river from loading the old local wlroots.
Keep the recovery procedure available during the first Nix river login.

On Debian, setup adds the user to `nix-users` when that group exists. Log in again
after first joining the group. The helper enables the Nix command and flake
features for its invocations; setup also enables them in the daemon configuration.

Before the initial Nix files are committed, bootstrap explicitly:

```sh
env-nix update nixpkgs --include flake.nix --include nix/packages.nix
env-nix check --working-tree --include flake.nix --include flake.lock \
  --include nix/packages.nix
env-nix activate --working-tree --include flake.nix --include flake.lock \
  --include nix/packages.nix
```

If an untracked lock already exists, include it in the update command too. Review
and commit the Nix files before using ordinary committed refreshes. The helper
does not stage them. Setup sources `scripts/nix-session.sh` at the end of
`~/.profile` after successful activation; the river launcher also sources it.

Activation prints an `env-nix rollback <activation.json>` command. Recovery
restores the saved output as a profile generation, or removes only the newly
created profile link for a first installation. The saved outputs remain rooted.
This currently restores packages only; service and session migration must not
be enabled until their recovery procedure is implemented and tested.

Verified with Debian Nix 2.34.8: all current flake checks build successfully;
repeating them requires no compilation. A local waynav snapshot matches the
locked source hash. A patched asdcontrol checkout produces the patched help text
without changing the repository lock. An isolated profile switches from bat to
the aggregate and rolls back to the original store output. Helper regression
tests run with `python3 -m unittest discover -s tests -p 'test_env_nix.py'`.

## Existing installation boundaries

Software currently comes from apt, downloaded executables, language installers,
and builds from home-repository submodules. Those submodules pin source commits,
but their builds depend on mutable host toolchains and libraries.

Keep tracked personal scripts in `~/.local/bin`. Nix replaces third-party
executables and locally built artifacts. Debian and existing setup scripts retain
ownership of drivers, permissions, system services, GDM entries, and user-service
enablement. Large vendor applications and language managers for unrelated
projects remain outside this migration initially.

## Repository layout

Keep the expressions together and split out packages only when their size
obscures the package set:

```text
env/
  flake.nix
  flake.lock
  nix/
    packages.nix       # package set and overrides
    sources.nix        # source mappings, when needed
    checks.nix         # package builds and smoke tests, when needed
```

The flake exports:

```text
packages.x86_64-linux.<name>       individual packages
packages.x86_64-linux.tools        ordinary command-line tools
packages.x86_64-linux.desktop      migrated desktop/session programs
packages.x86_64-linux.environment  complete installed package set
checks.x86_64-linux.*              package builds and smoke tests
devShells.x86_64-linux.*           development environments where useful
```

Use nixpkgs packages directly where possible. Write derivations for forks,
unreleased versions, or software absent from nixpkgs.

## Source boundaries

`~/env` has no discoverable Git root: `cfg` supplies the home repository's Git
directory explicitly. Do not pass `~/env` directly to Nix as a flake path. That
can copy the entire directory, including `penrose/target`, into the store.
Filtering a derivation's source does not prevent this initial copy. See
[Nix's local-source documentation](https://nix.dev/tutorials/working-with-local-files.html).

The `env-nix` helper must first create a temporary snapshot outside `~/env`,
using a stable source directory name and an explicit allowlist: `flake.nix`,
`flake.lock`, package expressions, and their required source files. Never
snapshot the whole home directory. Exclude Git metadata, build outputs,
credentials, and unrelated files. Preserve internal source links without
dereferencing them; reject links that escape the selected source.

Normal builds and refreshes export selected files from one recorded home-repo
commit, defaulting to `HEAD`. Report relevant working-tree differences without
including them. An explicit `--working-tree` mode copies selected working files
for testing expression changes; additional untracked files must be named
explicitly. Neither mode stages or commits files.

Local checkout overrides snapshot tracked working files, including unstaged edits
and deletions, plus explicitly selected untracked files. Submodules are separate
inputs, not recursively copied working directories. Report the checkout revision,
dirty state, and added files. Unrelated changes and build outputs must not affect
a package's derivation. Use package-specific source selections where several
packages share a repository.

Penrose receives only its configuration sources, Cargo manifests and lock, and
required assets from the home-repo snapshot. Its fork is a separately pinned
input placed at the path expected by the Cargo dependency. Do not use the entire
home repository as its source.

## Source pins and local development

Each independently replaceable custom checkout gets a named non-flake input:

```nix
inputs = {
  nixpkgs.url = "github:NixOS/nixpkgs/nixos-unstable";
  ghostty-src = {
    url = "github:ghostty-org/ghostty";
    flake = false;
  };
};
```

The package receives `ghostty-src` as its source; `flake.lock` records the
default revision and hash. River and wlroots have separate inputs but remain a
tested, matched package set.

Extend `env-nix` as part of the foundation:

```text
env-nix build ghostty                  # committed recipe and locked source
env-nix build --working-tree ghostty   # selected working recipe and lock
env-nix build-local ghostty            # committed recipe, local source snapshot
env-nix shell-local ghostty            # shell containing that local build
env-nix update ghostty-src             # explicit lock update for review
env-nix activate-local waynav          # rooted local build and recoverable link
```

Package-to-checkout mappings belong in the helper or a small data table, not in
environment variables consumed during evaluation. Allow `--working-tree` with
local builds when the patch also requires recipe changes.

Normal builds use `--no-update-lock-file` and reject an incomplete lock.
`update` runs `nix flake update` on a working-tree snapshot and copies only the
resulting lock back for review, provided the original file has not changed in
the meantime. Version changes may also require edits to package expressions.

A local build first validates the base lock without overrides. It then passes
`--override-input <name>-src path:<selected-source-snapshot>` and
`--no-write-lock-file`. Only the requested inputs may differ in the effective
lock; reject unrelated changes. Keep that effective lock with the build record.
The repository lock remains unchanged. Use `build` to return to pinned sources.
These flags have different purposes: see the
[`nix build` reference](https://nix.dev/manual/nix/2.28/command-ref/new-cli/nix3-build.html).

Source-only overrides support patches compatible with the existing dependencies
and build recipe. Dependency changes can require new vendor hashes; other changes
can invalidate inherited patches or require a different compiler. For these,
use explicit working-tree recipe overrides and review source pins, recipes, and
hashes together. Do not disable hash checking or sandboxing. For example, Go's
`vendorHash` pins a separate dependency output; see the
[Go packaging documentation](https://github.com/NixOS/nixpkgs/blob/master/doc/languages-frameworks/go.section.md).

`shell-local` uses the same snapshot and override with `nix shell`. Verify the
resolved executable: shell startup files and absolute launch paths can select a
different installation. Check the patch's behavior, not just its version string.

The flake lock is authoritative for installed versions. Nix builds do not require
source submodules. Keep checkouts only for development; clone a repository when
a local patch is needed. Retained submodule pins are development checkout defaults
and may intentionally differ. Before a local build,
report both revisions. To start a patch from the installed revision, create a
separate worktree at that revision; never reset an existing checkout. Build from
that worktree with `build-local <package> --checkout <path>`. Promoting a patch
updates the flake input explicitly. Updating a submodule pin is a separate
human-reviewed change, not a refresh side effect.

## Installation and activation

Build the aggregate `environment` and install its checked store path into one
named profile:

```sh
nix-env --profile "$HOME/.local/state/nix/profiles/env" --set "$checked_output"
```

`checked_output` is the absolute store path built and tested during this refresh.
Use `nix-env --set` only to switch the whole environment, never to select package
versions. Do not mix `nix profile` operations into this profile. If an existing
profile uses that format, preserve it for recovery and initialize a separate
profile before switching the configured path. This activates the checked output
without reevaluating a mutable flake. See the
[`nix-env --set` reference](https://nix.dev/manual/nix/2.24/command-ref/nix-env/set).

Use this PATH order in interactive shells and login sessions:

```text
~/env/bin
~/.local/bin
~/.local/state/nix/profiles/env/bin
system paths
language-specific paths during the transition
```

Do not generate or replace tracked personal scripts. Remove a third-party binary
or source-tree link only after its replacement and callers have been verified.

Each migrated package must identify its executable callers, data discovery paths,
service units, and restart requirements. Setup scripts own Debian integration:

* Add the profile's `share` to session `XDG_DATA_DIRS`, preserving system
  entries. Verify desktop-file and D-Bus discovery in the actual session.
* Explicitly link required user-systemd units from the profile, reload the user
  manager, and enable or restart the intended services. Record replaced units and
  links. PATH changes alone do not replace darkman's installation in `/usr`;
  inspect the service's executable and competing D-Bus activation entries.
* Keep GDM entries pointing at stable repository launchers. Launchers use the
  configured PATH or an explicit managed profile path. River's hard-coded
  `~/.local/bin/river` must change before that link can be retired.
* Import required session environment into the user service and D-Bus activation
  environments. Verify discovery and process executable paths after a fresh login.

For a persistent local override, create a Nix output link under
`~/.local/state/env-nix/local/<name>` to root the build, then replace only its
designated untracked executable link. Services using absolute executable paths
need a corresponding unit override and restart. Removing an executable override
restores profile lookup only for callers using PATH; restore explicit links and
unit overrides separately.

## Refresh and recovery

`023-setup-nix.sh` explicitly installs and configures multi-user Nix, then invokes
the same build and activation procedure as refresh. Document the installer or
Debian package used and the supported Nix version. Setup must be idempotent;
`freshen` must not silently install a daemon or change system users.

Normal refresh:

1. Update Debian-owned packages.
2. Snapshot one home-repo commit and validate its lock without allowing updates.
3. Build checks and the aggregate from that same snapshot. Keep the aggregate
   rooted with an explicit output link and run smoke tests against that output.
4. Record the previous profile generation, executable links, unit overrides, and
   affected service state; activate the exact checked output.
5. Reload and restart affected services, verify their executables and behavior,
   then run remaining setup actions that do not reinstall migrated packages.

Do not run `nix flake update` during refresh. Dirty expressions and locks are used
only in explicitly requested working-tree builds or activations, recorded as such.
Serialize refreshes and local-override changes so recovery describes one
transition. Profile switching is atomic; service and session changes are not.

On failure, run:

```sh
nix-env --profile "$HOME/.local/state/nix/profiles/env" \
  --switch-generation "$saved_generation"
```

Then restore recorded links and unit overrides, reload the user manager, and
restart affected services. A compositor or window-manager change requires a new
login. Keep a TTY-accessible recovery procedure and the previous session entry
during the first session migration.

Keep previous Nix outputs rooted and preserve old non-Nix binaries until login
and rollback validation succeeds. A failed first installation restores the
previous PATH and launchers even if no prior Nix generation exists. Profile
rollback does not undo Debian upgrades or application data migrations.

## Cache and development builds

Multi-user Nix shares `/nix/store` across users and projects. Identical derivations
can reuse stored outputs or trusted substitutes; different source snapshots or
recipes may require separate builds.

The store caches derivation outputs, not arbitrary compiler incrementals. A
source change can rebuild an entire package unless its recipe separates reusable
dependency builds. Development shells provide pinned compilers and libraries;
ordinary project-local build directories remain useful for fast edit/build cycles.

Configure substituters centrally, initially using `cache.nixos.org` and its
published signing key. Add a private signed cache only when sharing across
machines is needed. Keep signing keys outside the home repository.

Profiles and explicit local output links retain installed builds. Collect garbage
deliberately, after pruning generations and override roots no longer needed for
recovery. A temporary `result` link is not a durable installation.

## Graphics boundary

Debian continues to own the kernel and graphics drivers. Nix-built applications
still need compatible access to the host graphics stack; linking river to its
exact wlroots closure does not solve that boundary. Tools such as
[nixGL](https://github.com/nix-community/nixGL) address this integration, but the
appropriate wrapper and driver discovery must be tested on the actual machine.

During the foundation phase, run a representative Nix-built accelerated
application under the existing session. Verify hardware rendering, driver loading,
and launching Debian applications from it. Record any wrapper, host-driver
dependency, and environment propagation required. Do not move the compositor
until that experiment has a working, repeatable result.

For river, test its matched wlroots closure, device access, Xwayland, portals,
screen sharing, and GDM launch. Remove the session's `LD_LIBRARY_PATH=~/.local/lib`
only after those tests pass. Retest graphics integration after host-driver updates.

The initial Intel-machine experiment failed without a wrapper (no GLX visual),
then reported hardware acceleration with nixGLIntel and the locked nixpkgs.
`nixgl` exposes that pinned wrapper; `graphics-check` supplies
`env-nix-graphics-check`, which checks for an accelerated renderer in the current
X11/Xwayland session. Run it from `env-nix shell graphics-check`. Keep wrapper
variables scoped to the launched application. Native Wayland rendering, inherited
environments when launching Debian applications, and GDM login remain separate
validation requirements.

## Validation

For each migrated package:

1. Build in the sandbox and run a minimal smoke test from the result.
2. Repeat the build without compilation; the same selected sources and recipe
   from another checkout must produce the same derivation and output paths.
3. Change an unrelated file or create a build artifact; the selected source and
   package derivation must remain unchanged.
4. Apply a source patch and demonstrate its behavior in the resulting executable,
   with no repository lock changes. Test dependency-changing patches through the
   explicit recipe-override workflow.
5. Verify command resolution, data discovery, and service executables from a
   fresh login, including relevant D-Bus, graphics, and portal behavior.
6. Activate an update and a local override, then restore the saved generation and
   integration state. Verify the restored running programs, not just symlinks.

Also verify that invalid locks and failed builds leave the active profile intact,
and that a failed service activation can recover from a TTY. An application that
migrates persistent data needs a separate data-recovery plan before relying on
binary rollback.

## Migration order

1. Foundation: implement source snapshots, lock rules, and exact-output activation;
   package `bat` and `joshuto`; test reuse, local overrides, and rollback. Run the
   early graphics experiment.
2. Command-line tools: move low-risk apt, Cargo, and downloaded tools in small
   groups. Wire the aggregate into `freshen`; remove superseded installers only
   after activation succeeds.
3. Custom builds: package asdcontrol, keynav, waynav, dunst, and darkman. Include
   their runtime integration; retain source checkouts only for development.
4. Session stack: package Penrose, Ghostty, and matched river/wlroots builds.
   Switch launchers only after GDM and recovery tests pass.
5. Consolidation: remove obsolete build dependencies and setup scripts. Reconsider
   Home Manager only if profile and service management justify it.
