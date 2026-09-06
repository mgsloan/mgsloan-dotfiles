# Nix migration design

## Goals

* Build user tools from pinned inputs.
* Share build results across projects and users through the machine's Nix store.
* Switch one package to a patched checkout under `~/oss` without editing pins.
* Keep the home-dir repository, `cfg`, and existing session setup working.
* Migrate incrementally. Installing Nix must not require replacing Debian or making
  the whole home directory declarative.

This design uses multi-user Nix, one flake in `~/env`, and small package modules in
`~/env/nix`. It does not initially use NixOS or Home Manager.

## Current state

Software currently comes from several independent mechanisms:

* `~/.local/bin/freshen` installs `apt-packages.md`, upgrades Debian, downloads
  `.deb` files, and refreshes Zig and Typst.
* `setup-scripts/050-installs.sh` runs network installers for Rust, Stack, pnpm,
  wasm-pack, uv, just, and the Google Cloud SDK.
* `cargo-packages.md` lists `bat` and `joshuto`, but does not install them;
  `050-xidlehook.sh` separately runs `cargo install xidlehook`.
* river/wlroots, Ghostty, darkman, asdcontrol, Penrose, dunst, keynav, and waynav
  are built from `~/oss` or `~/env` checkouts. Most `~/oss` checkouts are home-repo
  submodules, so their commits are pinned, but builds still depend on the mutable
  host toolchain and libraries.
* `~/.local/bin` mixes tracked scripts, links into working trees, downloaded
  executables, and build outputs. Its early position in `PATH` makes those local
  links useful, but also makes package provenance difficult to see.

The migration should preserve tracked personal scripts in `~/.local/bin`. Nix
only replaces third-party executables and locally built artifacts.

## Repository layout

Add the following files gradually:

```text
env/
  flake.nix
  flake.lock
  nix/
    packages.nix       # package set and overrides
    sources.nix        # names for non-flake source inputs
    checks.nix         # builds and lightweight smoke tests
```

Keep the first version small. Split out a package only when its expression is
large enough to obscure `packages.nix`; river plus wlroots is the likely first
case.

The flake exports:

```text
packages.x86_64-linux.<name>  individually buildable packages
packages.x86_64-linux.tools   all ordinary command-line tools
packages.x86_64-linux.desktop source-built desktop/session programs
checks.x86_64-linux.*         the important package builds
devShells.x86_64-linux.*      development environments where useful
```

Use nixpkgs packages directly where possible. Write derivations only for forks,
unreleased versions, or software absent from nixpkgs.

## Source pinning and local overrides

Every custom source gets a named, non-flake input in `flake.nix`. For example:

```nix
inputs = {
  nixpkgs.url = "github:NixOS/nixpkgs/nixos-unstable";

  ghostty-src = {
    url = "github:ghostty-org/ghostty";
    flake = false;
  };
};
```

The corresponding package receives `ghostty-src` as `src`. `flake.lock` records
the normal revision and content hash. Updating is explicit:

```sh
nix flake update ghostty-src
nix build ~/env#ghostty
```

To test a patched checkout, replace only that input for one invocation:

```sh
nix build ~/env#ghostty \
  --override-input ghostty-src "path:$HOME/oss/ghostty"
./result/bin/ghostty --version
```

This has the desired properties:

* the checked-in lock remains the default;
* no Nix expression is edited;
* the local tree, including uncommitted files, becomes a content-addressed Nix
  input;
* dependencies and build instructions remain identical to the pinned build;
* switching back means omitting `--override-input`.

Use one source input per independently replaceable checkout. Do not make all of
`~/oss` one input: changing an unrelated checkout would invalidate every package.
For a package assembled from multiple repositories, such as river and wlroots,
give each repository its own input so either can be overridden independently.

Add a concise `env-nix` helper to `~/env/bin` after the expressions settle. Its
interface should expand package names to their conventional checkout paths:

```text
env-nix build ghostty             # locked source
env-nix build-local ghostty       # ~/oss/ghostty
env-nix shell-local ghostty       # shell containing that local build
```

The mapping belongs in the helper or a small data table, not in environment
variables consumed during evaluation. Impure environment-selected sources make
it too easy to build something other than the lock file claims.

`shell-local` should run `nix shell` with the same override and is the convenient
way to try a patch without changing the persistent installation. If a local build
needs to replace the login-session binary, explicitly build an output link under
`~/.local/state/env-nix/local/<name>` and point the existing `~/.local/bin` link
at its `bin` entry. That link is also a garbage-collection root. Removing the
override link restores the profile-provided binary on `PATH`.

## Installation and PATH ownership

Install packages intended to be present in every interactive session into one
named Nix profile, for example:

```sh
nix profile install --profile "$HOME/.local/state/nix/profiles/env" ~/env#tools
```

Add its `bin` directory to `PATH` after `~/env/bin` and `~/.local/bin`, but before
language-specific bins. The order intentionally preserves personal commands and
temporary local overrides:

```text
~/env/bin
~/.local/bin
~/.local/state/nix/profiles/env/bin
system paths
language-specific paths during the transition
```

Use the explicit named profile instead of an assortment of `nix profile install`
commands against the default profile. It gives this repository one replaceable
unit and keeps unrelated experiments out of the managed command set.

Eventually the flake should expose one aggregate environment and `freshen` should
run a single profile upgrade/build operation. Until then, it can invoke a separate
`freshen-nix` script after apt succeeds.

Do not generate or replace tracked files in `~/.local/bin`. Remove a third-party
file or source-tree symlink only after its Nix replacement is installed and
verified. Keep `cfg`, `freshen`, `backup`, `gab`, hardware helpers, and similar
personal scripts where they are.

## Cache model

Install Nix in multi-user/daemon mode. All derivations then use the machine-wide
`/nix/store`, so identical inputs produce one stored result regardless of which
checkout, shell, or user requested the build. This is the primary global shared
build cache; no per-project cache configuration is needed.

The cache stores derivation outputs, not arbitrary compiler incrementals. A Cargo
derivation will reuse its completed dependency/build outputs when its inputs are
identical, but Nix does not turn mutable `target/` directories into a global Cargo
incremental cache. Development shells should keep ordinary project-local build
directories for fast edit/build cycles. Nix provides their pinned compilers and
native dependencies.

Configure trusted public substituters centrally in `/etc/nix/nix.conf`. Start
with the cache supplied by the chosen nixpkgs channel. If builds need to be shared
between machines, add a private binary cache later (Cachix, Attic, or a signed
`file://`/HTTP store). Require signatures and keep cache signing keys outside the
home repository.

Profile generations and explicit local output links keep installed builds alive.
Run garbage collection deliberately; an unreferenced `result` link removed by a
later build is not a durable installation.

## Package boundaries

### Move to nixpkgs first

Start with leaf command-line tools that are already packaged and have little
desktop integration:

* `bat`, `joshuto`, `xidlehook`, `lychee`, `just`, `ripgrep`, `shellcheck`,
  `pandoc`, `qpdf`, and similar utilities from `apt-packages.md`;
* language launchers such as `uv`, where the nixpkgs version is acceptable;
* build tools used only inside a Nix derivation.

Remove each corresponding apt/Cargo/download install only after the Nix version
is active. `cargo-packages.md` can become a migration checklist and then be
deleted when empty.

### Package custom builds next

Suggested order, from least coupled to most coupled:

1. asdcontrol, keynav, waynav, and dunst;
2. darkman;
3. Penrose's X11 binary;
4. Ghostty;
5. wlroots, river, and Penrose's river backend.

Each package should build in the sandbox with only declared inputs. Avoid
installing its build dependencies globally once its Nix derivation exists.
Penrose should use the home repository source plus its separately pinned Penrose
fork, rather than copying a prebuilt binary to `~/.local/bin`.

River and wlroots must remain a matched package set. The current session script's
`LD_LIBRARY_PATH=~/.local/lib` is a symptom of the prefix install; Nix should patch
or wrap river with its exact wlroots closure, after which that loader-path setting
can be removed.

### Keep outside Nix initially

Keep these under Debian or their vendor installer until there is a concrete
reason to migrate them:

* kernel, drivers, udev rules, groups, sysctl, GRUB, and system services;
* GDM session files and user-systemd enablement;
* large desktop/vendor applications such as Chrome, Zoom, Spotify, Keybase,
  Google Cloud SDK, and AppImages;
* language version managers needed for unrelated development repositories.

Nix can manage some of these later, but mixing that work into the first migration
adds desktop integration and state-management questions unrelated to deterministic
source builds.

## Setup and refresh flow

Add a numbered setup script after the home repository is restored:

```text
023-install-nix.sh       install multi-user Nix if absent
024-configure-nix.sh     configure flakes, substituters, and trusted users
025-install-nix-tools.sh build checks, then install the named profile
```

The installation script should be idempotent and pin or document the installer
used. Because it changes `/nix`, system users, and daemon configuration, it should
remain an explicit setup action rather than something `freshen` silently performs.

The normal refresh sequence becomes:

1. update apt metadata and Debian-owned packages;
2. run `nix flake check ~/env` or build the relevant aggregate package;
3. update the named profile only if the build succeeds;
4. run the remaining stateful setup actions.

Do not run `nix flake update` from `freshen`. Refreshing software should realize
the committed lock file; changing versions is a separate, reviewable operation.

## Validation

For every migrated package:

1. `nix build ~/env#<name>` succeeds in the sandbox.
2. A minimal version/help command succeeds from the result.
3. Repeating the build does no compilation.
4. Building from another checkout resolves to the same store path.
5. `--override-input <name>-src path:$HOME/oss/<name>` changes the store path and
   runs the patched binary without changing `flake.lock`.
6. The program works from a fresh login, not only from an interactive shell.

For session-critical packages, keep the old working binary until a login test has
succeeded. A Nix build succeeding does not verify D-Bus activation, graphics,
desktop files, portals, or the GDM environment.

## Migration phases

### Phase 1: foundation

Install multi-user Nix, add `flake.nix`/`flake.lock`, configure PATH, and package
the two entries in `cargo-packages.md`. Confirm store reuse and local overrides.

### Phase 2: command-line tools

Move low-risk packages out of apt and download scripts in small groups. Add an
aggregate `tools` package and call it from `freshen`.

### Phase 3: source checkouts

Package the simpler `~/oss` submodules, replacing direct `make`, `cargo install`,
and source-tree symlinks. Preserve submodules for convenient hacking even after
the flake lock becomes the installation pin.

### Phase 4: graphical session stack

Package Penrose, Ghostty, wlroots, and river. Update session launchers only after
their closures and runtime integration have been tested from GDM.

### Phase 5: consolidation

Remove obsolete build dependencies from `apt-packages.md`, retire superseded
setup scripts, and decide whether Home Manager would provide enough value for
profile and service declarations. It is optional; adopting it is not required for
deterministic builds or the shared store.

## Decisions

* Use a flake lock as the software-version manifest.
* Use multi-user Nix for a machine-wide store and build daemon.
* Use named non-flake inputs plus `--override-input path:...` for patched sources.
* Keep personal scripts in the home repo and put Nix behind their PATH precedence.
* Keep operating-system configuration in the existing Debian setup scripts.
* Do not introduce Home Manager in the initial migration.
* Do not update locks implicitly during routine refreshes.
