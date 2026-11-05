# Debian-native packaging adoption plan

Status: full Debian port implemented on 2026-11-05. All 25 public/private
catalog packages are built or retained from upstream Debian releases. Host
installation and native activation are validated. A fresh login/reboot remains
to be checked. Nix is retained by request.

## Implemented workflow

The maintained interface is [debian/README.md](../debian/README.md). Public APT
requirements are restored in `apt-packages.md`; `debian/packages.toml` declares
22 local packages. Private selections and vendor applications remain in `~/ep`.
`freshen` installs both APT lists, builds and installs the catalogs, refreshes
private tools, then selects Debian only after validating the installed set.

`bin/env-debian` generates independent gbp repositories and clean sbuild/unshare
builds. It supports uncommitted local checkouts, retains their selection across
refreshes, vendors language dependencies, records distinct versions and source
artifacts, and uses uscan for explicit update discovery. Catalog patches retain
the River keystroke histograms and private font discovery for Rofi and Dunst.
Debian supplies wlroots; versioned native Zig compiler packages supply River and
Ghostty. Portable upstream releases and vendor `.deb` files are documented
exceptions to local source builds.

The session selector removes inherited Nix graphics settings and search paths.
Activation saves Nix launch links and service environment for rollback. All Nix
flakes, scripts, profiles, and store outputs remain available; removal is out of
scope for this migration. The existing Penrose rebuild workflow remains in place. Independently managed
Nix launchers, including Zathura, remain outside the catalog migration.

Validation so far: all 121 environment tests pass with `/usr/bin/python3`;
`env-nix check --working-tree` also passes for the retained configuration. Native
GLX/EGL report accelerated Intel rendering when Nix variables are cleared.
Spotifast creates its window using Debian Mesa in an isolated Xvfb session with
an empty profile. Ghostty passes required lintian checks and renders a terminal and launches a child without Nix
paths/settings. Headless River with Debian wlroots serves Wayland and Xwayland
clients. The active Waynav checkout, including its two extra pointer/overlay
fixes, builds without changing its source or index. Logs and build records are retained in
`~/.local/state/debian-packages/`.

A real login/reboot check remains necessary for compositor startup and the
original Penrose M-p launch path. Existing desktop processes are left running.

## Pilot results

The workflow works for Keynav. Builds used sbuild 0.91.10, git-buildpackage
0.9.43, and a forky/amd64 root created by mmdebstrap's unshare mode. The host
already had usable subordinate UID/GID ranges. No host development libraries
were needed for package compilation.

Because sudo requires an interactive password and the tools were not installed,
the trial used downloaded Debian tool packages under
`~/.local/state/debian-packages/bootstrap`. One temporary sbuild module was
adjusted to locate its unpacked helper; this is bootstrap plumbing, not an
application patch. Normal installation through `freshen` removes that requirement.
The source archive checksums were verified against Debian's signed source index.

| Build | Result |
| --- | --- |
| `0.20180421~git6505bd0d-3.1` | Unmodified Debian source package built successfully |
| `0.20180421~git6505bd0d-3.1+local1` | Existing local crash fix imported with `gbp pq`, exported, and built |
| `0.20190518~git8012f9f-0+local2` | Newer fork snapshot discovered with uscan, imported, rebased, built, and tagged |
| `0.20180421~git6505bd0d-3.1+localsnapshot1` | Uncommitted source changes exported and built without changing the checkout or index |

The newer snapshot is the same upstream commit selected by the existing Nix
recipe: `8012f9f1a557d09852e63cea7e524cbf60b70343`. The package additionally retains
Debian patches, including its Hyper modifier mapping. It is not intended to be
byte-identical to the Nix output.

Runtime checks installed these packages in a separate disposable Debian root,
with no Nix graphics variables. They verified installation, upgrade, explicit
downgrade, reinstall, and switching back from the uncommitted snapshot. Xvfb
checks started Keynav and used its keyboard navigation to move the pointer to
the expected screen position. ELF inspection confirmed Debian's loader,
resolved Debian libraries, and no Nix or `/usr/local/lib` runtime search paths.
The original crash itself was not reproduced as a regression test.

Updating the old packaging required three patch-conflict resolutions, an
explicit Xrandr build dependency, bypassing upstream's install target because
it ignores DESTDIR, and leaving binary stripping to debhelper. The obsolete libXtst patch was
dropped; `gbp pq rebase` automatically dropped the crash fix once upstream
contained it. Clean builds took roughly 40–55 seconds here, including setup and
checks. The cached base root is about 466 MiB.

The final package and uncommitted snapshot passed lintian with warnings. Existing
warnings concern an old FSF address and an unsupported upstream metadata field;
the snapshot also warns about its generated patch. Early local builds using
`UNRELEASED` triggered a lintian error, fixed by targeting forky. Set
`$lintian_require_success = 1` in sbuild configuration: its default otherwise
allows a successful build exit despite lintian errors.

Trial state:

- Development checkout: `~/oss/keynav`.
- Independent packaging repository: `~/oss/keynav-debian`; current successful tag
  `debian/0.20190518_git8012f9f-0+local2`. Its commits and tag are local.
- Builds, complete source artifacts, logs, and checksums:
  `~/.local/state/debian-packages/builds/keynav/<version>/forky-amd64/`.
- Machine-readable results, including intermediate builds:
  `~/.local/state/debian-packages/pilot-results.json`.
- Recovery record: `~/.local/state/debian-packages/pilot-before.json`.
- Uncommitted snapshot example: `~/.local/state/debian-packages/snapshot-checkout`;
  its diff remains in place and is also saved as `snapshot-before.diff`.
- Runtime evidence: `runtime-test.log` and `runtime-test-snapshot.log` in that
  state directory. The disposable runtime root was removed after testing.

At the end of the pilot, the live desktop still selected Nix Keynav. The full
port above supersedes the pilot-only continuation instructions below.

### Continue after freshen

The retained `sbuild-native.conf` selects unshare, caches the base root, builds
source artifacts, and makes lintian errors fatal. Unlike the temporary bootstrap
configuration, it uses the normal installed Debian tools.

```sh
cd ~/oss/keynav-debian
package_state="$HOME/.local/state/debian-packages"
export SBUILD_CONFIG="$package_state/sbuild-native.conf"
build_directory="$package_state/builds/keynav/$(dpkg-parsechangelog -SVersion)/forky-amd64"
mkdir -p "$build_directory"
gbp buildpackage --git-export-dir="$package_state/exports/manual" \
  --git-tarball-dir="$package_state/sources/keynav" \
  --git-builder="sbuild --chroot-mode=unshare -d forky --no-clean-source --source --arch-all --build-dir='$build_directory'"
```

Use a new changelog version before changing source or packaging. To install the
already tested package on the host, first inspect the transaction:

```sh
package="$HOME/.local/state/debian-packages/builds/keynav/0.20190518~git8012f9f-0+local2/forky-amd64/keynav_0.20190518~git8012f9f-0+local2_amd64.deb"
apt-get --simulate install "$package"
sudo apt-get install "$package"
/usr/bin/keynav --version
```

Explicit `/usr/bin/keynav` selects the Debian build while the Nix profile remains
earlier on PATH. Select the Debian executable in the session only after its host
check, then retire the corresponding Nix requirement during migration. No host
package hold has been added.

## Goal

Use Debian source packages, git-buildpackage (`gbp`), and sbuild to install
upstream releases, build patched checkouts, and switch between retained builds.
Installed applications should use Debian's runtime libraries and desktop
integration. Keep fast project-local development builds available.

Adopt this incrementally. Keep Nix and its working outputs available until each
replacement has passed installation and recovery checks. Ordinary applications
available from Debian should use APT without maintaining a local source package.

## Architecture

| Responsibility | Tool or convention |
| --- | --- |
| Upstream release discovery | `debian/watch`, `uscan` |
| Upstream imports and packaging history | `gbp import-orig`, upstream tags, packaging branch |
| Local patches | `debian/patches`, maintained with `gbp pq` |
| Build dependencies | `Build-Depends` in `debian/control` |
| Clean package builds | sbuild with an explicitly selected unshare backend |
| Runtime dependencies | debhelper and `${shlibs:Depends}` / `${misc:Depends}`, plus explicit dependencies |
| Installation and version selection | APT and dpkg |
| Recovery | Retained `.deb` files, source artifacts, and build records |
| Interactive development | Project build commands with separately provisioned development dependencies |

Choose unshare explicitly rather than relying on a version-dependent default.
Other backends are alternatives if the host cannot support it, not initial
requirements. See [sbuild](https://manpages.debian.org/unstable/sbuild/sbuild.1.en.html).

A `.deb` does not by itself ensure native linking. Build against Debian
dependencies, inspect the output, and declare libraries loaded dynamically:
`dpkg-shlibdeps` detects linked libraries, not every plugin or `dlopen` dependency.
See [dpkg-shlibdeps](https://manpages.debian.org/unstable/dpkg-dev/dpkg-shlibdeps.1.en.html).

## 1. Inventory and preserve the working system

- [x] Inventory public requirements from `flake.nix`, `nix/packages.nix`,
  `apt-packages.md`, setup scripts, and tracked callers. Inventory private
  requirements separately in `~/ep`; do not copy the private list here.
- [x] Classify each requirement: Debian package, vendor package, locally patched
  application, project toolchain, or development-only dependency.
- [x] Record the selected executable, package/version, local checkout, patches,
  resource paths, and service/D-Bus launch paths for each migration candidate.
  Include Nix overrides and older installations under `~/.local-old`.
- [x] Record and retain the active Nix profile generation, rooted local outputs,
  and the commands needed to restore them. Do not garbage-collect recovery builds.
- [x] Inspect actual APT sources and package policy before selecting a build suite.
  This host identifies as forky/sid, but its Debian APT sources currently follow
  testing. Start with a forky build root if that still matches the host; do not
  silently introduce unstable runtime dependencies.
- [x] Record existing user overrides, desktop entries, portal configuration, and
  session environment settings. These can shadow newly installed package files.

Initial migration groups:

| Group | Approach |
| --- | --- |
| Existing Debian CLI applications | Restore direct APT installation; no custom packaging |
| keynav, waynav, asdcontrol | Small native-build candidates; inspect available Debian packaging first |
| dunst, rofi | Reuse Debian packaging where possible; preserve the current font changes |
| darkman | Migrate with its user services, D-Bus activation, and Settings portal |
| Ghostty, Spotifast | Prove native graphics and assess compiler/language dependency availability |
| River and wlroots | Migrate together only after validating their dependency and toolchain versions |
| errlog-filter and same-spot tools | Package source, scripts, data/model files, and runtime requirements |
| Penrose | Retain its current rebuild/restart workflow initially |

These are sequencing choices, not assertions that Debian currently packages every
application or provides every required version.

## 2. Bootstrap the packaging tools

- [x] Add public workflow tools to `apt-packages.md`: `git-buildpackage`,
  `sbuild`, `mmdebstrap`, `devscripts`, `debhelper`, `dpkg-dev`, `lintian`,
  `uidmap`, and any additional bootstrap requirements identified by the installed
  sbuild documentation. `iproute2`, `pristine-tar`, and `zstd` are also listed.
- [x] Add matching Debian source repositories where needed for source retrieval.
  Do not enable unrelated binary suites to satisfy a build accidentally.
- [x] Configure unshare and required lintian success through
  `debian/sbuild.conf`, passed explicitly as `SBUILD_CONFIG`. Keep artifacts
  outside the source tree; the helper selects forky/amd64.
- [x] Validate subordinate UID/GID mappings and unprivileged namespace support.
  Create or validate the cached build root using the installed sbuild/mmdebstrap
  instructions. Root may be needed for initial host setup, not each build.
- [x] Rebuild an unchanged small Debian source package in the clean root.
  Confirm its declared dependencies suffice without host development libraries.

Use installed manuals to confirm bootstrap details and configuration paths;
Debian testing changes over time. Retain logs and `.buildinfo` files. A moving
suite and cached root are not an exact dependency lock; record root refreshes
and dependency versions. Use a Debian archive snapshot only when exact rebuilds
justify it.

## 3. Establish repository and artifact conventions

Use independent packaging repositories so imported upstream trees do not become
files in the home-directory dotfiles repository:

```text
~/oss/foo/                         existing upstream/development checkout
~/oss/foo-debian/                  independent gbp repository, including source
~/.local/state/debian-packages/    retained source, binary artifacts, and logs
~/env/                            public setup and packaging-repository references
~/ep/                             private package selection and references
```

The packaging repository owns `debian/`, imported upstream history, and patch
history. It is a full source repository, not just a metadata directory. If an
existing checkout already supports this layout, use it directly instead of
creating a second repository. Add a public package inventory under `~/env` only
when needed; record packaging repository URLs and selected refs there.

- [ ] Reuse the Debian maintainer's repository or import its source package with
  `gbp import-dsc`. Inspect repository conventions before choosing branch names.
  Ubuntu packaging is a starting point only after checking Debian compatibility.
- [x] For new packaging, prefer debhelper and `3.0 (quilt)` for third-party
  upstream sources. Provide `control`, `rules`, `changelog`, `copyright`,
  `source/format`, install rules, and `watch` where applicable.
- [x] Record branches/tags in `debian/gbp.conf`. Use upstream and packaging
  branches; retain pristine-tar data when needed to reproduce orig tarballs.
- [x] Store artifacts by source package, Debian version, suite, and architecture.
  Keep all binary packages from a build, source `.dsc` and referenced tarballs,
  `.changes`, `.buildinfo`, logs, checksums, and packaging/upstream commit IDs.
- [x] Reserve a new version for changed source or packaging. Never replace an
  artifact with different bytes under the same Debian version.

Use ordinary Git inside these independent repositories. Continue using `cfg` for
the home repository, where staging and selection of commit contents remain human
activities. Do not automate staging or commit unrelated working-tree changes.

See the [gbp manual](https://honk.sigxcpu.org/projects/git-buildpackage/manual-html/gbp.html).

## 4. Prove the complete workflow with one small application

Start with keynav if its packaging and dependencies are straightforward; otherwise
use another small application from the inventory. Defer desktop activation until
the unchanged package builds successfully.

- [x] Build an unchanged upstream release in sbuild. Check package contents and
  lintian findings before installation.
- [x] Import an actual local patch through `gbp pq`, build a distinct version,
  verify the exported change, and exercise the built package. Prefer an existing
  useful patch.
- [x] Install the patched package, restore the retained upstream package, and
  reinstall the patched package in a disposable Debian runtime root. Record the
  exact package set and commands.
- [x] Import a newer upstream snapshot, rebase the patch queue, and build again.
- [x] Build each version in a fresh root extracted from the cached base image.
- [ ] Install and select the package in the actual desktop session.

Do not recreate a package manager. After this pilot, add a thin helper only for
repeated artifact selection, build invocation, or installation bookkeeping that
existing commands do not handle adequately.

### Release and patch workflow

Use `uscan --report --no-download` for discovery. Import a selected release through
`gbp import-orig --uscan`; record the resulting version and upstream identity.
Pass `--upstream-version=<selected-version>` to avoid an interactive version prompt.
Adapt `debian/watch` to the version of devscripts in use rather than assuming
one watch-file syntax. See [uscan](https://manpages.debian.org/unstable/devscripts/uscan.1.en.html).

For patches against the packaged upstream version:

```sh
gbp pq import
# edit or cherry-pick selected development commits on the patch-queue branch
gbp pq export
# review the exported patches and update debian/changelog
```

When updating upstream, prepare the old patch queue before importing the release,
then rebase it with `gbp pq rebase` and export the result. Resolve conflicts and
drop patches incorporated upstream. The durable patch history lives on the
packaging branch; the patch-queue branch may be regenerated. See
[gbp patch updates](https://honk.sigxcpu.org/projects/git-buildpackage/manual-html/gbp.patches.newupstream.html).

From a reviewed packaging revision, the pilot should validate this command shape:

```sh
gbp buildpackage --git-export-dir=../build-area \
  --git-builder='sbuild --chroot-mode=unshare -d forky --no-clean-source'
```

Configure a separate output directory before routine use. `--no-clean-source`
is appropriate here only because gbp exports a clean source tree; it avoids
requiring build dependencies on the host for the initial clean operation.
Package building and cleaning inside the build root must still work normally.
See [gbp buildpackage](https://honk.sigxcpu.org/projects/git-buildpackage/manual-html/man.gbp.buildpackage.html).

### Local checkouts and development dependencies

The first supported path is a selected commit from `~/oss/foo`, cherry-picked
onto the package's patch queue. For substantially different upstream revisions,
import a separately versioned upstream snapshot instead of treating the entire
revision difference as one patch.

For uncommitted work in a Debianized checkout, the pilot validated gbp's existing
working-copy export and dpkg-source's automatic patch recording:

```sh
gbp buildpackage --git-export=WC --git-export-dir=../snapshot-build-area \
  --git-builder='sbuild --chroot-mode=unshare -d forky --no-clean-source --source --arch-all --dpkg-source-opt=--auto-commit'
```

Assign a unique snapshot version first. Review the exported file scope, including
untracked files; use a disposable checkout when selecting changes from another
development tree. Record its diff and source identity. In the trial, the raw
source change became a patch only in the exported package source; the original
checkout's diff and index remained byte-for-byte unchanged. `--git-ignore-new`
alone does not select working-copy contents; `--git-export=WC` does.

sbuild provisions the package build, not the editor or the host shell. For rapid
edit/build cycles, keep the project's ordinary build directory and document one
of these development paths per project:

- Install host development dependencies from `debian/control`, using
  `mk-build-deps` where useful. Archive `apt-get build-dep foo` describes the
  archive's source version, which may differ from the local packaging.
- Use a persistent matching Debian development environment if host dependency
  versions conflict. Keep sbuild as the clean package-validation path.

Do not duplicate project development libraries into the public application list.
Package-specific declarations belong with the project/packaging; private user
tools remain in `~/ep`.

## 5. Make version selection and recovery explicit

- [ ] Preserve Debian epochs and revisions. For an existing `1.4.2-1` package,
  use versions such as `1.4.2-1+local1`, incrementing the local suffix for changes.
  For new packaging use a consistent initial local revision. Verify ordering with
  `dpkg --compare-versions`; do not introduce epochs to force selection.
- [x] Start with direct installation of exact retained artifact paths through
  `apt install ./package_version_arch.deb`. Inspect the transaction first and
  install related binary packages together when their versions must match.
- [ ] Decide per package whether newer archive versions may replace local builds.
  Use an explicit hold for a patch that must remain installed, document why, and
  review it during updates. Restore the intended hold state after switching.
- [ ] Restore an older build by explicitly selecting its retained `.deb` files;
  inspect dependency changes and use downgrade options only for that transaction
  if necessary. Do not enable global automatic downgrades.
- [x] Keep at least the active and previous validated builds, plus their source
  artifacts. Retain any additional dependency packages required for recovery.

APT version policy is distinct from artifact retention; see
[apt_preferences](https://manpages.debian.org/unstable/apt/apt_preferences.5.en.html).
Reinstalling an old application does not restore its former dependency stack,
configuration, or application data. If dependencies have changed incompatibly,
rebuild the older source against the current suite and test it before switching.

Add a signed local repository managed by reprepro only when dependency resolution
across several local packages makes direct artifact installation cumbersome.
Keep historical artifacts separately and verify repository retention behavior;
do not assume a repository keeps every version of one package automatically.

## 6. Migrate applications and desktop integration

Migrate one application at a time, following the inventory groups. Translate
existing patches and install behavior rather than silently dropping them.

- [x] Package executables and data under `/usr`, desktop entries under
  `/usr/share/applications`, and user units/D-Bus files in their distribution
  locations. Use debhelper integrations for generated dependencies and triggers.
- [x] Check `${shlibs:Depends}` and explicitly declare runtime executables,
  plugins, Python modules, model data, and dynamically loaded libraries.
- [ ] Inspect ELF interpreter and RPATH/RUNPATH entries. Confirm no required
  `/nix/store` paths remain in binaries, scripts, desktop entries, or units.
- [ ] Check command resolution and resource discovery. Carefully retire only
  known obsolete `~/.local/bin` overrides and user desktop/service links.
  A package in `/usr` can remain shadowed by the Nix profile on PATH.
- [ ] Replace links owned by `setup/050-nix-services.py` as their packages migrate;
  preserve unrelated user overrides. Reload user systemd/D-Bus configuration
  and restart the relevant applications or services deliberately.
- [ ] Test launch from Penrose M-p, a terminal, a desktop entry, and D-Bus/service
  activation where applicable. Verify graphics without inherited Nix variables.
- [ ] Check dunst notifications, darkman appearance/Settings portal, font
  behavior, desktop resources, and user-service activation as applicable.

For Ghostty and Spotifast, validate native GPU rendering and the original M-p
failure case. Test Ghostty's child environment as well as its own rendering.
Do not remove remaining nixGL wrappers until their applications have migrated.

For River, first determine whether the chosen Debian suite supplies the required
wlroots and Zig versions. Build missing dependencies as Debian packages, provide
them to sbuild with `--extra-package` or a local repository, and install any
required runtime packages together. Keep shared-library ABI/package names
consistent with Debian. Do not restore a session-wide `LD_LIBRARY_PATH` to hide
dependency gaps. Test nested first, retain a TTY recovery path, and perform the
real compositor switch at a later login.

For Rust, Zig, or Node projects, enumerate compiler versions and transitive
source dependencies. `Build-Depends` does not automatically satisfy Cargo or
other language lockfiles. Choose Debian-provided dependencies or a documented,
checksummed vendored source strategy; verify builds do not need undeclared
network downloads. Keep difficult backport chains as explicit exceptions while
the simpler migrations proceed.

## 7. Integrate updates; retain Nix until separately authorized

- [x] Keep APT responsible for ordinary updates. `freshen` rebuilds changed
  catalog recipes and selected checkouts; `env-debian updates` reports upstream
  discovery separately. Source-pin updates and patch rebases remain explicit.
- [x] Add only the repeated commands demonstrated by the pilot. Preserve logs,
  source selection, and artifact identity; serialize operations on one packaging
  repository. Never reset a development checkout as an update mechanism.
- [x] Update `README.md`, `setup/readme.md`, package inventories, session setup,
  and service setup to describe the adopted workflow.
- [ ] Deferred by request: retire `setup/023-setup-nix.sh`, Nix refresh/service activation, profile PATH
  entries, and Nix-specific session/resource plumbing only after their remaining
  callers and package requirements have replacements. Audit `~/ep` separately.
- [ ] Perform a fresh login and reboot with the Debian replacements selected.
  Check graphics, sound, portals, notifications, window-manager startup, and
  recovery. Keep working Nix generations until these checks pass.
- [ ] Deferred by request: remove Nix installation/store data in a separate final cleanup, after a
  complete dependency audit and a successful Debian-package recovery exercise.

If implementation changes Nix recipes during coexistence, validate those edits
with `~/env/bin/env-nix check --working-tree` as required by `AGENTS.md`.

## Acceptance criteria

- [x] One application completes release import, local patch, clean build,
  installation, newer-release patch rebase, and recovery to a retained build.
- [x] A disposable snapshot of uncommitted local work can be packaged without
  changing or staging the original checkout.
- [x] Project development dependencies are available through a documented path,
  and ordinary incremental builds remain practical.
- [ ] Installed migrated packages use the Debian runtime with no Nix dependency
  or launcher-specific graphics environment requirement.
- [ ] Desktop and user-service activation work after a fresh login.
- [x] Public and private requirements retain their existing separation.
- [ ] Every removed Nix requirement has a replacement or a recorded exception;
  retained artifacts support the documented recovery procedure.
