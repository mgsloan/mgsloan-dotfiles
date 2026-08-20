#!/bin/bash -e

# freshen: yes

# Builds darkman and installs it into /usr.
#
# darkman is what runs the light/dark hooks in ~/.data/{dark,light}-mode.d: the
# ghostty and alacritty palettes and the GTK theme. Debian does not package it
# at all, so it is built from the submodule at oss/darkman, which pins the
# commit the way 047-build-ghostty.sh does.
#
# It was a clone-if-missing before that, which is the worst of both: the
# checkout was never fetched, so it never got newer, and its commit was
# recorded nowhere, so which darkman a machine ended up with depended on the
# day it was set up. A submodule at least makes that a decision.
#
# Its build dependencies are golang and scdoc, under "Deps of building darkman"
# in env/apt-packages.md.
#
# == Cost on the freshen path
#
# `make` is the up-to-date check, and Go's build cache makes an unchanged tree a
# no-op in single-digit milliseconds. `make install` and `systemctl --user
# enable --now` are both idempotent and cheap, so none of this needs a guard.

DARKMAN_DIR="$HOME/oss/darkman"

# From $HOME, because `cfg` sets --work-tree but not the working directory: git
# reads a path as relative to where it was called from, so `oss/darkman` run
# from setup-scripts/ would ask about setup-scripts/oss/darkman and always miss.
# 045-build-penrose.sh and 047-build-ghostty.sh cd for the same reason.
cd "$HOME"

# Said outright rather than left to git's "pathspec did not match any file(s)",
# because a home repo that predates the submodule is the ordinary way to arrive
# here and the fix is one command.
if ! cfg ls-files --error-unmatch "oss/darkman" > /dev/null 2>&1; then
  echo "050-darkman: oss/darkman is not a submodule of the home repo yet." >&2
  echo "Add it with:" >&2
  echo "  cd ~ && cfg submodule add https://gitlab.com/WhyNotHugo/darkman oss/darkman" >&2
  exit 1
fi

# Targeted rather than a blanket `submodule update`, so this does not quietly
# move other submodules under someone's feet.
cfg submodule update --init "oss/darkman"

cd "$DARKMAN_DIR"

make

# Installs into /usr, so it sudos for itself. Invoked from the source tree
# because its `make install` reads the working directory as that tree.
"$HOME/env/setup-scripts/darkman-install.sh"

# Enabled rather than spawned: a second `darkman run` unlinks and rebinds the
# first one's control socket before finding out the D-Bus name is taken, which
# leaves the survivor listening on an orphaned inode. See the note in
# penrose/src/startup.rs, which is why nothing starts darkman by hand.
systemctl --user enable --now darkman.service

echo
echo "Installed:"
darkman --version
