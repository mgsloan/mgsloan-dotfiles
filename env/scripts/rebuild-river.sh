#!/bin/sh -eu

# Builds the river variant of the config and installs it where river's init
# expects to find it.
#
# The X11 variant does not need a script like this: xmonad's own recompile
# machinery calls ~/.xmonad/build, which hard links the binary into
# ~/.xmonad/. Under river nothing does that for us -- river just execs
# ~/.local/bin/custom-xmonad-river -- so the install step has to live here.
#
# Forgetting it is not a subtle failure but it looks like one: river starts,
# the init script dies on a missing binary, and since a compositor with no
# window management client renders nothing, the screen is blank apart from the
# cursor.

SRC_DIR=~/env
EXE_NAME=custom-xmonad-river
DESTINATION=~/.local/bin/$EXE_NAME

# The river build lives in its own stack.yaml, which sets the `river` cabal
# flag. Point at it explicitly so an inherited STACK_YAML cannot select the
# X11 build and quietly install the wrong binary under the river name.
export STACK_YAML=$SRC_DIR/stack-river.yaml

cd $SRC_DIR
stack build
stack install errlog-filter

mkdir -p "$(dirname $DESTINATION)"

# Hard link rather than copy, matching ~/.xmonad/build, so that the running
# session keeps the inode it started from and a rebuild cannot swap the binary
# out from under a restart that is midway through exec.
echo "Linking $EXE_NAME to $DESTINATION"
ln -f -T "$(stack path --local-install-root)/bin/$EXE_NAME" $DESTINATION
