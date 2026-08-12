#!/bin/sh -e
# Build the window managers and install them where the session scripts look.
#
# Usage: rebuild-penrose.sh [x11|river]
#
# One config produces two window managers, and M-q rebuilds from inside one of
# them, so the argument names which one is running. That one is checked, built
# and installed first and this script then returns, so the restart happens as
# soon as the binary being restarted into is ready. The other backend is built
# afterwards, in the background: it has to be built by something, or a change
# that breaks it is not discovered until the next login into a session that then
# drops to a recovery shell -- but nothing about it needs to hold up the restart.
#
# With no argument both are built in the foreground, which is what the setup
# scripts want: nothing is running to restart, and a background build that
# outlives the installer would be a surprise.
#
# `--only <backend>` builds one and backgrounds nothing. That is how the
# background half runs, and it has to exist: re-invoking this script with a bare
# backend name would background the *other* one again, and the two would take
# turns rebuilding each other for as long as the machine was up.
#
# Invoked by M-q from inside the running window manager, so it must not assume a
# terminal is attached: output goes to the caller, which logs it.
#
# Installing over a running binary is fine -- `install` replaces the directory
# entry rather than writing through it, so the running process keeps its own copy
# until it exits.

cd "$(dirname "$0")/.."

BIN="$HOME/.local/bin"
LOG="${XDG_STATE_HOME:-$HOME/.local/state}/penrose-rebuild.log"
mkdir -p "$BIN" "$(dirname "$LOG")"

# clippy.toml forbids the APIs that compile but are wrong in a window manager,
# and main.rs makes them a hard error. Running it before the build is what
# enforces them, and running it before *anything* is installed is what makes a
# failure harmless: M-q reports it and the working binary stays where it is.
build() {
  case "$1" in
    x11)
      cargo clippy --all-targets
      cargo build --release
      install -m 755 target/release/penrose-wm "$BIN/penrose-wm"
      ;;
    river)
      # The two features are alternatives rather than additions, hence
      # --no-default-features; see main.rs.
      cargo clippy --all-targets --no-default-features --features river
      cargo build --release --no-default-features --features river
      install -m 755 target/release/penrose-wm "$BIN/penrose-river-wm"
      ;;
  esac
}

# The half that outlives this script, and so has to report for itself: the
# window manager that asked for the rebuild is gone by the time this finishes.
#
# Deprioritised, because it runs behind a window manager that has just restarted
# and a person who has gone back to what they were doing. `nice` covers the CPU
# and `ionice` the linker's I/O, and both are inherited by cargo and every rustc
# it spawns. Not `-j 1`: that caps throughput even when nothing else wants the
# machine, where the point is to yield when something does. If memory rather than
# CPU turns out to be what hurts, a job limit is the knob for that.
build_in_background() {
  other=$1

  low_priority="nice -n 19"
  if command -v ionice >/dev/null 2>&1; then
    low_priority="$low_priority ionice -c 3"
  fi

  # Both outcomes are worth a notification, because nothing else reports them:
  # the window manager that asked for this has already restarted, and the log is
  # not somewhere anybody is looking. Success is low urgency -- it says the other
  # session is safe to log into -- and failure is not.
  #
  # Unquoted on purpose: this is a command prefix, not one argument.
  # shellcheck disable=SC2086
  setsid $low_priority sh -c "
    cd '$PWD'
    if '$0' --only '$other' >>'$LOG' 2>&1; then
      notify-send -u low -i '$HOME/env/xmonad.png' 'Penrose' \
        'Finished building $other'
    else
      notify-send -i '$HOME/env/xmonad.png' 'Penrose' \
        'Failed building $other. See $LOG'
    fi
  " >/dev/null 2>&1 &
}

case "${1:-}" in
  x11)
    build x11
    build_in_background river
    ;;
  river)
    build river
    build_in_background x11
    ;;
  --only)
    case "${2:-}" in
      x11 | river) build "$2" ;;
      *)
        echo "usage: $(basename "$0") --only [x11|river]" >&2
        exit 2
        ;;
    esac
    ;;
  "")
    build x11
    build river
    ;;
  *)
    echo "usage: $(basename "$0") [x11|river|--only x11|--only river]" >&2
    exit 2
    ;;
esac
