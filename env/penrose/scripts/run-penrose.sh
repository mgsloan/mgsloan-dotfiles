#!/usr/bin/env bash
# Session script for the penrose window manager.
#
# gdm-x-session runs this as the session itself, so when it returns the X
# session is over. penrose has no restart action of its own -- M-q rebuilds and
# exits, and this loop brings the new binary up -- which is why exiting has to
# mean two different things:
#
#   0   M-q: relaunch (tags and focus come back from EWMH properties)
#   42  logout: stop, ending the X session
#   *   crash: keep the log, relaunch
#
# Anything that must survive a restart is either an X property or is spawned
# again by the startup hook, so there is nothing to hand over here.

set -u

WM="$HOME/.local/bin/penrose-wm"

# The recovery terminals below are alacritty rather than the ghostty everything
# else here opens: they run when the session is already broken, and alacritty is
# an apt package in /usr/bin where ghostty is a hand-built binary under
# ~/.local. A recovery shell should depend on as little of this setup as it can.

if [ ! -x "$WM" ]; then
  echo "run-penrose: $WM is missing or not executable." >&2
  echo "run-penrose: run ~/env/penrose/scripts/rebuild-penrose.sh to build it." >&2
  exec alacritty -e sh -c \
    'echo "penrose-wm is not installed."
     echo "Run: ~/env/penrose/scripts/rebuild-penrose.sh"
     echo
     exec bash'
fi

# A window manager that dies immediately, every time, leaves a blank screen and
# no way to get a terminal up. Three failures inside a minute is treated as
# unrecoverable and drops to a shell instead of spinning.
failures=0
window_start=$SECONDS

while true; do
  systemd-cat --identifier=penrose "$WM"
  code=$?

  case $code in
    0)
      failures=0
      ;;
    42)
      echo "run-penrose: logout requested, ending session." >&2
      break
      ;;
    *)
      echo "run-penrose: penrose-wm exited with status $code." >&2

      if (( SECONDS - window_start > 60 )); then
        failures=0
        window_start=$SECONDS
      fi

      failures=$(( failures + 1 ))

      if (( failures >= 3 )); then
        echo "run-penrose: too many rapid failures, dropping to a shell." >&2
        exec alacritty -e sh -c \
          'echo "penrose-wm keeps crashing. Recent output:"
           echo
           journalctl --user -t penrose -n 50 --no-pager
           echo
           exec bash'
      fi
      ;;
  esac

  # Distinguishes a restart from the first start of the session, which is what
  # the startup hook uses in place of xmonad's handleStartup.
  export RESTARTED=true
done
