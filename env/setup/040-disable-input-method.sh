#!/bin/bash -ex

# Stops an input method framework being started with the session.
#
# The sibling of 040-disable-unnecessary-services.sh, for the same reason and at
# the same point: something unnecessary is running and getting in the way. It is
# a separate script because this one is user configuration -- im-config writes
# ~/.xinputrc -- where that one is systemd units and needs root.
#
# == Why
#
# Debian's /etc/X11/Xsession.d/70im-config_launch rewrites $STARTUP to
# `im-launch <session>`, so ibus starts and then execs the window manager. It
# then takes passive X grabs on keys the window manager wants, and only one
# client may hold a grab on a key: whoever asks first wins, silently, and the
# window manager's binding simply never fires.
#
#   Super+space                      switch to the next input engine
#   Super+period, Super+semicolon    ibus's emoji picker
#   Ctrl+space, Alt+grave, ...       the legacy "trigger" list
#
# The first three are `M-space` (next layout), `M-period` (IncMain(-1)) and
# `M-semicolon` (ExpandMain) in the xmonad and penrose configs -- three of the
# four layout controls, which is how this was found.
#
# None of it buys anything here: the configured engine is `xkb:us::eng`, which
# is a plain XKB layout handled by the X server rather than an input method, so
# the "switch engine" key cycles a list of one. Typing, dead keys and Compose
# are all XKB and unaffected; GTK and Qt fall back to their built-in simple
# input methods, which keep Ctrl+Shift+U for Unicode and GTK's own emoji chooser
# on Ctrl+. and Ctrl+;.
#
# What is given up is CJK, Indic and Vietnamese input, and ibus's emoji picker.
# If you want any of that back: `im-config -w ibus`, then log in again.
#
# This is an X11 problem specifically. Under river the window manager registers
# its bindings with the compositor rather than racing for them, so nothing can
# take them; ibus's grabs there would only reach Xwayland clients.

if [ "$UID" -eq 0 ]; then
  echo "Run this as yourself, not with sudo: im-config writes ~/.xinputrc," >&2
  echo "and as root that would be /root/.xinputrc." >&2
  exit 1
fi

if ! command -v im-config >/dev/null; then
  echo "im-config is not installed, so nothing starts an input method: done."
  exit 0
fi

# `-w none` rather than the `-n none` that older documentation gives: im-config
# 1.1 has no -n, and its unknown-option branch logs to syslog and exits 1 with
# nothing on the terminal, which looks exactly like it worked.
#
# Idempotent: writes the same ~/.xinputrc every time. `im-config -w auto` removes
# the file, which is how to get back to the default.
im-config -w none

echo ""
echo "Input method framework disabled. ~/.xinputrc now says:"
echo ""
cat ~/.xinputrc

echo ""
echo "This takes effect at the next login. To free the keys in the session that"
echo "is running now, either log out and back in, or:"
echo ""
echo "  gsettings set org.freedesktop.ibus.panel.emoji hotkey \"[]\""
echo "  gsettings set org.freedesktop.ibus.general.hotkey triggers \"[]\""
echo "  gsettings set org.gnome.desktop.wm.keybindings switch-input-source \"[]\""
