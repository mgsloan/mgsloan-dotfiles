#!/bin/bash

# Fix the "failed to register" error that 'gnome-session' reports for
# 'local-xmonad-windowmanager-provider.desktop'. Based on
# http://askubuntu.com/a/583365/56280 .
#
# Presumably this can also eventually be fixed in the XMonad Gnome
# config:
#
# http://xmonad.org/xmonad-docs/xmonad-contrib/src/XMonad-Config-Gnome.html
#
# But, simply changing the '--print-reply=string' to '--print-reply'
# (as below, or to '--print-reply=literal', as in other answers on the
# AskUbuntu question linked above) might break things for other
# versions of Gnome.
dbus-send \
  --session \
  --print-reply \
  --dest=org.gnome.SessionManager \
  /org/gnome/SessionManager \
  org.gnome.SessionManager.RegisterClient \
  string:local-xmonad-windowmanager-provider.desktop \
  string:$DESKTOP_AUTO_START_ID

echo 'dot.xmonad/startup-hook.sh ran!' | tee -a ~/.xsession-log
