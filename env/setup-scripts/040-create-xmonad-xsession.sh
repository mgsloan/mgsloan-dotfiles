#!/bin/bash -ev

[ "$UID" -eq 0 ] || exec sudo USER_HOME="$HOME" bash -ex "$0" "$@"

TEMPLATE="$USER_HOME/env/xsessions/xmonad.desktop.template"
DESTINATION="/usr/share/xsessions/xmonad.desktop"

envsubst < $TEMPLATE > $DESTINATION

echo ""
echo "Contents of $DESTINATION is now:"
echo ""
cat $DESTINATION
