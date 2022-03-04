#!/bin/bash -ev

[ "$UID" -eq 0 ] || exec sudo USER_HOME="$HOME" bash -ex "$0" "$@"

TEMPLATE="$USER_HOME/env/templates/move-downloads.template"
DESTINATION="/etc/cron.daily/move-downloads"

envsubst < $TEMPLATE > $DESTINATION

echo ""
echo "Contents of $DESTINATION is now:"
echo ""
cat $DESTINATION
