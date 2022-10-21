#!/bin/bash -e

[ "$UID" -eq 0 ] || exec sudo USER_HOME="$HOME" bash -e "$0" "$@"

TEMPLATE="$USER_HOME/env/templates/move-downloads.template"
DESTINATION="/etc/cron.hourly/move-downloads"

envsubst < $TEMPLATE > $DESTINATION
chmod ugo+x $DESTINATION

echo ""
echo "Contents of $DESTINATION is now:"
echo ""
cat $DESTINATION
