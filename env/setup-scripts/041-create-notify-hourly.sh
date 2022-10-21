#!/bin/bash -e

[ "$UID" -eq 0 ] || exec sudo USER_HOME="$HOME" bash -e "$0" "$@"

SOURCE="$USER_HOME/env/scripts/notify-hourly.sh"
DESTINATION="/etc/cron.hourly/notify-hourly"

cp "$SOURCE" "$DESTINATION"
chmod ugo+x $DESTINATION

echo ""
echo "Contents of $DESTINATION is now:"
echo ""
cat $DESTINATION
