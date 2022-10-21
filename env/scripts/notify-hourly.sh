#!/bin/bash -e

# This script is installed into /etc/cron.hourly/ by ~/env/setup-scripts/041-create-notify-hourly.sh

CURRENT_HOUR=$(date +"%H")

case "$CURRENT_HOUR" in

  "21")notify-send "time = 21:00" "Time to think about winding down";;
  "22")notify-send "time = 22:00" "Initiate wind down sequence!! :D";;
  "23")notify-send "time = 23:00" "Hey! Stop ignoring this! Time to wind down!";;
  "00")notify-send "MIDNIGHT" "You really should wind down. Tomorrow going well is the priority now.";;
  "01")notify-send "THE TIME IS NOW ONE AM" "Don't be a degenerate!";;
  "02")notify-send "THE TIME IS NOW TWO AM" "Seriously??";;
  "03")notify-send "THE TIME IS NOW THREE AM" "You better have a darn good reason..";;

  "07")notify-send "7 am - someone's up early, awesome!" "Good time to do something you've been procrastinating.";;
  "08")notify-send "8 am. Good morning!" "What can you do now to help make this a great day?";;
  "10")notify-send "10 am. Coffee time!";;

  *)notify-send "hour = $CURRENT_HOUR";;
esac
