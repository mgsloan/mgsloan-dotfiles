#!/bin/bash -ex

# freshen: yes

[ "$UID" -eq 0 ] || exec sudo USER_HOME="$HOME" USER_NAME="$LOGNAME" bash -e "$0" "$@"

../sysctl/apply.sh
