#!/bin/bash -ev

[ "$UID" -eq 0 ] || exec sudo USER_HOME="$HOME" bash -ex "$0" "$@"

# Re-execing above is the intended route, and it passes USER_HOME through.
# Running this as `sudo 044-...` instead skips that line with USER_HOME unset,
# which used to leave TEMPLATE pointing at /env/... and fail on the spot.
if [ -z "${USER_HOME:-}" ]; then
  USER_HOME="$(getent passwd "${SUDO_USER:-$USER}" | cut -d: -f6)"
  export USER_HOME
fi

TEMPLATE="$USER_HOME/env/xsessions/penrose.desktop.template"
DESTINATION="/usr/share/xsessions/penrose.desktop"

mkdir -p "$(dirname "$DESTINATION")"
envsubst < "$TEMPLATE" > "$DESTINATION"

echo ""
echo "Contents of $DESTINATION is now:"
echo ""
cat "$DESTINATION"
