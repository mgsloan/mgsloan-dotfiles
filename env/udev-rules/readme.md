See the comments in [99-batify.rules.template][] and
[90-backlight.rules][] for a description of what these udev rules do.

To generate `99-batify.rules`, run `USER_NAME=$(id -un) ./generate.sh` after
activating the Nix environment, which supplies `envsubst` through gettext.

To apply these rules, run `sudo ./apply.sh`.

## `99-cpu-governor.rules`

Re-applies the stored CPU governor mode (performance / powersave / auto) on
AC plug and unplug, via `~/env/scripts/cpu-governor-apply.sh`. See that
script's header, and `~/env/polkit-rules/60-cpu-governor.rules` for why root
is allowed to drive power-profiles-daemon with no login session.
