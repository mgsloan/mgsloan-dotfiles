#!/bin/sh
# Resolve the stored CPU governor mode against current AC status and apply it
# through power-profiles-daemon. Invoked by 99-cpu-governor.rules (udev, on AC
# change), cpu-governor.service (systemd, at boot) and
# src/actions/cpu_governor.rs (penrose, on every startup/restart and from
# M-x cpu-governer).
#
# Goes through `powerprofilesctl` rather than writing scaling_governor
# directly: power-profiles-daemon already owns cpufreq governor + EPP on this
# intel_pstate machine, and a direct write would just get overwritten the next
# time ppd re-asserts its profile (e.g. on resume). Runs as whoever invokes it
# -- root from udev/systemd, mgsloan from penrose -- which needs
# ~/env/polkit-rules/60-cpu-governor.rules: ppd's own polkit policy only
# authorizes an *active* session, which a udev worker or a boot-time service
# has none of.

set -eu

STATE=/home/mgsloan/.local/state/penrose/cpu-governor-mode
AC=/sys/class/power_supply/AC/online

mode=$(cat "$STATE" 2>/dev/null || echo auto)

default_profile() {
    if [ "$(cat "$AC" 2>/dev/null)" = 1 ]; then
        echo performance
    else
        echo power-saver
    fi
}

case "$mode" in
    performance) profile=performance ;;
    powersave)   profile=power-saver ;;
    auto)        profile=$(default_profile) ;;
    *)
        echo "cpu-governor-apply: unknown mode '$mode' in $STATE, defaulting to auto" >&2
        profile=$(default_profile)
        ;;
esac

powerprofilesctl set "$profile"
