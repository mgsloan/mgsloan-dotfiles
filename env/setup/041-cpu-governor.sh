#!/bin/bash -e

# freshen: yes

[ "$UID" -eq 0 ] || exec sudo bash -e "$0" "$@"

parent_path=$( cd "$(dirname "${BASH_SOURCE[0]}")" ; pwd -P )
cd "$parent_path/../system"

install -m 0644 systemd/cpu-governor.service /etc/systemd/system/cpu-governor.service
install -m 0644 udev-rules/99-cpu-governor.rules /etc/udev/rules.d/99-cpu-governor.rules
install -m 0644 polkit-rules/60-cpu-governor.rules /etc/polkit-1/rules.d/60-cpu-governor.rules
systemctl daemon-reload
# Remove the old multi-user.target link as well as adding the new targets.
systemctl reenable cpu-governor.service
udevadm control --reload-rules
systemctl restart cpu-governor.service
systemctl show cpu-governor.service -p Result -p ExecMainStatus
