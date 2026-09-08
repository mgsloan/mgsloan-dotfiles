#!/bin/bash -ex

# envsubst is supplied by the Nix gettext package.

if [[ ! ${USER_NAME:-} =~ ^[a-z_][a-z0-9_-]*[$]?$ ]]; then
    echo "Expected USER_NAME to contain a valid login name." >&2
    exit 1
fi

parent_path=$( cd "$(dirname "${BASH_SOURCE[0]}")" ; pwd -P )

cd "$parent_path"

export USER_NAME
# Restrict substitution so udev's $attr{capacity} expressions survive.
envsubst '${USER_NAME}' < 99-batify.rules.template > 99-batify.rules
echo "Successfully generated 99-batify.rules"
