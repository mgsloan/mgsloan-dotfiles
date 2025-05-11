#!/bin/bash -ex

# Note that this script requires 'mustache'
# (http://mustache.github.io/).
#
# > gem install mustache

if [ -z "$USER_NAME" ]; then
    echo "Expected USER_NAME to be set."
    exit 1
fi

parent_path=$( cd "$(dirname "${BASH_SOURCE[0]}")" ; pwd -P )

cd "$parent_path"

echo "user: \"$USER_NAME\"" > env-vars.yaml
mustache env-vars.yaml 99-batify.rules.mustache > 99-batify.rules
echo "Successfully generated 99-batify.rules"
