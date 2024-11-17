#!/bin/bash -ex

# Note that this script requires 'mustache'
# (http://mustache.github.io/).  I installed it via:
#
# > gem install mustache

parent_path=$( cd "$(dirname "${BASH_SOURCE[0]}")" ; pwd -P )

cd "$parent_path"

echo "user: \"$USER\"" > env-vars.yaml
mustache env-vars.yaml 99-batify.rules.mustache > 99-batify.rules
echo "Successfully generated 99-batify.rules"
