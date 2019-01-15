#!/bin/sh -e

# Note that this script requires 'mustache'
# (http://mustache.github.io/).  I installed it via:
#
# > gem install mustache

echo "---\nuser: \"$USER\"\n---\n" > env-vars.yaml
mustache env-vars.yaml 99-batify.rules.mustache > 99-batify.rules
echo "Successfully generated 99-batify.rules"
