#!/bin/bash -ev

export GIT_DIR="$HOME/.home.git"
export GIT_WORK_TREE="$HOME"

git status --porcelain \
    | awk '$1 == "D" {print $2}' \
    | xargs git checkout HEAD --

git config submodule.env-private.active false
git -C "$HOME" submodule update --init --recursive -- . ':!ep'
# Setup explicitly updates the private checkout while leaving it inactive.
git -C "$HOME" -c submodule.env-private.active=true submodule update --init --recursive -- ep
git config core.excludesFile "$HOME/ep/home.gitignore"
