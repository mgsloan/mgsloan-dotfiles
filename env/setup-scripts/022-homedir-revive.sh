#!/bin/bash -ev

export GIT_DIR="$HOME/.home.git"
export GIT_WORK_TREE="$HOME"

git status --porcelain \
    | awk '$1 == "D" {print $2}' \
    | xargs git checkout HEAD --
