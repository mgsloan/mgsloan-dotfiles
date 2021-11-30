#!/bin/bash -ex

# Use this script to verify things look as expected before doing
# reviving deleted files.

export GIT_DIR="$HOME/.home.git"
export GIT_WORK_TREE="$HOME"

git status
