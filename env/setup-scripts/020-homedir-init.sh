#!/bin/bash -ex

# Clones my computer config to the home dir. See env/home-dir-git.md

export GIT_DIR="$HOME/.home.git"
export GIT_WORK_TREE="$HOME"

git clone --bare https://github.com/mgsloan/mgsloan-dotfiles.git "$HOME/.home.git"
git config core.bare false
git config core.logAllRefUpdates true
git config core.workdir ../
git reset HEAD -- .
