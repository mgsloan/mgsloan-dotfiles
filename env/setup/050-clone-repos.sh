#!/bin/bash -ex

# Clone missing working repos without updating existing checkouts.

if [ ! -d "$HOME/.emacs.d" ]; then
  git clone https://github.com/mgsloan/mgsloan-emacs.git "$HOME/.emacs.d"
  git -C "$HOME/.emacs.d" submodule init
  git -C "$HOME/.emacs.d" submodule update --recursive
else
  echo "$HOME/.emacs.d exists, so not cloning."
fi

clone_if_missing() {
  local dir="$1" repo="$2"
  local name="${repo##*/}"

  if [ -d "$dir/$name" ]; then
    echo "$dir/$name exists, so not cloning."
    return
  fi

  mkdir -p "$dir"
  cd "$dir"
  hub clone "$repo"
}

clone_if_missing "$HOME/proj/utils" mgsloan/todoist-shortcuts
clone_if_missing "$HOME/proj/utils" mgsloan/unblock-with-intention
clone_if_missing "$HOME/proj/utils" mgsloan/gmail-label-switch-shortcuts

clone_if_missing "$HOME/proj/old" mgsloan/roam-navigator
