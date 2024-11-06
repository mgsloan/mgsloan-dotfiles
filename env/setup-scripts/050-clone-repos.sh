#!/bin/bash -ex

git config --global hub.protocol https

if [ ! -d "$HOME/.emacs.d" ]; then
  git clone https://github.com/mgsloan/mgsloan-emacs.git "$HOME/.emacs.d"
  cd "$HOME/.emacs.d"
  git submodule init
  git submodule update --recursive
else
  echo "$HOME/.emacs.d exists, so not cloning."
fi

mkdir -p "$HOME/proj"

REPO="todoist-shortcuts"
REPO_DIR="$HOME/proj/$REPO"
if [ ! -d "$REPO_DIR" ]; then
  hub clone "mgsloan/$REPO"
else
  echo "$REPO_DIR exists, so not cloning."
fi

REPO="roam-navigator"
REPO_DIR="$HOME/proj/$REPO"
if [ ! -d "$REPO_DIR" ]; then
  hub clone "mgsloan/$REPO"
else
  echo "$REPO_DIR exists, so not cloning."
fi

REPO="unblock-with-intention"
REPO_DIR="$HOME/proj/$REPO"
if [ ! -d "$REPO_DIR" ]; then
  hub clone "mgsloan/$REPO"
else
  echo "$REPO_DIR exists, so not cloning."
fi

REPO="gmail-label-switch-shortcuts"
REPO_DIR="$HOME/proj/$REPO"
if [ ! -d "$REPO_DIR" ]; then
  hub clone "mgsloan/$REPO"
else
  echo "$REPO_DIR exists, so not cloning."
fi
