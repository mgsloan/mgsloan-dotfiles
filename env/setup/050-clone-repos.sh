#!/bin/bash -ex

# Clone missing working repos without updating existing checkouts.

if [ ! -d "$HOME/.emacs.d" ]; then
  git clone https://github.com/mgsloan/mgsloan-emacs.git "$HOME/.emacs.d"
  git -C "$HOME/.emacs.d" submodule init
  git -C "$HOME/.emacs.d" submodule update --recursive
else
  echo "$HOME/.emacs.d exists, so not cloning."
fi
