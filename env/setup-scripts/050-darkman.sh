#!/bin/bash -ex

git config --global hub.protocol https

REPO="WhyNotHugo"
REPO_DIR="$HOME/oss/$REPO"
if [ ! -d "$REPO_DIR" ]; then
  hub clone "mgsloan/$REPO"
else
  echo "$REPO_DIR exists, so not cloning."
fi

cd "$REPO_DIR"
make
gksu make install PREFIX=/usr
systemctl --user enable --now darkman.service
