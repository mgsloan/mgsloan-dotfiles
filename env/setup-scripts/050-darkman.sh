#!/bin/bash -ex

git config --global hub.protocol https

REPO="darkman"
REPO_DIR="$HOME/oss/$REPO"
if [ ! -d "$REPO_DIR" ]; then
  git clone "https://gitlab.com/WhyNotHugo/darkman"
else
  echo "$REPO_DIR exists, so not cloning."
fi

cd "$REPO_DIR"
make
$HOME/env/setup-scripts/darkman-install.sh
