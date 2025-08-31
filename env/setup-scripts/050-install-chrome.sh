#!/bin/bash

cd
mkdir -p dl
cd dl
wget https://dl.google.com/linux/direct/google-chrome-stable_current_amd64.deb
dpkg -i google-chrome-stable_current_amd64.deb
cd -
