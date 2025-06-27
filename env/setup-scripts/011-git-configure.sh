#!/bin/bash -ev

git config --global user.name "Michael Sloan"
git config --global user.email "mgsloan@gmail.com"
git config --global credential.helper /usr/share/doc/git/contrib/credential/libsecret/git-credential-libsecret

# Use ssh key for signing commits
git config --global gpg.format ssh
git config --global user.signingkey ~/.ssh/id_ed25519.pub
git config --global commit.gpgsign true
