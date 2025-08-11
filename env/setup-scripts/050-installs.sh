#!/bin/bash

curl --proto '=https' --tlsv1.2 -sSf https://sh.rustup.rs | sh
curl -sSL https://get.haskellstack.org/ | sh
curl -fsSL https://get.pnpm.io/install.sh | sh -
curl https://rustwasm.github.io/wasm-pack/installer/init.sh -sSf | sh
curl -LsSf https://astral.sh/uv/install.sh | sh
curl --proto '=https' --tlsv1.2 -sSf https://just.systems/install.sh | bash -s -- --to ~/.local/bin/


# Install gsutil etc
curl -O https://dl.google.com/dl/cloudsdk/channels/rapid/downloads/google-cloud-cli-linux-x86_64.tar.gz
mv google-cloud-cli-linux-x86_64.tar.gz ~/.local
cd ~/.local
tar -xf google-cloud-cli-linux-x86_64.tar.gz
rm google-cloud-cli-linux-x86_64.tar.gz
./google-cloud-sdk/install.sh
cd ~/
