# NOTE: thusfar untested.

cp -i .vimrc ../
mkdir ../.logs

# Install cabal
cabal -V || apt-get install cabal-install
cabal update
cabal install cabal-install

# Install xmonad dependencies
apt-get install libxft-dev 
cabal install xmonad-contrib

# optional: install referenced programs
#apt-get install feh vlc google-chrome
