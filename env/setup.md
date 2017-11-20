# Computer setup notes

This is **not** a tutorial.  I'm not going to explain much.  Instead, this is just notes about how I've setup my computer, and generally the changes I've made to my setup. I think a lot of the stuff is good here, but don't blame me if it breaks.

Early on this will describe some changes that ended up in this git repository.  However, for the most part, if a part of the setup is a config file that is tracked by the repo, I'm omitting discussion of it.

# Initial install (2017-11-11)

Based on Ubuntu 17.10 "artful ardvark"

## Preliminaries

Uncommented every repo in `/etc/apt/sources.list`

Install some stuff that ends up getting used below:

```
sudo apt update
sudo apt install build-essential autoconf automake git htop golang-go vlc camorama scrot byzanz ubunmtu-restricted-extras gimp
```

## Building emacs from source

```
mkdir oss/dev
cd oss/dev
```

Took a look at https://github.com/emacs-mirror/emacs/releases and picked a version.  Decided to go for latest unstable

Iterated with configure a bit, needed to ilist '("/path/to/fonts"))nstall the following:

```
sudo apt install texinfo libx11-dev libxaw7-dev libjpeg-dev libpng-dev libgif-dev libtiff-dev libncurses-dev

git clone --branch emacs-25.3 --depth 1 https://github.com/emacs-mirror/emacs
cd emacs
./autogen.sh
./configure
make
sudo make install
```

## Installing hub

```
cd ~/oss/dev
git clone https://github.com/github/hub.git
cd hub
./script/build
mv bin/hub ~/.local/bin
```

Added `eval "$(hub alias -s)"` to `~/.bashrc`

## Set up ssh keys

https://help.github.com/articles/generating-a-new-ssh-key-and-adding-it-to-the-ssh-agent/

## Creating git repo to manage dot files

https://developer.atlassian.com/blog/2016/02/best-way-to-store-dotfiles-git-bare-repo/

```
git clone --bare mgsloan/dotfiles .dotfiles-git
echo "alias cfg='/usr/bin/git --git-dir=$HOME/.dotfiles-git/ --work-tree=$HOME'" >> $HOME/.bashrc
```

NOTE: One side effect of my dotfiles is setting `~/.config/user-dirs.dirs` to some rather idiosyncratic paths. Take a look before using this directly.

## Installing spacemacs

Included as a submodule of the dotfiles repo, so init / update it.

I figured I'd install the suggested font.  Previously I'd used Inconsolata.  So, from https://github.com/adobe-fonts/source-code-pro/issues/17#issuecomment-167650167

```
FIXME: figure out an approach that actually works.  Clean up ~/.fonts
```

## Installing stack

Added `PATH=/home/mgsloan/.local/bin:$PATH` to `~/.profile`.

```
mkdir ~/.local/bin
curl -sSL https://get.haskellstack.org/ | sh
```

Added `eval "$(stack --bash-completion-script stack)"` to `~/.bashrc`.

## Installing ghc

```
cd ~/env
stack setup
```

## Installing xmonad

```
sudo apt install x11-dev libxinerama-dev libxrandr-dev libxft-dev
cd ~/env
stack build
```

## Building / installing keynav


```
sudo apt install libcairo2-dev libxinerama-dev libxdo-dev

cd ~/env/keynav
make

# Make a symbolic link so that I can just edit and rebuild
cd ~/.local/bin
ln -s ~/env/keynav/keynav
```

## Installing rust

```
curl https://sh.rustup.rs -sSf | sh
```

## Installing spotify

https://www.spotify.com/us/download/linux/

```
sudo apt-key adv --keyserver hkp://keyserver.ubuntu.com:80 --recv-keys 0DF731E45CE24F27EEEB1450EFDC8610341D9410
echo deb http://repository.spotify.com stable non-free | sudo tee /etc/apt/sources.list.d/spotify.list
sudo apt-get update
sudo apt-get install spotify-client
```

## Fixing hidpi issues

Happily, my displays are all high DPI. 4K laptop screen on my P51, with 282 ppi.
264 ppi on my [packed pixels displays](https://www.packedpixels.com/).

The config in this repo
