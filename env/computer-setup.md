# Computer setup notes

This is **not** a tutorial.  I'm not going to explain much.  Instead, this is just notes about how I've setup my computer, and generally the changes I've made to my setup. I think a lot of the stuff is good here, but don't blame me if it breaks.

# Initial install (2017-11-11)

## Preliminaries

Uncommented every repo in `/etc/apt/sources.list`

```
sudo apt update
sudo apt install build-essential autoconf automake git htop golang-go vlc camorama scrot byzanz
```

## Building emacs from source

```
mkdir oss/dev
cd oss/dev
```

Took a look at https://github.com/emacs-mirror/emacs/releases and picked a version.  Decided to go for latest unstable

Iterated with configure a bit, needed to install the following:

```
sudo apt install texinfo libx11-dev libxaw7-dev libjpeg-dev libpng-dev libgif-dev libtiff-dev libncurses-dev
 
git clone --branch emacs-25.3 --depth 1 https://github.com/emacs-mirror/emacs
cd emacs
./autogen.sh
./configure
make
sudo make install
```

## Installing stack

Added `PATH=/home/mgsloan/.local/bin:$PATH` to `~/.profile`.

```
mkdir ~/.local/bin
curl -sSL https://get.haskellstack.org/ | sh
```

Added `eval "$(stack --bash-completion-script stack)"` to `~/.bashrc`.

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
git clone --bare mgsloan/dotfiles .dotfiles-repo
echo "alias cfg='/usr/bin/git --git-dir=$HOME/.dotfiles-repo/ --work-tree=$HOME'" >> $HOME/.bashrc
```

NOTE: One side effect of my dotfiles is setting `~/.config/user-dirs.dirs` to some rather idiosyncratic paths. Take a look before using this directly.


