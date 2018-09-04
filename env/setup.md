# Computer setup notes

This is **not** a tutorial. I'm not going to explain much. Instead, this is just
notes about how I've setup my computer, and generally the changes I've made to
my setup. I think a lot of the stuff is good here, but don't blame me if it
breaks.

Early on this will describe some changes that ended up in this git repository.
However, for the most part, if a part of the setup is a config file that is
tracked by the repo, I'm omitting discussion of it.

# Initial install (2017-11-11)

Based on Ubuntu 17.10 "artful ardvark"

## Preliminaries

Uncommented every repo in `/etc/apt/sources.list`

Install some stuff that ends up getting used below:

```
sudo apt update
sudo apt install build-essential autoconf automake git htop golang-go vlc camorama scrot byzanz ubuntu-restricted-extras gimp feh silversearcher-ag net-tools suckless-tools cmake tmux
```

Even after initial setup I've been updating the above install with packages that
are useful.

## Building emacs from source

```
mkdir oss/dev
cd oss/dev
```

Took a look at https://github.com/emacs-mirror/emacs/releases and picked a
version. Decided to go for latest unstable

Iterated with configure a bit, needed to ilist '("/path/to/fonts"))nstall the
following:

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
git clone --bare git@github.com:mgsloan/.dotfiles
echo "alias cfg='/usr/bin/git --git-dir=\$HOME/.dotfiles.git/ --work-tree=\$HOME'" >> $HOME/.bashrc
```

NOTE: Up to date instructions are in ../readme.md

NOTE: One side effect of my dotfiles is setting `~/.config/user-dirs.dirs` to
some rather idiosyncratic paths. Take a look before using this directly.

NOTE: This repo already contains the alias in its .bashrc

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

cd ~/oss/env/keynav
make

# Make a symbolic link so that I can just edit and rebuild
cd ~/.local/bin
ln -s ~/oss/env/keynav/keynav
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

The config in this repo attempts to enable various hidpi settings for the
following environment setting: ``USE_DPI="true"``, in
[`settings.sh`](./settings.sh).

## Display brightness

The wayland display management settings do not work for me, so I need to do the
following to set display brightness:

```
echo 1060 | sudo tee /sys/class/backlight/intel_backlight/brightness
```

(Note, worth checking `/sys/class/backlight/intel_backlight/max_brightness`)


FIXME: automate this

## Power management considerations

Many things suggest to use tlp. However, it looks like [this may be
problematic](https://www.reddit.com/r/thinkpad/comments/6hi0zn/if_youre_thinking_of_running_linux_on_a_p51_read/):

> Do not use TLP, thermald or anything of the sort, if you're not disabling
> intel_pstate. It'll get stuck at the lowest p-state i.e. you'll be using your
> CPU at 800MHz fixed. It won't get fixed by a reboot, you have to plug/unplug
> or remove the battery or an event like this to get it back. Apparently this
> this is pretty common, and in earlier models Intel provided gems like
> workaround: None identified
> (https://www.intel.com/content/dam/www/public/us/en/documents/specification-updates/xeon-phi-coprocessor-specification-update.pdf).

## Display driver / bumblebee stuf

https://wiki.archlinux.org/index.php/hybrid_graphics

From https://help.ubuntu.com/community/BinaryDriverHowto/Nvidia , first I
checked `sudo ubuntu-drivers devices`, and then chose the latest non-free
driver:

```
sudo apt install nvidia-384
```

FIXME: Fully Power Down Discrete GPU

FIXME: add comment to https://www.reddit.com/r/thinkpad/comments/6hi0zn/if_youre_thinking_of_running_linux_on_a_p51_read/

## Fixing firefox scroll rate

After installing the nvidia driver, I noticed that scrolling in firefox was
sometimes awfully slow. Slower than with mesa, but that had ugly tearing. I
tried checking "Use hardwaree acceleration when available", however this didn't
change anything. Unchecking "Use smooth scrolling" greatly improved things, but
still seemed too slow.

Here's what worked:

* Open `about:config` + set `layers.acceleration.force-enabled` to `true`

Ended up disabling smooth scrolling anyway, because I prefer quick scroll.

## Other Firefox config tweaks

https://www.reddit.com/r/linux/comments/39q6xt/some_useful_firefox_tips_to_fix_choppy_scrolling/

* Enable tracking protection, also boosts performance

  - Set `privacy.trackingprotection.enabled` to `true`

* Right clicked on toolbar, hit customize. Rearranted it and switched to dark
  theme. Changed density to compact to save screen space.

## Building Firefox from source

I noticed in `about:support` that the new rust based "WebRender" stuff isn't
included.  Similarly to how I built emacs from source, it might encourage me to
work on Firefox if I build it from source.  Particularly because now it's using
stuff written in Rust!

```
wget https://hg.mozilla.org/mozilla-central/raw-file/default/python/mozboot/bin/bootstrap.py
python bootstrap.py
```

Part of this bootstrapping process asked me to install watchman, in order to
make mercurial status / etc faster. Presumably by updating incrementally. I
followed the instructions
[here](https://medium.com/@vonchristian/how-to-setup-watchman-on-ubuntu-16-04-53196cc0227c)
and checked out `v4.9.0`.  Needed to do `sudo apt install libssl-dev`

FIXME: Did mercurial extensions.firefoxtree get installed?

Onwards to actually building firefox:

```
./mach build
```

NOTE: I also put `mk_add_options MOZ_MAKE_FLAGS="-j3"` into a new mozconfig
file, so that my computer wouldn't get hammered so hard / can work on other
stuff.

`./mach run` then runs the new firefox in a temporary profile.  I couldn't find
instructions on how to install the built version.  So I just did this:

```
cd ~/oss/env/mozilla/obj-x86_64-pc-linux-gnu
cp dist dist-used
cd ~/.local/bin
ln -s ~/oss/env/mozilla/obj-x86_64-pc-linux-gnu/dist-used/bin/firefox
```

Probably copying **way** more than necessary, but that's fine.

## Firefox add-ons

* Disabled "Ubuntu Modifications"

### Digression: vimium-fx background

I was a heavy vimium user in chrome.  As is clear from the above, I'm thinking
about making the switch back to Firefox.  Here's why:

* Newest firefox appears to be faster and lighterweight than chrome.

* I really like Rust, and now substantial portions of Firefox are written in
  Rust. I like using software that I'm more likely to enjoy working on, if
  there's anything I particularly want changed.

I'm a heavy vimium user. Something like that is essential to me browsing
comfortably. I looked into whether I shoudl use vimperator or vimfx, and found
the surprise that it looks unlikely that they will be supported in Firefox
Quantum.  Happily, it looks Vimium will be able to fill the void!!

Relevant git issue - https://github.com/philc/vimium/issues/2425

* [vimium-ff addon](https://addons.mozilla.org/en-US/firefox/addon/vimium-ff/)

### Extensions installed

TODO: At some later date check back and ensure this is still the main package.

* [ublock origin](https://addons.mozilla.org/en-US/firefox/addon/ublock-origin/)
  (less memory requirements than ad block)

* [stackgo](https://addons.mozilla.org/en-US/firefox/addon/stackgo/) to redirect
  from haskell package site hackage.org to stackage.org, which has more reliable
  documentation building among other features.

* [HTTPS Everywhere](https://addons.mozilla.org/en-US/firefox/addon/https-everywhere/)

* [Enhancer for YouTube](https://addons.mozilla.org/en-US/firefox/addon/enhancer-for-youtube/)
  (not sure if this is quality, but it was recommended and has 300k users)

  - Enabled "automatically enable the cinema mode"

  - Enabled "automatically remove ads"

  - Enabled "automatically sort comments from newest to oldest"

  - Selected "Deep Dark (Firefox Quantum Dark)" theme

* [React Developer Tools](https://addons.mozilla.org/en-US/firefox/addon/react-devtools/)

* [Keepa Amazon Price Tracker](https://addons.mozilla.org/en-US/firefox/addon/keepa/)
  (not sure if this is quality, seemed interesting / useful)

* [Amazon smile redirect](https://addons.mozilla.org/en-US/firefox/addon/amazon-smile/)

* [Tree style tabs](https://addons.mozilla.org/en-US/firefox/addon/tree-style-tab/)

  - NOTE: In the end disabled it, doesn't feel snappy enough (I hate animations
    in UI..), and it needs keyboard shortcuts to actually be powerful

  - Selected 'Plain Dark' theme

  * Insertion position of new tabs from pinned tabs: end of tree

  - Need to manually hide normal tabs bar -
    https://github.com/piroor/treestyletab/issues/1349

    ```
    cd ~/.mozilla/firefox/fifoxxzm.default/
    mkdir chrome
    cd chrome
    vim userChrome.css
    ```

    Then put the following in `userChrome.css`:

    ```
    @namespace url("http://www.mozilla.org/keymaster/gatekeeper/there.is.only.xul");

    #TabsToolbar {
        visibility: collapse !important;
    }
    ```

* [Stylish](https://addons.mozilla.org/en-US/firefox/addon/stylish/)

* [Extension source viewer](https://addons.mozilla.org/en-US/firefox/addon/crxviewer/)


## Installing docker-machine

Purpose of this is to provide easy access to docker containers for the following
purposes:

1. Building / running untrusted code

2. Higher degree of security (VM + docker), useful for ensuring credentials
   aren't leaked. Unfortunately, this does not really help prevent key-logging
   (I am not using Wayland + SELinux)

```
curl -L https://github.com/docker/machine/releases/download/v0.13.0/docker-machine-`uname -s`-`uname -m` >/tmp/docker-machine &&
chmod +x /tmp/docker-machine &&
sudo cp /tmp/docker-machine /usr/local/bin/docker-machine
```

# A bit more setup (2017-11-20)

## Further XMonad setup

Note: These changes are included in the repo and so don't need to be done
manually

* In `.profile`, I set `XMONAD_DATA_DIR` and `XMONAD_CONFIG_DIR` to be
  `$HOME/env`, so that it doesn't use `~/.xmonad`

* Added a `xmonad-config.cabal` file, so that:

  - All dependencies are specified. When just using `stack ghc` without package
    hiding, it's easy to depend on things that just happen to be in the snapshot
    DB.

  - intero works when editing the configuration.

* Split out some functionality into separate modules

* Seems that xmonad reload wants to find an executable on path named "xmonad",
  so did the following:

```
cd ~/.local/bin
ln -s ~/.env/xmonad-x86_64-linux xmonad
```

# Installing gcloud sdk (2017-11-24)

`python -V` to verify 2.7 is installed - it was.

```
cd ~/dl
tar -xvf google-cloud-sdk-180.0.1-linux-x86_64.tar.gz
./google-cloud-sdk/install.sh
```

Added some conditional execution of shell stuff to ~/.profile, to add gcloud to
environment.

# 2017-11-25

## Git hooks to enforce gitignore

Idea is that I'd like to make it very difficult to accidentally use `cfg add
--force` to add ignored files.  I don't want to unintentionally commit
something.  Moreover, I want to use a whitelist.  It is possible to specify a
whitelist in .gitignore.  However, I want to be notified of files that are not
in the blacklist.  So, basically I want both a blacklist and whitelist in order
to ensure the repo only contains intended contents.

## Fixing middle click copy/paste

It seems that alacritty and emacs both require `xclip` in order to support
middle-click paste.  So:

```
sudo apt install xclip
```

# 2017-11-26

## Ensuring wayland is disabled

Uncommented `WaylandEnable=false` in `/etc/gdm3/custom.conf` as described
[here](https://askubuntu.com/questions/961304/how-do-you-switch-from-wayland-back-to-xorg-in-ubuntu-17-10)

## Enabled REISUB

As descibed
[here](https://askubuntu.com/questions/4408/what-should-i-do-when-ubuntu-freezes/36717#36717),
alt+prtsc combination "REISUB" (a few seconds between each press) is safer than
hard power off.

May be worthwhile to know alt+sysreq+f == oom kill

> sudo nano /etc/sysctl.d/10-magic-sysrq.conf

Switch 176 to 244

> echo 244 | sudo tee /proc/sys/kernel/sysrq

# 2017-11-28

## Added firefox tab suspender

https://addons.mozilla.org/en-US/firefox/addon/tab-suspender-tab-unloader/

## Installed slack native client

https://slack.com/downloads/linux

I measured, and this seems to use 0.7 gb more than having the firefox tabs open.
Could be some GC hasn't been triggered yet (I didn't restart firefox).

Note from half a year later: I don't seem to use this, instead just use browser.

# 2018-01-06

## Switched to firefox 57.0.4 instead of custom built

Via https://www.mozilla.org/en-US/firefox/new/?scene=2

# 2018-05-27

## Removed thunderbird

(I just use gmail)

```
sudo apt purge thunderbird
```
## Removed slack desktop

```
sudo apt purge slack-desktop
```

## Setup cli brightness control

https://gitlab.com/wavexx/acpilight/

Did the `/etc/udev/rules.d/90-backlight.rules` thing to allow
userspace write access to the backlight brightness. Also required
assing myself to the `video` group:

```
sudo usermod -a -G video $LOGNAME
```

Unfortunately, the xbacklight utility didn't work for me - not sure
why.  Writing manually to the file works, though, so instead I just
copy modified some code to my xmonad config, that writes to the file.

## Install gist

I realized I no longer used my xmonad gisting key combos, most likely
because I didn't install [gist] on my new machine!

[gist]: https://github.com/defunkt/gist

> sudo gem install gist
