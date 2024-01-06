# Setup encrypted home dir

As described in [encrypted-home-dir.md](./encrypted-home-dir.md).

# Steps that now have scripts

The steps under this section are now available in
[setup-scripts/](./setup-scripts/).

## Configuring git

```
git config --global user.name "Michael Sloan"
git config --global user.name "mgsloan@gmail.com"
```

## Clone homedir repo

```
git clone --bare https://github.com/mgsloan/mgsloan-dotfiles.git .home.git
export GIT_DIR="$PWD/.home.git"
export GIT_WORK_TREE=$PWD
git config core.bare false
git config core.logAllRefUpdates true
git config core.workdir ../
git reset HEAD -- .
git status --porcelain | awk '$1 == "D" {print $2}' | xargs git checkout HEAD --
```

This shell then gets closed since it has modified environment
variables that interfere with other git usage.

## Clone emacs repo

```
git clone git@github.com:mgsloan/mgsloan-emacs.git .emacs.d
cd .emacs.d
git submodule init
git submodule update --recursive
```

## Installing packages

I've automated some part of my setup process, in an [idempotent script
called `freshen`](/.local/bin/freshen), which I plan to continue to
update. This script does the following:

* Installs all apt packages listed in
  [`apt-packages.md`](/env/apt-packages.md)

* Installs all snap packages listed in
  [`snap-packages.md`](/env/snap-packages.md)

* Downloads and installs `google-chrome`, `stack`, `run_keybase`,
  `zoom`, `roam-to-git` if not already installed.

* Probably more, I plan to extend this script without updating the
  text here.

## Updating grub config

This config uses text login and a timeout of 1 second.

Make sure the changes are sensible:

```
diff ~/env/grub/grub /etc/default/grub
```

Then apply:

```
cp -f ~/env/grub/grub /etc/default/grub
```

## Userspace backlight control

```
sudo cp ~/env/udev-rules/90-backlight.rules /etc/udev/rules.d/
sudo usermod -a -G video $LOGNAME
```

## Removing unnecessary services

Identified in a prior setup log

```
sudo systemctl disable postfix.service
sudo systemctl disable avahi-daemon.service
sudo systemctl disable cups-browsed.service
```

## Custom browser extensions

Since I wrote the code for these, I typically use them unpacked rather
than from the chrome webstore.

```
cd ~/proj/
git clone mgsloan/todoist-shortcuts
git clone mgsloan/roam-navigator
git clone mgsloan/unblock-with-intention
git clone mgsloan/gmail-label-switch-shortcuts
```

## Creating xdg dirs

```
mkdir -p .xdg/desktop
mkdir .xdg/templates
mkdir .xdg/public
```

# Unscripted steps

## Initial configuration build from source

```
cd ~/env
stack build -j8
```

# Power tuning via powertop

```
powertop
```

* Kept VM writeback timeout "Bad" in tunables, as I want prompt disk
  writes

* Didn't want usb autosuspend

# Setting up hub cli tool

[Add new token on github](https://github.com/settings/tokens), then
paste in as "password" when making first use of `hub` tool.

```
git config --global hub.protocol https
```

# Updating ubuntu version

Tired of encountering issues due to older versions of packages, so I
updated to a non-LTS ubuntu, 21.10.

```
sudo do-release-upgrade
```

# 2021-11-29

# Switching from lightdm to gdm3

Starting with xubuntu instead of ubuntu has been a bit of a misadventure...

Specifically, any running of xmonad would immediately exit or malfunction. I suspect it is similar to the issue discussed in https://forum.endeavouros.com/t/warning-possible-crash-with-xorg-server-1-20-12-1-and-lightdm/15789/14

From https://askubuntu.com/questions/152256/how-do-i-switch-from-lightdm-to-gdm

```
sudo dpkg-reconfigure gdm3
```

And setting gdm as the default seems to have done the trick!

# Abortative attempt at use of git-credential-manager

Previously I've checked out repos using ssh, but this can be a PITA
when submodules are involved, and doesn't work properly for
others. So, it is sensible to switch to using HTTPS.

I came across [GitHub's docs on
this](https://docs.github.com/en/get-started/getting-started-with-git/caching-your-github-credentials-in-git)
and they recommended `git-credential-manager`.  The first smell was
that there is no ppa or apt package. The next smell was that this
simple thing is packaged as a 70mb deb. Then, it simply did not work
at all:

```
  An assembly specified in the application dependencies manifest (GitHub.UI.deps.json) was not found:
    package: 'HarfBuzzSharp.NativeAssets.Linux', version: '2.6.1.7'
    path: 'runtimes/linux-x64/native/libHarfBuzzSharp.so'
fatal: helper error (140): Unknown
```

At this point, I uninstalled it. This is mostly a note for my future
self to not try using this again.

# Using gnome libsecret git credential storage

From [a helpful
article](https://www.softwaredeveloper.blog/git-credential-storage-libsecret#libsecret-git-credential-storage-by-gnome)
I found these instructions:

```
sudo apt-get install libsecret-1-0 libsecret-1-dev
cd /usr/share/doc/git/contrib/credential/libsecret
sudo make
git config --global credential.helper /usr/share/doc/git/contrib/credential/libsecret/git-credential-libsecret
```

Boom! 200Kb later the problem is solved, I just enter credentials once
and we're good.

**Update:** This is now included in the setup-scripts.

# 2021-12-27

# Storing roam username / password in keyring

(for use by `.local/bin/roam-backup` script)

```
secret-tool store --label='Roam research user email' roam user
secret-tool store --label='Roam research password' roam password
secret-tool store --label='Roam research database' roam database
```

# Getting authy working

For some reason the authy snap starts and fails immediately. I found
out that you can install snaps with AppArmor disabled, instead logging
the issues to the journal. This fixed the issue. See [this gist for
the journalctl
logs](https://gist.github.com/mgsloan/735189edc027322685365909cdeb5787). Of
course, it would be best to not use `--devmode`, but I'm happy that
this fixes the issue.

# 2022-03-04 move downloads cron

To keep downloads folder tidy, added a cron to move files out of
folder. Creating this file is scripted by
`env/setup-scripts/041-create-move-downloads.sh`
