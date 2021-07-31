I recently purchased an F110-G3 tablet, mostly due to its durability, bright screen, swappable batteries, and relative inexpensiveness buying used.

# 2021-07-30 Xubuntu install

Since I had an Xubuntu 20.04 install on a thumbstick laying around on a thumbstick, I installed that.  Process was seemless.  Great job ubuntu people!

I followed the instructions in [home-dir-git.md](./home-dir-git.md) to checkout the env dir.  I also checked out my emacs config.

## Installing software

Based on the `setup-log.md` in this repo.

```
sudo apt install curl libcairo2-dev libxinerama-dev libxrandr-dev libxdo-dev htop tmux redshift rxvt-unicode suckless-tools slock powertop feh httpie python3-pip lm-sensors ruby xsel meson scrot byzanz ccze groff

curl -sSL https://get.haskellstack.org/ | sh

cd ~/env
stack build -j8
```

## Removing unnecessary boot stuff

Based on old notes

```
sudo systemctl disable avahi-daemon.service
sudo systemctl disable cups-browsed.service
```
