I recently purchased an F110-G3 tablet, mostly due to its durability,
bright screen, swappable batteries, and relative inexpensiveness
buying used.

# 2021-07-30 Xubuntu install

Since I had an Xubuntu 20.04 install on a thumbstick laying around on
a thumbstick, I installed that.  Process was seemless.  Great job
ubuntu people!

I followed the instructions in [home-dir-git.md](./home-dir-git.md) to
checkout the env dir.  I also checked out my emacs config.

## Installing software

Based on the `setup-log.md` in this repo.

```
sudo apt install curl libcairo2-dev libxinerama-dev libxrandr-dev libxdo-dev libxft-dev htop tmux redshift rxvt-unicode suckless-tools slock powertop feh httpie python3-pip lm-sensors ruby xsel meson scrot byzanz ccze groff cryptsetup gparted

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

## Encrypted home dir

Perhaps I should do security by obscurity by not documenting this
publicly, but oh well :)

Following the approach in
https://blog.sensecodons.com/2019/10/encrypt-your-home-directory-using-luks.html

```
sudo cryptsetup --verify-passphrase luksFormat /dev/sda3
sudo cryptsetup luksOpen /dev/sda3 encrypted-home
sudo cryptsetup luksHeaderBackup /dev/sda3 --header-backup-file /root/luks.encrypted-home.header
sudo mkdir /mnt/encrypted-home
sudo mount /dev/mapper/encrypted-home /mnt/encrypted-home
sudo cp -a /home/mgsloan /mnt/encrypted-home/mgsloan
```

I then skipped the setup of crypttab. Since this is a tablet computer,
I don't want boot to be contingent on the presence of a keyboard
(though, I will have a keyboard 99% of the time)

I then found
https://blog.trifork.com/2020/05/22/linux-homedir-encryption/, which
happily is compatible with what I've already setup.
