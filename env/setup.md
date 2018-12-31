# Computer setup notes

This is **not** a tutorial. I'm not going to explain much. Instead,
this is just notes about how I've setup my computer, and generally the
changes I've made to my setup. I think a lot of the stuff is good
here, and so may be interesting to you, but of course I do not certify
that everything here is safe or ideal.

# 2018-12-23: Initial fresh install

I ran the live cd for Ubuntu 18.04.1, and ran ubuntu rather than doing
the install directly.

## Software raid for filesystem

In theory, my laptop has hardware raid.  However, I recall from a year
or so ago struggling to even get Linux live CDs to boot while the
UEFI + harware raid was on. Disabling both resolved the issue.

So, for this fresh install I wanted to use software raid at boot time.
I mostly followed this old forum thread:
https://ubuntuforums.org/showthread.php?t=1551087

I created the following partitions on my 256gb nvme ssds:

```
/nvme0n1p1 /boot (ext4)
/nvme0n1p2 swap1 (unformatted)
/nvme0n1p3 root1 (unformatted)

/nvme0n1p1 /misc (ext4)
/nvme0n1p2 swap2 (unformatted)
/nvme0n1p3 root2 (unformatted)
```

Both `/boot` and `/misc` were set to `1gb` size.

Both `swap1` and `swap2` were set to `43008mb` (`42gb`).  I later
realized that the swap partitions were somewhat of a mistake.  I made
them so large because I wanted to hibernate my full `40gb` of ram.
However, I hadn't accounted for raid 0 combining the partition sizes,
so a full `84gb` of swap, whcih is way too much.  Also, these days I
think it makes more sense to have a swap file rather than a swap
partition.  So anyway, continuing this with what I actually did.  I've
later decided to repurpose the swap partition as an encrypted raid
partition.

```
sudo mdadm --create /dev/md0 --verbose --level=0 --raid-devices=2 /dev/nvme0n1p2 /dev/nvme1n1p2
sudo mdadm --create /dev/md1 --verbose --level=0 --raid-devices=2 /dev/nvme0n1p3 /dev/nvme1n1p3

sudo mkfs.ext4 /dev/md1
sudo mkswap /dev/md0
```

(Note: md0 and md1 are flipped w.r.t. the forum post - just a consequence of how I ordered the partitions)

## In ubuntu installer

* Minimal installation

* Install third-party software for graphics and Wi-Fi hardware and
  additional media formats

* Formatted /dev/md1 as ext4 mounted to /

* Formatted /dev/md0 as swap

* Formatted /dev/nvme0n1p1 as ext4 mounted to /boot

* Selected /dev/nvme0n for boot partition

## After installation

It took a couple tries to get this booting properly.  While still in
the live cd, I did the following, based on the post installation
instructions:

```
sudo su

# Mount the install target
mount /dev/md1 /target

# Bind mount stuff from live cd OS over stuff in the target, so that chroot works
for f in sys proc dev dev/pts; do mount --bind $f /target/$f; done

# Mount the boot partition
u=`blkid -o value -s UUID /dev/sda1`
mkdir /media/$u
mount -t ext4 /dev/sda1 /media/$u
mount --bind /media/$u /target/boot
```

In order to access the internet within the chroot, I copied over
`resolv.conf` as described in
https://unix.stackexchange.com/questions/280500/unable-to-reach-network-from-chroot

```
cp /etc/resolv.conf /target/etc/resolv.conf
```

Finally, I could enter the chroot, essentially pretending to be
running inside the newly installed filesystem, and install mdadm to
it.  Why this isn't included in the install by default (or by option)
is beyond me.

```
chroot /target
apt update
apt install mdadm
```

## Reassembling raid in live cd

Since this took multiple tries, sometimes I needed to re-assemble the
raid 0 arrays while running the live cd. I also needed to create
`/target`.  From memory I think the commands were:

```
sudo mdadm --assemble --verbose /dev/md0 /dev/nvme0n1p2 /dev/nvme1n1p2
sudo mdadm --assemble --verbose /dev/md1 /dev/nvme0n1p3 /dev/nvme1n1p3
cd /
mkdir target
```

# 2018-12-23: Building / installing software

```sh
# Emacs from source
sudo apt install build-essentials
sudo apt build-dep emacs25
cd ~/oss/emacs
./autogen.sh
./configure --with-x-toolkit=gtk3 --prefix=$HOME/.local
make -j8
make install

# stack installation
sudo apt install curl
curl -sSL https://get.haskellstack.org/ | sh

# xmonad configuration from source
cd ~/env
stack build -j8

# keynav from source
cd ~/oss/keynav
sudo apt install libcairo2-dev libxinerama-dev libxdo-dev
make

# hub installation
# (check https://github.com/github/hub/releases for latest url)
cd dl
curl https://github.com/github/hub/releases/download/v2.6.1/hub-linux-amd64-2.6.1.tgz
tar -xvf hub-linux-amd64-2.6.1
cd hub-linux-amd64-2.6.1
prefix=~/.local ./install

# hack font installation (referenced by emacs configuration)
cd dl
wget https://github.com/source-foundry/Hack/releases/download/v3.003/Hack-v3.003-ttf.zip
unzip Hack-v3.003-ttf.zip
cd Hack-v3.003-ttf
sudo mv * /usr/share/fonts
wget https://raw.githubusercontent.com/source-foundry/Hack/master/config/fontconfig/45-Hack.conf
sudo mv 45-Hack.conf /etc/fonts/conf.d/
fc-cache -f -v
# confirm it's installed
fc-list | grep Hack

# rust installation
curl https://sh.rustup.rs -sSf | sh

# chrome installation
cd ~/dl
wget https://dl.google.com/linux/direct/google-chrome-stable_current_amd64.deb
sudo dpkg -i google-chrome-stable_current_amd64.deb

# keybase installation
# https://keybase.io/docs/the_app/install_linux
cd ~/dl
curl -O https://prerelease.keybase.io/keybase_amd64.deb
sudo dpkg -i keybase_amd64.deb
sudo apt-get install -f
run_keybase

# ripgrep installation
cd ~/oss
git clone BurntSushi/ripgrep
cd ripgrep
cargo install

# other utilities
sudo apt install htop tmux camorama redshift rxvt-unicode
```

# 2018-12-24: More setup

```sh
# creation of xdg directories
mkdir .xdg
mkdir .xdg/desktop
mkdir .xdg/public
mkdir .xdg/templates

# installation of gnome session for xmonad + gnome flashback
cd ~/env/gnome-session-xmonad
./minimal-install.sh

# more misc utilities
sudo apt install suckless-tools
```

# 2018-12-25: Setup continued

```sh
sudo apt install slock powertop feh httpie python3-pip mesa-utils
```

# 2018-12-25: Using slock for login password and login-after-suspend

I thought it'd be quite nice to be able to start up applications
concurrently with typing in my login password.  I'm not sure how to do
this with gdm, it may well be impossible, so instead I did the following:

* Enabled autologin by setting the following in
  `/etc/gdm3/custom.conf`:

```
AutomaticLoginEnable = true
AutomaticLogin = mgsloan
```

* Made xmonad spawn `slock` first thing on start. I know this is
  highly iffy as a security mechanism, but I think it's good enough
  for my purposes. I like the obscurity of the whole screen changing
  colors depending on the state of slock.

* I wanted to reliably run slock on suspend, so I followed the
  instructions
  [here](https://wiki.archlinux.org/index.php/Slock#Lock_on_suspend),
  particularly, putting the following in
  `/etc/systemd/system/slock@.service`:

```
[Unit]
Description=Lock X session using slock for user %i
Before=sleep.target

[Service]
User=%i
Environment=DISPLAY=:0
ExecStartPre=/usr/bin/xset dpms force suspend
ExecStart=/usr/bin/slock

[Install]
WantedBy=sleep.target
```

This was then enabled via:

```sh
systemctl enable slock@mgsloan.service
```

# 2018-12-25: More xmonad setup

I'm no longer attempting to use the `GNOME + XMonad`, since it doesn't
seem to work for me.  See some [extensive discussion
here](https://github.com/Gekkio/gnome-session-xmonad/issues/10). So, I
removed it:

It turned out that all I needed was the session / desktop installed by
`sudo apt install --no-install-recommends xmonad`.

I also made sure that my xmonad gets used on startup by doing the
following:

```sh
cd /usr/bin
sudo rm xmonad
sudo ln -s ~/.xmonad/xmonad-x86_64-linux xmonad
```

It's seems like poor practice to link into a home dir from
`/usr/bin/`, but oh well.

By default, `xmonad` tries to recompile on start if its name isn't
`xmonad-x86_64-linux` (and this check doesn't seem to look through
symlinks). I've patched my version of xmonad to not do this, to make
boot faster.

# 2018-12-25: More fine tuning of boot

I wanted to immediately skip grub, but still have the option of
entering the menu by holding shift.  Unfortunately, I couldn't get
this to work despite trying the following 2 approaches:

- [Checking keystatus --shift, setting timeout if test passes](https://ask.fedoraproject.org/en/question/52450/how-to-get-to-rescue-mode-when-grub-timeout-is-0/?answer=52536#post-id-52536).

- [31_hold_shift
  script](https://wiki.archlinux.org/index.php/GRUB/Tips_and_tricks#Hide_GRUB_unless_the_Shift_key_is_held_down).
  This script is complicated, and `sudo update-grub` failed with it.

So, instead I kept my tuning to `/etc/default/grub`.  In particular:

The following settings make the grub menu appear for 1 second before
automatically choosing the default boot:

```
GRUB_TIMEOUT_STYLE=menu
GRUB_TIMEOUT=1
```

I prefer text boot and shutdown, instead of showing a graphical
booting screen. This setting

```
GRUB_CMDLINE_LINUX_DEFAULT="text"
```

The next setting I'm not sure about.  I noticed that `30_os-prober`
can be disabled via the following environment variable.  It seems like
it might be more efficient to skip it, and I don't seem to need
it. So:

```
GRUB_DISABLE_OS_PROBER="true"
```

The configuration file is included in this repo, and can be installed
via:

```
sudo cp -f ~/env/grub/grub /etc/default/grub
sudo update-grub
```

# 2018-12-25: Removing snapd

Snaps seem to show up during boot and in `df` as loopback
devices. Yuck!

```
sudo apt purge snapd squashfs-tools gnome-software-plugin-snap
```

# 2018-12-25: Removing other unnecessary boot stuff

```
sudo systemctl disable postfix.service
sudo systemctl disable avahi-daemon.service
```

# 2018-12-25: Userspace backlight control

```
sudo cp ~/env/udev-rules/90-backlight.rules /etc/udev/rules.d/
sudo usermod -a -G video $LOGNAME
```

# 2018-12-30: Installing nvidia drivers

```
sudo apt install nvidia-driver-390
```

It will then default to using the discrete graphics, which means more
power consumption, but also grants the capability to use a 4K external
monitor. Running `sudo nvidia-settings`, and selecting `Prime
Profiles` can allow switching between the two (requires a restart).
This is particularly useful when traveling, because that's when I
value reduced power consumption and don't care about the other
outputs.

# 2018-12-30: More utilities

```
sudo apt install lm-sensors
```

# 2018-12-30: More boot service removal

This service held up shutdown once, so removing it. I don't need
remote printers.

```
sudo systemctl disable cups-browsed.service
```
