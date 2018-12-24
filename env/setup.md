# Computer setup notes

This is **not** a tutorial. I'm not going to explain much. Instead, this is just
notes about how I've setup my computer, and generally the changes I've made to
my setup. I think a lot of the stuff is good here, and so may be interesting to
you, but of course I do not certify that everything here is safe or ideal.

# 2018-12-23: Initial fresh install

I ran the live cd for Ubuntu 18.04.1, and ran ubuntu rather than doing the install directly.

## Software raid for filesystem

In theory, my laptop has hardware raid.  However, I recall from a year or so ago struggling to even get Linux live CDs to boot while the UEFI + harware raid was on. Disabling both resolved the issue.

So, for this fresh install I wanted to use software raid at boot time.  I mostly followed this old forum thread: https://ubuntuforums.org/showthread.php?t=1551087

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

Both `swap1` and `swap2` were set to `43008mb` (`42gb`).  I later realized that the swap partitions were somewhat of a mistake.  I made them so large because I wanted to hibernate my full `40gb` of ram.  However, I hadn't accounted for raid 0 combining the partition sizes, so a full `84gb` of swap, whcih is way too much.  Also, these days I think it makes more sense to have a swap file rather than a swap partition.  So anyway, continuing this with what I actually did.  I've later decided to repurpose the swap partition as an encrypted raid partition.

```
sudo mdadm --create /dev/md0 --verbose --level=0 --raid-devices=2 /dev/nvme0n1p2 /dev/nvme1n1p2
sudo mdadm --create /dev/md1 --verbose --level=0 --raid-devices=2 /dev/nvme0n1p3 /dev/nvme1n1p3

sudo mkfs.ext4 /dev/md1
sudo mkswap /dev/md0
```

(Note: md0 and md1 are flipped w.r.t. the forum post - just a consequence of how I ordered the partitions)

## In ubuntu installer

* Minimal installation
* Install third-party software for graphics and Wi-Fi hardware and additional media formats
* Formatted /dev/md1 as ext4 mounted to /
* Formatted /dev/md0 as swap
* Formatted /dev/nvme0n1p1 as ext4 mounted to /boot
* Selected /dev/nvme0n for boot partition

## After installation

It took a couple tries to get this booting properly.  While still in the live cd, I did the following, based on the post installation instructions:

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

In order to access the internet within the chroot, I copied over `resolv.conf` as described in https://unix.stackexchange.com/questions/280500/unable-to-reach-network-from-chroot

```
cp /etc/resolv.conf /target/etc/resolv.conf
```

Finally, I could enter the chroot, essentially pretending to be running inside the newly installed filesystem, and install mdadm to it.  Why this isn't included in the install by default (or by option) is beyond me.

```
chroot /target
apt update
apt install mdadm
```

## Reassembling raid in live cd

Since this took multiple tries, sometimes I needed to re-assemble the raid 0 arrays while running the live cd. I also needed to create `/target`.  From memory I think the commands were:

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
```
