These are some notes about how I've setup home dir encryption which
decrypts using your standard login password, so you only have to enter
one password on boot. I found that existing guides didn't work
directly for me, so I cobbled this together, mostly from [a page on
the archlinux
wiki](https://wiki.archlinux.org/title/Dm-crypt/Mounting_at_login).

# Creation of encrypted partition

```
sudo cryptsetup --verify-passphrase luksFormat "$PARTITION"
sudo cryptsetup luksOpen "$PARTITION" encrypted-home
sudo cryptsetup luksHeaderBackup "$PARTITION" --header-backup-file /root/luks.encrypted-home.header
sudo mkdir /mnt/encrypted-home
sudo mkfs -t ext4 /dev/mapper/encrypted-home
```

Set `PARTITION` to `/dev/some-partition`, and `USERNAME` to your
username.  If you want to have multiple encrypted home dirs, consider
a better naming scheme, possibly `home-$USERNAME` instead of
`encrypted-home`.

# Copying files

I forgot to write down the particular commands, but it's pretty
straightforward - mount the home dir and recursively copy. I left the
default home dir in place such that it would be obvious if decryption
/ mounting failed, but a home dir would still be present. (the
mounting is over the original home dir)

# Manual usage

If no automation is added, then the encrypted home directory can be
used via these commands:

```
sudo cryptsetup luksOpen "$PARTITION" encrypted-home
sudo mount /dev/mapper/encrypted-home /home/$USERNAME
```

# Setup for auto-decrypting on login

This was the trickiest part to get working right.

One part is figuring out which file in `/etc/pam.d/` to edit, since
the `/etc/pam.d/system-login` mentioned in the archlinux page does not
exist on ubuntu. Based on https://wiki.ubuntu.com/EncryptedHomeFolder
`/etc/pam.d/common-auth` is a decent place for this. I added:

```
###############################################################################
## mgsloan modifications based on
##
## https://wiki.archlinux.org/title/Dm-crypt/Mounting_at_login
################################################################################
auth optional pam_exec.so expose_authtok /usr/sbin/pam_cryptsetup.sh
```

The contents of `/usr/sbin/pam_cryptsetup.sh` are

```
#!/usr/bin/env bash

PASS=$(cat -)

CRYPT_USER="mgsloan"
PARTITION="/dev/nvme0n1p5"
NAME="encrypted-home"

if [[ "$PAM_USER" == "$CRYPT_USER" && ! -e "/dev/mapper/$NAME" ]]; then
	echo "$PASS" | /usr/sbin/cryptsetup --timeout=1 --tries=1 luksOpen "$PARTITION" "$NAME"
fi
```

This deviates from the script in the archlinux page, as it captures
the password in a variable before providing it to `cryptsetup`. I
found this to be necessary, and I'm not sure why. It also adds the
`--timeout` and `--tries` options so that it fails quickly if the
password is wrong / not provided.

# Setup for auto-mounting of decrypted partition

Then create a file called
`/etc/systemd/system/home-$USERNAME.mount`. There is [an example in
this repo](/env/systemd/home-mgsloan.mount) which works with ext4
(which is what it was formatted as above).

```
[Unit]
Requires=user@1000.service
Before=user@1000.service

[Mount]
Where=/home/mgsloan
What=/dev/mapper/encrypted-home
Type=ext4
Options=defaults

[Install]
RequiredBy=user@1000.service
```
