I went with a different strategy than the original `encrypted-home-dir.md` file when setting up a new computer.

# Creation of encrypted btrfs

Simply ran `gnome-disks` and used the UI to set it up.

FIXME: continue

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
