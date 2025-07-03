Switched to Debian :) Going to see how long I can stick with Debian 12, likely to update to unstable.

> If your graphic card was manufactured in 2007 or later, try uninstalling the xserver-xorg-video-intel package and use the builtin modesetting driver (xserver-xorg-core) instead.

```bash
sudo apt remove xserver-xorg-video-intel
```

Updated `/etc/apt/sources.list` to have `main contrib non-free non-free-firmware` for each source

`nvidia-detect` says to install `nvidia-driver`
