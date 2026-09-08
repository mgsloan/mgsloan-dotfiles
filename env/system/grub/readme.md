The configuration in the `grub` file has the following changes:

The following settings make the grub menu appear for 1 second before
automatically choosing the default boot:

```
GRUB_TIMEOUT_STYLE=menu
GRUB_TIMEOUT=1
```

I prefer text boot and shutdown instead of a graphical boot screen. Zswap
keeps frequently accessed swapped pages compressed in RAM:

```
GRUB_CMDLINE_LINUX_DEFAULT="text zswap.enabled=1 zswap.compressor=zstd zswap.max_pool_percent=20"
```

Enable os-prober so the Windows installation appears in the GRUB menu:

```
GRUB_DISABLE_OS_PROBER=false
```

The configuration file can be installed via:

```
sudo cp -f ~/env/system/grub/grub /etc/default/grub
sudo update-grub
```
