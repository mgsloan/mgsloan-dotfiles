The configuration in the `grub` file has the following changes:

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

The configuration file can be installed via:

```
sudo cp -f ~/env/grub/grub /etc/default/grub
sudo update-grub
```
