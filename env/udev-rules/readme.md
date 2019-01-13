Copy these into `/etc/udev/rules.d/` to use, like so:

```
sudo cp -f *.rules /etc/udev/rules.d
```

They can then be applied without restart by doing:

```
sudo udevadm control --reload-rules
```
