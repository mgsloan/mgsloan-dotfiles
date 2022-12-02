This dir contains nearly identical custom layouts for the Kinesis
Advantage 2, Kinesis Advantage 2 KinT controller, Kinesis Advantage
360, and Kinesis Advantage 360 Pro.

# Kinesis Advantage 2

`./generate.hs Adv2` to generate `m_qwerty.txt`. The script will also
attempt to copy it to the typical mount point.

# Kinesis Advantage 360 ("SmartSet")

`./generate.hs Adv360` to generate `layout1.txt`. The script will also
attempt to copy it to the typical mount point.

# Kinesis Advantage 360 Pro (ZMK)

`adv360.keymap` contains the keymap which can be used with [this
repo](https://github.com/KinesisCorporation/Adv360-Pro-ZMK) to build
the firmware.

# Kinesis Advantage 2 KinT

One of my kinesii had its controller fail, so I replaced it with a
Teensy! This was made possible by the
[KinT](https://github.com/kinx-project/kint)
project. The steps are:

* Load `kint2pp.json` into [QMK configurator](https://config.qmk.fm/).

* Compile and download the firmeware to `kin2pp.hex`.

* Flash it by running `./kint2pp.sh` and pressing the `progm` button
  in the upper right corner.
