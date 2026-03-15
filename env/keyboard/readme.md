This dir contains nearly identical custom layouts for the Kinesis
Advantage 2, Kinesis Advantage 2 KinT controller, Kinesis Advantage
360, Kinesis Advantage 360 Pro, and SliceMK ergodox 

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

# SliceMK ErgoDox Wireless LP

These are instructions idiosyncratic to my setup:

1. Upload `slicemk_keymap.json` to [the configurator](https://config.slicemk.com/zmk/keymap/?keyboard=ergodox&layout=QWERTY).
2. Compile and download binary for Raytac MDBT50Q-RX Green (USB A). GREEN is important
3. Hold button on dongle and insert.
4. Copy / move the uf2 file to the dongle.

Sometimes it seems like the firmware doesn't update. For some reason loading up nvsclear fixes it.  See [Bond Reset](https://docs.slicemk.com/firmware/zmk/wireless/nvsclear/)

[peripheral firmware downloads](https://docs.slicemk.com/keyboard/ergodox/peripheral/)
