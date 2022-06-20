# Vanilla Kinesis Controller

This contains a script, `generate.hs`, to generate the layout for my
Kinesis Advantage 2 Keyboard, readable by the standard firmware. The
motivation to use a script was so that I could intersperse whitespace
/ comments.

# KinT Kinesis Controller

One of my kinesii had its controller fail, so I replaced it with a
Teensy! This was made possible by the
[KinT](https://github.com/kinx-project/kint)
project. `kinesis_kint2pp_layout_mine.json` contains a layout
compatible with the [QMK configurator](https://config.qmk.fm/).  Once
a `.hex` file has been compiled, I load it via:

```
teensy_loader_cli --mcu=TEENSY2PP -w kinesis_kint2pp_kinesis_kint2pp_layout_mine.hex
```

I then hit the `progm` button in the upper right corner to flash the
keyboard.
