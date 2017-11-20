# Michael Sloan's dotfiles

This is my hacked together computer configuration, centered around an xmonad config.

## Particular choices

### No symlink manager - straight git

This repo is intended to be cloned with `--bare`, and have its working directory be `~/`.  There are some instructions for this [here](env/setup-notes.md)

### Environment variables to enable computer specific settings

As much as possible, I want to be able to still use this repo with my old
computer setup. I didn't want to fuss around with also reinstalling things on
that drive / machine.

To facillitate this, there's script in [`env/settings.sh`](`env/settings.sh` ) which sets environment variables to determine the following:

* `USE_HIDPI`, to enable various mechanisms for rescaling applications. Ubuntu
  17.10 has pretty nice support for hidpi. Unfortunately, this support relies on
  the wayland display server, which is not compatible with xmonad. I might give
  Ongy's [waymonad](https://github.com/Ongy/waymonad) a try one of these days,
  looks like a potential solution to this!
