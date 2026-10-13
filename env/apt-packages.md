# APT packages

# Read by freshen: one package per line.
# Programs not supplied by Nix; development libraries only for ShoopDaLoop.

## Programs

apt-file
clang
g++
gcc
libnotify-bin
pkgconf
pm-utils
podman
swaylock
synaptic

# gcc and g++ retain the compilers previously installed by build-essential.
# Debian's pkgconf supplies pkg-config; the Nix profile supplies only pkgconf.
# libnotify-bin supplies /usr/bin/notify-send for udev as well as user scripts.
# pm-utils is retained, though the tracked configuration uses systemd and logind.

## ShoopDaLoop development

# Use rustup's toolchain and cargo build in ~/oss/shoopdaloop.

libasound2-dev
libjack-jackd2-dev
libgl-dev
libx11-dev
libxcb-render0-dev
libxcb-shape0-dev
libxcb-xfixes0-dev
libxcursor-dev
libxi-dev
libxkbcommon-dev
libxrandr-dev
libwayland-dev

# jack-sys needs jack.pc and the libjack.so linker symlink even with dynamic
# loading; libjack-jackd2-dev provides these, while the runtime package does not.
