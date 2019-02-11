This repo contains my computer configuration files! The code and
configuration here is covered by the MIT license - see [the text
here](env/LICENSE).

# [XMonad] configuration

Particularly highlighting stuff that may be interesting for others to
use:

## Logging and process running

Logging and process running is implemented by using [rio] package. To
integrate nicely with rio, a couple monads are defined:

* An [`XX` monad][Monad.hs], which wraps the [`X`
  monad](https://www.stackage.org/haddock/lts/xmonad/XMonad-Core.html#t:X)
  and adds a reader context compatible with rio.

* Similarly, a [`Xio` monad][Monad.hs], which wraps the `IO` monad in
  the same reader context.

There are also process running utilities which log process start and
end. In particular:

* The `spawn` function asynchronously spawns a process, similarly to
  the standard xmonad function of the same name.

* The `syncSpawn` function synchronously spawns a process. Its type is
  intentionally restricted to run in `Xio`,

* The `syncSpawnAndRead` function runs a process and reads its output.

* Implements a couple utilities inspired by [XMonad.Actions.SpawnOn] -
  `spawnOn` and `spawnAndDo`. These utilities record the spawned
  process ID and associate it with a `ManageHook`. The configuration
  uses this to spawn applications on particular workspaces at startup.

* If a sanity check of [systemd-cat] succeeds on startup, then it gets
  used to wrap the processes. This way the logs of all processes end
  up in the systemd log. One cool thing this allows is bringing up the
  logs for a particular window on request (currently bound to `M-y`).

* These utilities rely on the `SIGCHLD` patch to `xmonad`, described
  in a section below.

## Startup

At startup, some standard stuff like a browser and emacs get run. The
following also gets run:

* [dunst], a minimal notification display daemon. My configuration is
  located at [`.config/dunst/dunstrc`](.config/dunst/dunstrc). I wrote
  a couple patches which this config uses - [dunst#590] and [dunst#598].

* [keynav], a utility to use the keyboard to perform mouse actions. I
  find this most useful for putting focus on things and sending scroll
  events. My configuration is located at [`.keynavrc`](.keynavrc).

* [redshift], a utility to adjust screen color temperature at night.

* A terminal using systemd [journalctl], on workspace `9`, to display
  all error messages since boot. These errors get filtered by
  [env/errlog-filter.hs] - the goal being to investigate errors
  encountered and hide them once the conclusion is that they are
  benign. I don't yet have an exhaustive list of benign errors
  outputted by my computer.

* A terminal using systemd [journalctl], also on workspace `9`, to
  display the most recent logs.

* A terminal running `bluetoothctl`, also on workspace `9`, for cli
  control of bluetooth devices. Specifically, [Bluetooth.hs] has some
  utilities for connecting and disconnecting devices which have been
  paired. It uses `tmux send-keys` to enter commands into the
  `bluetoothctl` terminal. For some reason, non-interactive cli
  approaches to controlling bluetooth didn't work properly for me.

* A terminal running `nmtui`, also on workspace `9`, for controlling
  internet connections, particularly wifi.  I've found `nmtui` to be
  more reliable than using GUI interfaces, and more convenient than
  using `nmcli`.

## Screen lock on startup and after sleep

Probably more effort than it was worth, but I liked the idea of the
startup applications starting up in parallel with the password
screen. To do this, I enabled gdm autologin, and immediately start
`slock` on start of `xmonad`. See [ScreenLock.hs].

I also wanted to require the password after resuming from
sleep. Installing
[env/systemd/slock@.service](env/systemd/slock@.service) causes
`slock` to be run before sleeping.

## Other bits of code

* [Audio.hs] has utilities for adjusting volume and toggling the
  microphone via use of `amixer`.  It also uses `notify-send` to
  report the state of the audio.

* [Background.hs] has utilities for selecting a random image stored in
  `~/env/untracked/backgrounds` and using [feh] to set the desktop
  background.

* [Brightness.hs] has utilities for adjusting laptop screen brightness
  by writing to `/sys/class/backlight/intel_backlight/brightness`
  (works for thinkpad screens). User access to this is enabled by
  copying
  [`env/udev-rules/90-backlight.rules`](env/udev-rules/90-backlight.rule).

* [byzanz] is a utility for capturing gif recordings of the
  screen. [Byzanz.hs] has code to run [env/byzanz-record-region.sh], a
  script using [xrectsel] and [byzanz] to record a region of the
  screen.  The script also uses [imagemagick] to downscale the
  captured gif by a factor of 2 if `HIDPI` is set.

* [Scrot.hs] has a utility for running [scrot], taking a screenshot of
  a rectangular region of the screen. Also uses [imagemagick] to
  downscale the captured screenshot by a factor of 2 if `HIDPI` is
  set.

* [Gist.hs] runs [gist] to create GitHub gists of the current
  selection, and opens the gist in the browser.

* [RedShift.hs] has code for running redshift and allowing toggling it
  (by a rather crude mechanism - killing and starting).

* [Spotify.hs] has utilities for dbus control of spotify - basic stuff
  like play / pause / next / previous.

* [TallWheel.hs] is just like the built in xmonad `Tall` layout, but

* [Touchpad.hs] uses xinput to toggle the enabling / disabling of a
  thinkpad touchpad. The touchpad is initially disabled.

* [Prompt.hs] provides prompts for running shell actions
  (`shellPrompt`) or xmonad actions (`actionPrompt`). `actionPrompt`
  is used for the xmonad actions that I use quite infrequently, and so
  shouldn't have keybindings.

# No symlink manager, just git

This dotfiles repo uses git directly instead of having an install
procedure or symlink manager. This has quite a few benefits:

* Simplicity - no configuration needed.

* It helps keep my home directory tidy, as new files and folders
  either get added to this repo, deleted, or added to
  [`.gitignore`](.gitignore).

* No need to research, compare and contrast symlink management
  approaches.

This might initially seem like a bad idea, since `git` searches parent
folders for `.git` repos, and so commands might inadvertantly apply to
the home dir repo. There's a pretty good solution to this, though it
requires a bit of setup. See
[`env/home-dir-git.md`](env/home-dir-git.md) for more info on this.

# Upstream patches

The submodules referenced by this repo have a few non-standard patches
applied. Some of these are in the form of PRs which will hopefully be
merged, and some already have been merged.

## xmonad patches

This configuration uses a [patched xmonad] - the submodule is located
at `env/xmonad`.  At time of writing this readme, it has the following
patches:

* XMonad by default installs a `SIGCHLD` handler which does
  nothing. The problem with this is that it means that the typical
  process functions don't work properly if you want to wait for
  process exit. Since I want to have proper process handling and
  logging, `SIGCHLD` can't get ignored. The comments in the code
  indicate that it was ignored to avoid zombie processes, but I
  haven't seen them accumulate. Even when zombies do occur, they are
  pretty benign.
  [The patch](https://github.com/mgsloan/xmonad/commit/8b9aa8a28216051976bc5191ae44b0f8250e3f61)

* Omits implicit recompile on XMonad start.
  [The patch](https://github.com/mgsloan/xmonad/commit/6ef310bdebbfd4c088895bbe566a7493cd91c5af)

* Exposes `sendRestart` function, which seems to be a much nicer way
  to handle restarting XMonad, as it doesn't require running another
  xmonad process just to send the signal.
  [The patch](https://github.com/mgsloan/xmonad/commit/594f6b9f9e00fb94f8e4f543a493843acce11e28)

* Exposes constructor for the `X` monad, which allowed me to define
  `MonadThrow` and `MonadCatch` instances for it.
  [The patch](https://github.com/mgsloan/xmonad/commit/d0208e2a363e60907be6f96641bafc69c41dc55c)

## xmonad-contrib patches

Uses a [patched xmonad-contrib] - the submodule is located at
`env/xmonad-contrib`.  At time of writing this readme, it has the
following patches:

* A PR I wrote to make XMonad.Prompt more reliable on exceptions:
  [xmonad-contrib#287]

* A significant optimization to extended window manager hooks written
  by [bgamari] but not yet merged: [xmonad-contrib#263]

* A change to XMonad.Prompt to make it ignore more keymasks. Some
  users of XMonad may need these keymasks, so it's not a change
  everyone might want.  See discussion in [xmonad-contrib#290].

## systemd-cat patch

I wrote a [small patch of systemd-cat][systemd#11336], which allows
using different log levels for stdout and stderr. In particular, with
my config, `stderr` gets logged with `err` level and `stdout` gets
logged with `info` level.

# Other stuff

* [Log of setup notes][setup-log.md] - specific steps I've taken to
  setup my computer.

* [Separate repository for emacs configuration].

* [`.bashrc`](.bashrc) and [`.profile`](.profile) don't have anything
  particularly special.  The main thing to note is that `.profile`
  loads `~/env/untracked/settings.sh` if it exists.  The purpose of
  this is to set `HIDPI`, which in turn will cause some settings
  specific to using high pixel density displays.

[Audio.hs]: env/src/Audio.hs
[Background.hs]: env/src/Background.hs
[Bluetooth.hs]: env/src/Bluetooth.hs
[Brightness.hs]: env/src/Brightness.hs
[Byzanz.hs]: env/src/Byzanz.hs
[Monad.hs]: env/src/Monad.hs
[Prompt.hs]: env/src/Prompt.hs
[RedShift.hs]: env/src/RedShift.hs
[ScreenLock.hs]: env/src/ScreenLock.hs
[Scrot.hs]: env/src/Scrot.hs
[Gist.hs]: env/src/Gist.hs
[Separate repository for emacs configuration]: https://github.com/mgsloan/mgsloan-emacs
[Spotify.hs]: env/src/Spotify.hs
[TallWheel.hs]: env/src/TallWheel.hs
[Touchpad.hs]: env/src/Touchpad.hs
[XMonad.Actions.SpawnOn]: https://www.stackage.org/haddock/lts/xmonad-contrib/XMonad-Actions-SpawnOn.html
[XMonad]: https://xmonad.org/
[bgamari]: http://github.com/bgamari
[byzanz]: https://manpages.debian.org/testing/byzanz/byzanz-record.1.en.html
[dunst#590]: https://github.com/dunst-project/dunst/pull/590
[dunst#598]: https://github.com/dunst-project/dunst/pull/598
[dunst]: https://github.com/dunst-project/dunst/
[feh]: https://feh.finalrewind.org/
[gist]: https://github.com/defunkt/gist
[imagemagick]: https://imagemagick.org/
[journalctl]: https://www.freedesktop.org/software/systemd/man/journalctl.html
[keynav]: https://github.com/jordansissel/keynav
[patched xmonad-contrib]: https://github.com/mgsloan/xmonad-contrib/tree/mgsloan-changes
[patched xmonad]: https://github.com/mgsloan/xmonad/tree/mgsloan-changes
[redshift]: http://jonls.dk/redshift/
[rio]: https://www.stackage.org/package/rio
[setup-log.md]: env/setup-log.md
[scrot]: https://en.wikipedia.org/wiki/Scrot
[systemd#11336]: https://github.com/systemd/systemd/pull/11336
[systemd-cat]: https://www.freedesktop.org/software/systemd/man/systemd-cat.html
[xmonad-contrib#263]: https://github.com/xmonad/xmonad-contrib/pull/263
[xmonad-contrib#287]: https://github.com/xmonad/xmonad-contrib/pull/287
[xmonad-contrib#290]: https://github.com/xmonad/xmonad-contrib/issues/290
