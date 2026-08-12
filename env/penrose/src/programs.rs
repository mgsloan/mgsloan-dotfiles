//! Which external program does a job, and whether it exists on this backend.
//!
//! Most of what this config launches is portable -- emacs, chrome, spotify,
//! dunst, darkman, amixer, notify-send -- and rofi has had Wayland support
//! since 1.24, so the menu needs nothing. What is left is the set of X11-only
//! programs, which is small, and worth having written down in one place rather
//! than discovered one black screen at a time:
//!
//! | job                | X11        | Wayland                   |
//! |--------------------|------------|---------------------------|
//! | keyboard pointer   | `keynav`   | `waynav`, per invocation  |
//! | clipboard          | `xclip`    | `wl-copy` / `wl-paste`    |
//! | screen lock        | `slock`    | `waylock` or `swaylock`   |
//! | wallpaper          | `feh`      | `swaybg`                  |
//! | idle blank/suspend | `xidlehook`| `swayidle`                |
//! | screen rotation    | `xrandr`   | `wlr-randr`               |
//! | touchpad toggle    | `synclient`| `river-libinput-config`   |
//! | night colours      | `redshift` | `gammastep`               |
//! | root cursor        | `xsetroot` | the compositor's own      |
//!
//! Everything in the Wayland column is in `apt-packages.md`. The `installed`
//! guards stay anyway: a program that is missing says so rather than being
//! started into a session where it silently does nothing -- or, in `slock`'s
//! case, covers it in a black window that nobody can type into, which is exactly
//! how this list got written.
//!
//! Media control is not in the table because it needs no window and no
//! compositor: it is MPRIS over dbus, the same on both backends. See
//! `actions::audio::pause_video`.

use std::io;

use tracing::warn;

use crate::{env, notify::notify, process};

/// Is this a Wayland build?
const WAYLAND: bool = cfg!(feature = "river");

/// Is `cmd` on `PATH`?
///
/// Cheaper and quieter than running it to find out, and the answer is wanted
/// before deciding whether to complain.
pub fn installed(cmd: &str) -> bool {
    std::env::var_os("PATH")
        .is_some_and(|paths| std::env::split_paths(&paths).any(|dir| dir.join(cmd).is_file()))
}

/// Say that a job has no program on this backend, once, where it will be seen.
///
/// Startup is a series of independent things and a missing one is not fatal, so
/// the session comes up either way -- but silently missing is how you end up
/// wondering why the screen never blanks.
fn unavailable(job: &str, wanted: &str) {
    warn!(job, wanted, "no program for this on wayland");
    notify(&format!("No {job} on Wayland: install {wanted}"));
}

/// Start the keyboard-driven pointer, if it is the kind that runs as a daemon.
///
/// keynav is: it is spawned once and holds X grabs of its own, which is why
/// `bindings.rs` does not mention `C-semicolon` or `M-v` under X11.
///
/// waynav is not. Each invocation draws its overlay, grabs the keyboard for as
/// long as it is up, and exits, so there is nothing to start at login and its
/// entry points are ordinary bindings instead. Spawning it here would put the
/// overlay on the screen at every login and nothing else.
pub fn start_pointer_navigator() -> io::Result<()> {
    if WAYLAND {
        return Ok(());
    }

    process::spawn("keynav", &[])
}

/// Put text on the clipboard.
pub fn clipboard_copy(text: &str) -> io::Result<()> {
    if WAYLAND {
        process::spawn_with_input("wl-copy", &[], text)
    } else {
        process::spawn_with_input("xclip", &[], text)
    }
}

/// Read the clipboard, under a timeout.
///
/// The timeout is not paranoia: an X11 selection is served by the process that
/// owns it, so `xclip -o` hangs for as long as that process feels like taking.
/// `wl-paste` reads from the compositor and cannot hang the same way, but the
/// timeout costs nothing and keeps one code path.
pub fn clipboard_paste(timeout: &str) -> io::Result<String> {
    if WAYLAND {
        process::read_output("timeout", &[timeout, "wl-paste", "--no-newline"])
    } else {
        process::read_output("timeout", &[timeout, "xclip", "-o"])
    }
}

/// Set the desktop background.
pub fn set_background(path: &str) -> io::Result<()> {
    if WAYLAND {
        // swaybg is a layer surface, so it needs the window manager to have
        // bound the layer shell -- which it does -- and it replaces rather than
        // repaints, so the old one has to go.
        if !installed("swaybg") {
            unavailable("wallpaper", "swaybg");
            return Ok(());
        }

        let _ = process::spawn("killall", &["swaybg"]);
        return process::spawn("swaybg", &["-m", "fill", "-i", path]);
    }

    process::spawn("feh", &["--bg-scale", path])
}

/// Lock the screen now.
///
/// `lock-screen.sh` rather than a locker directly: swayidle's before-sleep hook
/// runs the same script, so a lid close and `M-s` put up the same screen, and
/// which locker and which picture is one decision in one place.
pub fn lock_screen() -> io::Result<i32> {
    process::status(&env::get().script(LOCK_SCRIPT), &[])
}

/// Shared with the xmonad config, and with the idle daemon below.
const LOCK_SCRIPT: &str = "lock-screen.sh";

/// Rotate the screen, or put it back.
///
/// The argument is X11's spelling, which is what the callers and the xmonad
/// config use.
pub fn rotate_screen(rotation: &str) -> io::Result<()> {
    if WAYLAND {
        if !installed("wlr-randr") {
            unavailable("screen rotation", "wlr-randr");
            return Ok(());
        }

        // Not the same vocabulary: xrandr names a rotation by where the top of
        // the screen ends up, wlroots by the angle it turns through, and
        // wlr-randr rejects the X11 spellings outright rather than doing
        // something surprising with them.
        //
        // Only the two rotations this config uses are translated. `left` and
        // `right` map onto `90` and `270`, but which way round depends on a
        // convention each side documents in its own terms, and guessing wrong
        // is invisible until somebody rotates a screen and finds it upside
        // down from what they asked for.
        let transform = match rotation {
            "normal" => "normal",
            "inverted" => "180",
            other => {
                warn!(other, "no wlroots transform known for this rotation");
                return Ok(());
            }
        };

        return process::spawn("wlr-randr", &["--output", "eDP-1", "--transform", transform]);
    }

    process::spawn("xrandr", &["--output", "eDP-1", "--rotate", rotation])
}

/// Turn the touchpad on or off.
pub fn set_touchpad(enabled: bool) -> io::Result<()> {
    if WAYLAND {
        // River configures input through river-libinput-config-v1, which its
        // own init script speaks and this window manager does not bind.
        unavailable("touchpad toggle", "a riverctl-style input config");
        return Ok(());
    }

    process::spawn(
        "synclient",
        &[if enabled {
            "TouchpadOff=0"
        } else {
            "TouchpadOff=1"
        }],
    )
}

/// Start the night-colour daemon.
pub fn start_night_colours() -> io::Result<()> {
    let (cmd, args) = if WAYLAND {
        ("gammastep", ["-l", "47:-120", "-t", "6500:3700", "-r"])
    } else {
        ("redshift", ["-l", "47:-120", "-t", "6500:3700", "-r"])
    };

    if WAYLAND && !installed(cmd) {
        unavailable("night colours", "gammastep");
        return Ok(());
    }

    process::spawn(cmd, &args)
}

/// Stop the night-colour daemon.
pub fn stop_night_colours() -> io::Result<()> {
    process::spawn("killall", &[if WAYLAND { "gammastep" } else { "redshift" }])
}

/// Programs that only make sense under X11, started only there.
///
/// Just the root cursor: under Wayland it belongs to the compositor and needs
/// nobody.
pub fn start_x11_only_daemons() {
    if WAYLAND {
        return;
    }

    if let Err(e) = process::spawn("xsetroot", &["-cursor_name", "left_ptr"]) {
        warn!(%e, "unable to set the root cursor");
    }
}

/// Blank after ten idle minutes, suspend ten minutes after that, and lock
/// before the machine sleeps.
///
/// The two timers are the same on both backends. What differs is everything
/// else:
///
/// Blanking is `wlopm`, which cuts power to the output, and not `wlr-randr
/// --off`, which would disable it: an output that leaves the layout is a screen
/// penrose has to reflow every workspace off, and back again on wake. On a
/// laptop with one screen there is nowhere to reflow to.
///
/// There is no counterpart to xidlehook's `--not-when-fullscreen`, and none is
/// wanted: idle inhibition on Wayland is a protocol, which river implements, so
/// a video player says for itself that it is playing rather than the window
/// manager guessing from the window's size. That covers a video in a window,
/// which the X11 rule never did, and stops covering a fullscreen terminal,
/// which it did.
///
/// Locking before sleep is swayidle's too. Under X11 that is `slock@.service`,
/// a systemd sleep unit, because xidlehook has no such hook.
pub fn start_idle_daemon() {
    if !WAYLAND {
        // The empty string after each timer is the "cancel" command, which
        // neither needs.
        let idle = process::spawn(
            "xidlehook",
            &[
                "--not-when-fullscreen",
                "--timer",
                "600",
                "xset dpms force suspend",
                "",
                "--timer",
                "600",
                "systemctl suspend",
                "",
            ],
        );

        if let Err(e) = idle {
            warn!(%e, "unable to start xidlehook");
        }

        return;
    }

    if !installed("swayidle") {
        unavailable("idle blank and suspend", "swayidle");
        return;
    }

    if !installed("wlopm") {
        unavailable("screen blanking", "wlopm");
    }

    let lock = env::get().script(LOCK_SCRIPT);

    // -w so that the lock is up before the machine goes to sleep rather than
    // racing it: swayidle holds a logind sleep inhibitor until the command
    // returns, which lock-screen.sh does as soon as the screen is locked.
    //
    // `lock` is logind's own signal, so `loginctl lock-session` reaches the same
    // script as the key binding does.
    let idle = process::spawn(
        "swayidle",
        &[
            "-w",
            "timeout",
            "600",
            "wlopm --off '*'",
            "resume",
            "wlopm --on '*'",
            "timeout",
            "1200",
            "systemctl suspend",
            "before-sleep",
            &lock,
            "lock",
            &lock,
        ],
    );

    if let Err(e) = idle {
        warn!(%e, "unable to start swayidle");
    }
}
