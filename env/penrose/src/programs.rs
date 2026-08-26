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
//! | screenshots        | `flameshot`| `slurp` + `grim`          |
//! | screen recording   | `byzanz`   | `wf-recorder`             |
//! | OCR selection      | `maim`     | `slurp` + `grim`          |
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

use crate::{WAYLAND, env, notify::notify, process};

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

/// Select a region, copy it to the clipboard, and keep a copy under `dir`.
///
/// The two backends do not just use different programs, they have different
/// shapes. flameshot is the whole job in one process -- it draws the selection,
/// copies, and names the file itself -- and returns immediately. Wayland has no
/// such program, so it is three in sequence, two of which have to be waited for,
/// which is why callers run this off the event loop.
///
/// The Wayland half is not flameshot with a different backend underneath it: an
/// X11 screenshot tool cannot work here at all. flameshot 14 captures through
/// `org.freedesktop.portal.Screenshot`, and under river nothing implements it,
/// so `M-r` hung on a dbus call that was never going to be answered. Forcing it
/// onto Xwayland instead would capture the Xwayland root, which under a rootless
/// server holds none of the session. What the annotation UI cost is real; that
/// is what `satty` or `swappy` is for, when it is worth installing one.
pub fn screenshot_region(dir: &str) -> io::Result<()> {
    if !WAYLAND {
        // `--path` is what puts the copy in `dir` rather than wherever flameshot
        // was last pointed.
        return process::spawn(
            "flameshot",
            &["gui", "--accept-on-select", "--clipboard", "--path", dir],
        );
    }

    let Some(geometry) = select_region()? else {
        return Ok(());
    };

    let path = format!(
        "{dir}/{}",
        jiff::Zoned::now().strftime("%Y-%m-%d_%H:%M:%S.png")
    );

    if !installed("grim") {
        unavailable("screenshots", "grim");
        return Ok(());
    }

    // Waited for rather than spawned: the file has to be complete before
    // wl-copy can be pointed at it.
    match process::status("grim", &["-g", &geometry, &path])? {
        0 => {}
        code => return Err(io::Error::other(format!("grim exited with {code}"))),
    }

    // wl-copy takes text as an argument but an image only on stdin, and this
    // one is binary, so it is the file that goes over rather than a string.
    process::spawn_with_file_stdin("wl-copy", &["--type", "image/png"], &path)
}

/// Ask for a region of the screen, as a geometry the capture programs take.
///
/// `None` is a cancelled selection, which is an ordinary outcome and not an
/// error. Empty output is the only way this hears about one: slurp reports a
/// cancel by exiting non-zero, and nothing in a running window manager can wait
/// for a child to find out what it exited with (`process.rs`).
///
/// Wayland only. Under X11 nothing calls this -- flameshot draws its own
/// selection, and the OCR binding is a script that runs `maim --select`.
fn select_region() -> io::Result<Option<String>> {
    if !installed("slurp") {
        unavailable("region selection", "slurp");
        return Ok(None);
    }

    let geometry = process::read_output("slurp", &[])?;
    let geometry = geometry.trim();

    if geometry.is_empty() {
        return Ok(None);
    }

    Ok(Some(geometry.to_owned()))
}

/// The script `M-S-r` records a region with.
///
/// A script per backend rather than one script with a branch in it: byzanz
/// records off the X11 damage extension and writes the gif itself, where
/// wf-recorder records off wlr-screencopy into a video that ffmpeg then has to
/// convert. Beyond taking a duration and an output path they have nothing in
/// common.
pub fn record_region_script() -> &'static str {
    if WAYLAND {
        "wf-record-region.sh"
    } else {
        "byzanz-record-region.sh"
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

        // The ones to stop, listed before the replacement exists so that it
        // cannot be one of them.
        //
        // `killall swaybg` could not draw that distinction. It matches by name,
        // so a killall still walking /proc when the new swaybg appears kills the
        // new one and the session is left with no wallpaper at all -- which is
        // what it was doing. `killall -w` closes the race, but psmisc waits on a
        // doubling backoff and takes two seconds to notice a process that died
        // immediately, which is two seconds of every wallpaper change.
        //
        // Pids also cover the swaybg left running by a previous window manager
        // process, which a pid remembered from our own spawn would not: `M-q`
        // does not set a wallpaper, so the one on screen after a restart usually
        // belongs to the process before it.
        let previous = process::pids_of("swaybg");

        // Started before the old ones are stopped, so what is on screen stays up
        // while a 4K JPEG decodes rather than the session showing the bare
        // compositor for as long as that takes. Two background-layer surfaces
        // overlap for those few milliseconds, newest on top, which is the one
        // wanted.
        let started = process::spawn("swaybg", &["-m", "fill", "-i", path]);

        // Only once there is a replacement: clearing the screen and then failing
        // to put anything back would be worse than changing nothing.
        if started.is_ok() {
            for pid in previous {
                process::terminate(pid);
            }
        }

        return started;
    }

    process::spawn("feh", &["--bg-scale", path])
}

/// Lock the screen now.
///
/// `lock-screen.sh` rather than a locker directly: swayidle's before-sleep hook
/// runs the same script, so a lid close and `M-s` put up the same screen, and
/// which locker and which picture is one decision in one place.
///
/// Turning the backlight off follows a few seconds later, from [start_idle_daemon].
pub fn lock_screen() -> io::Result<i32> {
    process::status(&env::get().script(LOCK_SCRIPT), &[])
}

/// Shared with the xmonad config, and with the idle daemon below.
const LOCK_SCRIPT: &str = "lock-screen.sh";

/// Turns the backlight off and back on, without the compositor's involvement.
const BACKLIGHT_SCRIPT: &str = "screen-backlight.sh";

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

        return process::spawn(
            "wlr-randr",
            &["--output", "eDP-1", "--transform", transform],
        );
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

/// Where the sun is, as `LAT:LON`.
///
/// Shared with darkman, which keys the light/dark theme switch off sunrise and
/// sunset from `~/.config/darkman/config.yaml` -- so the two want the same
/// place. They disagreed for a while, this one still on a previous address two
/// timezones west, which put the screen warming and the theme switch about
/// forty minutes apart.
const LOCATION: &str = "40:-105";

/// Colour temperature by day and by night, as `DAY:NIGHT` kelvin.
const TEMPERATURE: &str = "6500:3700";

/// Start the night-colour daemon.
pub fn start_night_colours() -> io::Result<()> {
    let cmd = if WAYLAND { "gammastep" } else { "redshift" };

    // -r disables the fade between temperatures, so the change is a step.
    let args = ["-l", LOCATION, "-t", TEMPERATURE, "-r"];

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
/// It happens only while the session is unlocked. A locked screen is darkened
/// by turning the backlight off instead, which `lock-screen.sh` does and this
/// undoes.
///
/// Either way, what brings a screen back is a `resume` here, and a resume runs
/// only once its own timeout has. Under Wayland nothing else will: input
/// notifies the idle tracker and goes no further, where an X server wakes DPMS
/// itself. So a screen darkened with nothing armed to undo it stays dark, and
/// every key press pushes the timeout that would have armed one further away.
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
    // Two daemons would blank and suspend on two schedules. Startup runs this
    // once per session, but `M-x startup-misc` and `M-x inhibit-idle` are both
    // how it gets re-run after that -- a change to the timers for the former, a
    // deliberate pause for the latter -- and killing first is what makes either
    // idempotent.
    stop_idle_daemon();

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

    // A locked screen goes dark three seconds after the seat does, and comes
    // back at a touch. Both from the same timeout, which is the whole trick: a
    // resume runs only once its own timeout has, so darkening the screen
    // anywhere else -- the lock script, say -- leaves nothing armed to undo it,
    // and every key press pushes the arming further away. Typing at it, the one
    // thing anybody would try, is then what keeps it dark.
    //
    // Three seconds rather than none is what "off when locked" costs. Nobody is
    // looking at a screen they have just locked and walked away from, and typing
    // a password keeps it lit, which is when you want to see it.
    let backlight = env::get().script(BACKLIGHT_SCRIPT);
    let backlight_on = format!("{backlight} on");
    let dim_locked = format!("pgrep -x swaylock > /dev/null && {backlight} off");
    // One command rather than two events: swayidle takes one of each.
    let screen_on = format!("{backlight_on}; wlopm --on '*'");

    // Powering the output down is the other way to darken a screen, and river
    // cannot survive it while the session is locked: twice it left this machine
    // dark and deaf to everything but the power button. Every part of it works
    // alone, so what breaks is the combination -- see vendor/penrose/todo.md.
    // Unlocked it is fine, and it saves more than the backlight does, so that is
    // where it stays.
    let blank_unlocked = "pgrep -x swaylock > /dev/null || wlopm --off '*'";

    // -w so that the lock is up before the machine goes to sleep rather than
    // racing it: swayidle holds a logind sleep inhibitor until the command
    // returns, which lock-screen.sh does as soon as the screen is locked.
    let idle = process::spawn(
        "swayidle",
        &[
            "-w",
            "timeout",
            "3",
            &dim_locked,
            "resume",
            &backlight_on,
            "timeout",
            "600",
            blank_unlocked,
            "resume",
            "wlopm --on '*'",
            "timeout",
            "1200",
            "systemctl suspend",
            "before-sleep",
            &lock,
            // Both ways back, and neither can strand anyone: this only ever
            // turns a screen on.
            "after-resume",
            &screen_on,
            // logind's own signal, so `loginctl lock-session` reaches the same
            // script as the key binding does.
            "lock",
            &lock,
        ],
    );

    if let Err(e) = idle {
        warn!(%e, "unable to start swayidle");
    }
}

/// Kill whichever idle daemon is running, if any.
///
/// Waited for: the replacement [start_idle_daemon] spawns straight afterwards
/// shares its name, and a `killall` still walking `/proc` when the replacement
/// appears kills the replacement too, leaving nothing armed to turn the screen
/// back on. `M-x inhibit-idle` calls this on its own, with no replacement to
/// follow it, for exactly as long as it asked for.
pub fn stop_idle_daemon() {
    let name = if WAYLAND { "swayidle" } else { "xidlehook" };
    let _ = process::status("killall", &["-w", name]);
}
