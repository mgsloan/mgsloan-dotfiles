//! Startup: launch the programs that a session needs.
//!
//! X11 is the target, so the X11-only pieces are here rather than skipped:
//! keynav, xidlehook, the root cursor, and the lock the session starts behind.
//! Each has a Wayland replacement (river-design.md) for whenever that happens.
//!
//! Still absent: the xrandr screen configuration, whose output names no longer
//! match this machine, and `gnomeRegister`, which is XSMP.
//!
//! Every terminal is launched with `--class`, which is what `manage.rs` matches
//! on to place it. Changing a class here means changing the rule there.
//!
//! Each step is a named function so that the `M-x` menu can re-run it without a
//! restart, which is how startup gets iterated on.

use std::io;

use penrose::{
    Result,
    core::{State, hooks::StateHook},
};
use tracing::{info, warn};

use crate::{
    Conn,
    actions::{background, toggles},
    env,
    notify::notify,
    process,
};

/// Set by the supervisor script on every relaunch, so that a restart can skip
/// the parts of startup that should only happen once per session. It replaces
/// xmonad's `handleStartup`, which penrose has no equivalent of.
const RESTARTED: &str = "RESTARTED";

pub fn hook() -> Box<dyn StateHook<Conn>> {
    Box::new(|state: &mut State<Conn>, _: &mut Conn| -> Result<()> {
        let restarted = std::env::var(RESTARTED).is_ok();
        info!(restarted, "running startup hook");

        // Before anything else: this is where the settings that outlive a
        // restart come back, and one of them gates tag switching.
        toggles::startup(state, restarted);

        every_run();

        if !restarted {
            first_run();
        }

        Ok(())
    })
}

/// Run on every start, including restarts.
fn every_run() {
    // The X server's default root cursor is the X shape, which is what shows
    // over an empty workspace until something sets it. xmonad did this with
    // `setDefaultCursor xC_left_ptr`; penrose has no cursor API, so xsetroot
    // does it. X11-only, like keynav above, and replaced by the compositor's
    // own cursor theme under Wayland.
    report(process::spawn("xsetroot", &["-cursor_name", "left_ptr"]));

    // The hourly background rotation is a thread, so a restart has to start a
    // new one: the old one died with the old process. At worst one background
    // gets a short hour.
    background::start_rotation();

    // The remaining everyRunAction entry, `gnomeRegister`, is XSMP and stays
    // out of scope.
}

/// Skips the lock, for when the session is being started deliberately rather
/// than by autologin. Named for the xmonad variable it replaces.
const NO_STARTUP_LOCK: &str = "PENROSE_NO_STARTUP_LOCK";

/// Lock the screen while the session loads.
///
/// The machine autologins, so the lock *is* the login: applications start
/// behind it while the password is being typed, which is the whole point of
/// doing it here rather than leaving it to the display manager.
///
/// Not fatal if it fails. xmonad ended the session instead, on the reasoning
/// that an unlocked desktop is worse than no desktop — but under the supervisor
/// loop that exit is a relaunch, which would try and fail again until the loop
/// gives up and drops to a shell, itself unlocked. So this says so loudly and
/// carries on.
fn lock_screen() {
    if std::env::var(NO_STARTUP_LOCK).is_ok() {
        info!("skipping the startup lock");
        return;
    }

    std::thread::spawn(|| match process::status("slock", &[]) {
        Ok(0) => info!("screen unlocked by user"),
        Ok(code) => {
            warn!(code, "slock exited badly, so the session may never have been locked");
            notify("slock failed: the session is UNLOCKED");
        }
        Err(e) => {
            warn!(%e, "unable to run slock, so the session is unlocked");
            notify("slock could not be started: the session is UNLOCKED");
        }
    });
}

/// Run once per session.
fn first_run() {
    // First, so that everything below loads behind it.
    lock_screen();

    log_terminals();
    wireless_terminals();
    initial_applications();
    misc();

    // A different desktop to start the session with.
    background::random();
}

/// Terminals showing this boot's errors and the most recent log output.
pub fn log_terminals() {
    let journal = "journalctl --output short-precise --follow";

    report(process::tmux_terminal(
        "syslog",
        &format!("{journal} | ccze -A"),
    ));
    report(process::tmux_terminal(
        "errlog",
        &format!("{journal} --priority err --boot | errlog-filter | ccze -A"),
    ));
}

/// Terminals for driving wifi and bluetooth.
///
/// The bluetooth one is not just a convenience: `bluetooth.rs` drives it with
/// `tmux send-keys`, so the actions that connect headphones need this session
/// to exist.
pub fn wireless_terminals() {
    report(process::tmux_terminal("bt", "bluetoothctl"));
    report(process::tmux_terminal("wifi", "nmtui connect"));
}

pub fn initial_applications() {
    report(process::spawn("emacs", &[]));

    // Without a profile chrome stops at the profile picker on every start.
    report(process::spawn("google-chrome", &["--profile-directory=Default"]));

    report(process::spawn("spotify", &[]));
}

pub fn misc() {
    // Drives the mouse from the keyboard, and grabs its own keys, so nothing in
    // bindings.rs refers to it: it owns `M-v` and `C-semicolon` directly.
    report(process::spawn("keynav", &[]));

    report(process::spawn("dunst", &[]));
    report(process::spawn("darkman", &["run"]));

    // Blank after ten idle minutes, suspend ten minutes after that. The empty
    // string after each timer is the "cancel" command, which neither needs.
    report(process::spawn(
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
    ));

    output_directories();
}

/// Directories the capture bindings write into, which they cannot create
/// themselves: `flameshot --path` fails on a missing directory rather than
/// making one.
fn output_directories() {
    for dir in ["pics/screenshots", "pics/screenshots-large", "pics/screencaps"] {
        let path = env::get().home(dir);

        if let Err(e) = std::fs::create_dir_all(&path) {
            warn!(%e, path, "unable to create output directory");
        }
    }
}

/// Startup is a series of independent things, so one that cannot be spawned is
/// logged and stepped over rather than abandoning the rest.
fn report(result: io::Result<()>) {
    if let Err(e) = result {
        warn!(%e, "unable to spawn during startup");
    }
}
