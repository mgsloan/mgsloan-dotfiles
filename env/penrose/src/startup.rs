//! Startup: launch the programs that a session needs.
//!
//! Mostly the ones that also work under Wayland, since that is the code worth
//! carrying forward. keynav is the exception: it is X11-only, with waynav as
//! its Wayland replacement (river-design.md), but this is an X11 session today
//! and driving the mouse from the keyboard is not optional in practice.
//!
//! Still absent: the xrandr screen configuration, and xidlehook.
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
    env, process,
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
    // The hourly background rotation is a thread, so a restart has to start a
    // new one: the old one died with the old process. At worst one background
    // gets a short hour.
    background::start_rotation();

    // xmonad's other everyRunAction entries were `gnomeRegister` and
    // `setDefaultCursor`, both X11-only and both out of scope.
}

/// Run once per session.
fn first_run() {
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
