//! Startup: launch the programs that a session needs.
//!
//! Which program does a job on which backend lives in `programs.rs`, along with
//! the list of what is still X11-only and what would replace it.
//!
//! Still absent: the xrandr screen configuration, whose output names no longer
//! match this machine, and `gnomeRegister`, which is XSMP.
//!
//! Every terminal is launched with a `--class` of its own, which is what
//! `manage.rs` matches on to place it. The classes are constants in `main.rs`,
//! so the spawn site here and the rule there name one thing rather than two
//! strings that have to be kept in step.
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
    CLASS_BT, CLASS_ERRLOG, CLASS_SYSLOG, CLASS_WIFI, Conn,
    actions::{background, toggles},
    env, layouts, process, programs,
};

/// Set by the supervisor script on every relaunch, so that a restart can skip
/// the parts of startup that should only happen once per session. It replaces
/// xmonad's `handleStartup`, which penrose has no equivalent of.
const RESTARTED: &str = "RESTARTED";

/// Set to run this window manager without starting a session around it.
///
/// The startup hook is the one part of this config that reaches outside its own
/// session: it spawns a dozen programs, several of which are singletons that a
/// second copy of disturbs, and terminals that adopt tmux sessions by name. That
/// is right at login and wrong in a test, where the window manager is being run
/// against a headless compositor next to a real session that is still using
/// those programs.
const NO_STARTUP_HOOK: &str = "PENROSE_NO_STARTUP_HOOK";

pub fn hook() -> Box<dyn StateHook<Conn>> {
    Box::new(|state: &mut State<Conn>, _: &mut Conn| -> Result<()> {
        if std::env::var(NO_STARTUP_HOOK).is_ok() {
            info!("skipping the startup hook: {NO_STARTUP_HOOK} is set");
            return Ok(());
        }

        let restarted = std::env::var(RESTARTED).is_ok();
        info!(restarted, "running startup hook");

        // Before anything else: this is where the settings that outlive a
        // restart come back, and one of them gates tag switching.
        toggles::startup(state, restarted);

        // Each workspace's layout, which a restart drops and a new session is
        // meant to start fresh from. Before the startup programs, so that a
        // window mapping cannot race the layout it will be placed by.
        layouts::startup(state, restarted);

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

    // The remaining everyRunAction entry, `gnomeRegister`, is XSMP and stays
    // out of scope.
}

/// Run once per session, rather than on every restart.
///
/// No lock: the machine autologins, so locking here asks for the password a
/// third time for a session that is already open. `M-s` locks on demand and the
/// idle daemon locks before sleep.
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
        CLASS_SYSLOG,
        &format!("{journal} | ccze -A"),
    ));
    report(process::tmux_terminal(
        CLASS_ERRLOG,
        &format!("{journal} --priority err --boot | errlog-filter | ccze -A"),
    ));
}

/// Terminals for driving wifi and bluetooth.
///
/// The bluetooth one is not just a convenience: `bluetooth.rs` drives it with
/// `tmux send-keys`, so the actions that connect headphones need this session
/// to exist.
pub fn wireless_terminals() {
    report(process::tmux_terminal(CLASS_BT, "bluetoothctl"));
    report(process::tmux_terminal(CLASS_WIFI, "nmtui connect"));
}

pub fn initial_applications() {
    report(process::spawn("emacs", &[]));

    // Without a profile chrome stops at the profile picker on every start.
    report(process::spawn(
        "google-chrome",
        &["--profile-directory=Default"],
    ));

    report(process::spawn("spotify", &[]));
}

pub fn misc() {
    // Drives the mouse from the keyboard, and grabs its own keys, so nothing in
    // bindings.rs refers to it: it owns `M-v` and `C-semicolon` directly.
    // Only under X11, where it is a daemon: see programs.rs. Under river the
    // same job is done by bindings, since waynav exits when it is dismissed.
    report(programs::start_pointer_navigator());

    report(process::spawn("dunst", &[]));

    // darkman is deliberately not spawned here: `darkman.service` is a user
    // unit and is enabled (see setup-scripts/050-darkman.sh). A second `darkman
    // run` unlinks and rebinds the first one's control socket *before* it finds
    // out that the D-Bus name `nl.whynothugo.darkman` is taken and exits, so the
    // survivor is left listening on an orphaned inode. Transitions keep working;
    // `darkman get`/`toggle` fail with ECONNREFUSED until it is restarted.

    programs::start_x11_only_daemons();
    programs::start_idle_daemon();

    output_directories();
}

/// Directories the capture bindings write into, which they cannot create
/// themselves: `flameshot --path` fails on a missing directory rather than
/// making one, and `grim` fails the same way on the path it is given.
fn output_directories() {
    for dir in [
        "pics/screenshots",
        "pics/screenshots-large",
        "pics/screencaps",
    ] {
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
