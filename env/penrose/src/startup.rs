//! Startup: launch the programs that a session needs.
//!
//! Only the ones that also work under Wayland live here, since that is the code
//! worth carrying forward. The X11-only pieces of the xmonad config's startup
//! (keynav, the xrandr screen configuration, xidlehook) are deliberately absent.
//!
//! Every terminal is launched with `--class`, which is what `manage.rs` matches
//! on to place it. Changing a class here means changing the rule there.

use penrose::{Result, core::{State, hooks::StateHook}, util::spawn};
use tracing::info;

use crate::{Conn, TERMINAL};

/// Set by the supervisor script on every relaunch, so that a restart can skip
/// the parts of startup that should only happen once per session. It replaces
/// xmonad's `handleStartup`, which penrose has no equivalent of.
const RESTARTED: &str = "RESTARTED";

pub fn hook() -> Box<dyn StateHook<Conn>> {
    Box::new(|_: &mut State<Conn>, _: &mut Conn| -> Result<()> {
        let restarted = std::env::var(RESTARTED).is_ok();
        info!(restarted, "running startup hook");

        every_run()?;

        if !restarted {
            first_run()?;
        }

        Ok(())
    })
}

/// Run on every start, including restarts.
fn every_run() -> Result<()> {
    Ok(())
}

/// Run once per session.
fn first_run() -> Result<()> {
    log_terminals()?;
    wireless_terminals()?;
    initial_applications()?;
    misc()?;

    Ok(())
}

/// Terminals showing this boot's errors and the most recent log output.
fn log_terminals() -> Result<()> {
    let journal = "journalctl --output short-precise --follow";

    tmux_terminal("syslog", &format!("{journal} | ccze -A"))?;
    tmux_terminal(
        "errlog",
        &format!("{journal} --priority err --boot | errlog-filter | ccze -A"),
    )
}

/// Terminals for driving wifi and bluetooth.
fn wireless_terminals() -> Result<()> {
    tmux_terminal("bt", "bluetoothctl")?;
    tmux_terminal("wifi", "nmtui connect")
}

fn initial_applications() -> Result<()> {
    spawn("emacs")?;
    spawn("google-chrome")?;
    spawn("spotify")
}

fn misc() -> Result<()> {
    spawn("dunst")?;
    spawn("darkman run")
}

/// A terminal running one command inside a named tmux session.
///
/// The class is what places the window (see `manage.rs`); the tmux session name
/// is what lets the command be found and replaced later.
fn tmux_terminal(name: &str, cmd: &str) -> Result<()> {
    spawn(format!(
        "{TERMINAL} --class {name} -e tmux new-session -A -s {name} {cmd}"
    ))
}
