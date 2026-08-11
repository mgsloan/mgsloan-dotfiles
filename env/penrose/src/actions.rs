//! Key handlers that are more than a single `modify_with`.

use std::{process::Command, thread};

use penrose::{
    builtin::actions::key_handler,
    core::bindings::KeyEventHandler,
    util::spawn,
};
use tracing::{error, info};

use crate::{Conn, EXIT_LOGOUT, EXIT_RESTART, menu};

/// `M-q`: rebuild the config and restart into it.
///
/// Rebuilding is slow, so it happens on a thread; the supervisor script
/// relaunches us when the process exits 0. There is no state to hand over
/// (tags and focus come back via EWMH properties on the next startup), so
/// exiting from the thread is safe and needs no channel into the event loop.
pub fn restart() -> Box<dyn KeyEventHandler<Conn>> {
    key_handler(|_, _| {
        thread::spawn(|| {
            notify("Recompile + restart");

            let rebuild = Command::new(script("rebuild-penrose.sh")).status();

            match rebuild {
                Ok(s) if s.success() => {
                    info!("rebuild succeeded, restarting");
                    std::process::exit(EXIT_RESTART);
                }
                Ok(s) => {
                    error!(code = ?s.code(), "rebuild failed");
                    notify("Failed recompilation");
                }
                Err(e) => {
                    error!(%e, "unable to run rebuild script");
                    notify("Failed to run rebuild script");
                }
            }
        });

        Ok(())
    })
}

/// End the X session.
///
/// gdm-x-session runs the window manager as the session script, so exiting used
/// to end the session on its own. Under the supervisor loop a plain exit means
/// "restart", so logging out needs its own exit code for the loop to break on.
fn logout() -> ! {
    info!("exiting for logout");
    std::process::exit(EXIT_LOGOUT)
}

/// `M-p`: rofi's run dialog, which keeps its own history.
pub fn run_prompt() -> Box<dyn KeyEventHandler<Conn>> {
    key_handler(|_, _| spawn("rofi -show run"))
}

/// `M-x`: the action menu.
///
/// The xmonad config's `actionPrompt` is a large map of one-off commands, most
/// of which are out of scope here. This is the window-management subset.
pub fn action_menu() -> Box<dyn KeyEventHandler<Conn>> {
    key_handler(|_, _| {
        let options = ["logout", "xrandrize", "tops"];

        match menu::select("M-x ", &options).as_deref() {
            Some("logout") => logout(),
            Some("xrandrize") => spawn(script("xrandrize.sh")),
            Some("tops") => spawn(format!("{} -e tmux new-session -s tops nvtop", crate::TERMINAL)),
            Some(other) => {
                notify(&format!("No action matching {other}"));
                Ok(())
            }
            None => Ok(()),
        }
    })
}

/// Path to a script in `~/env/scripts`.
pub fn script(name: &str) -> String {
    let home = std::env::var("HOME").unwrap_or_else(|_| "/root".to_owned());
    format!("{home}/env/scripts/{name}")
}

/// Desktop notification, matching the xmonad config's `notify`.
fn notify(msg: &str) {
    let home = std::env::var("HOME").unwrap_or_default();
    let icon = format!("{home}/env/xmonad.png");

    if let Err(e) = Command::new("notify-send")
        .args(["-i", &icon, "Penrose", msg])
        .status()
    {
        error!(%e, "unable to send notification");
    }
}
