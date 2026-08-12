//! Key handlers that are more than a single `modify_with`, and the actions
//! behind them.
//!
//! Everything in this directory is something a binding or an `M-x` entry does.
//! Nothing here knows about penrose beyond the handler signature.

pub mod audio;
pub mod logs;
pub mod background;
pub mod bluetooth;
pub mod capture;
pub mod notes;
pub mod spotify;
pub mod toggles;

use std::thread;

use penrose::{
    builtin::actions::key_handler,
    core::{bindings::KeyEventHandler, conn::ConnExt as _},
};
use tracing::{error, info};

use crate::{Conn, EXIT_LOGOUT, EXIT_RESTART, env, menu, notify, notify::notify, process, startup};

/// `M-q`: rebuild the config and restart into it.
///
/// Rebuilding is slow, so it happens on a thread; the supervisor script
/// relaunches us when the process exits 0. There is no state to hand over
/// (tags and focus come back via EWMH properties on the next startup), so
/// exiting from the thread is safe and needs no channel into the event loop.
///
/// The rebuild's exit code comes back through `process::status`, since nothing
/// inside a running window manager can wait for a child.
pub fn restart() -> Box<dyn KeyEventHandler<Conn>> {
    key_handler(|_, _| {
        thread::spawn(|| {
            notify("Recompile + restart");

            match process::status(&env::get().penrose_script("rebuild-penrose.sh"), &[]) {
                Ok(0) => {
                    info!("rebuild succeeded, restarting");
                    std::process::exit(EXIT_RESTART);
                }
                Ok(code) => {
                    error!(code, "rebuild failed");
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

/// `M-<tag>`: bring that tag to this screen, unless switching is locked.
///
/// xmonad's `focusWorkspace`. The lock is the only reason this is not a plain
/// `modify_with`: it is a deliberate speed bump for staying on one thing, so it
/// says so rather than doing nothing.
pub fn focus_tag(tag: &'static str) -> Box<dyn KeyEventHandler<Conn>> {
    key_handler(move |state, conn: &mut Conn| {
        if toggles::switching_locked(state) {
            notify("FOCUS!");
            return Ok(());
        }

        conn.modify_and_refresh(state, |cs| cs.pull_tag_to_screen(tag))
    })
}

/// `M-p`: rofi's run dialog, which keeps its own history.
pub fn run_prompt() -> Box<dyn KeyEventHandler<Conn>> {
    key_handler(|_, _| {
        process::spawn("rofi", &["-show", "run"])?;

        Ok(())
    })
}

/// `M-x`: the action menu.
///
/// Anything too rare to deserve a binding lives here. The `startup-*` entries
/// exist so that startup can be iterated on without a restart.
///
/// `xrandrize` is deliberately absent: the screen configuration it ran is
/// X11-only and stays in the xmonad config (see design.md, omitted modules).
pub fn action_menu() -> Box<dyn KeyEventHandler<Conn>> {
    key_handler(|state, conn: &mut Conn| {
        let options = [
            "logout",
            "tops",
            "show-logs",
            "redshift-toggle",
            "touchpad-toggle",
            "dunst-toggle",
            "lock",
            "unlock",
            "connect-headphones",
            "disconnect-headphones",
            "connect-receiver",
            "disconnect-receiver",
            "update-backgrounds-list",
            "bg-white",
            "spotify-clear-cache",
            "screenshot-ocr",
            "usb-reset",
            "bluetooth-reset",
            "gist-hs",
            "gist-md",
            "gist-txt",
            "invert-screen",
            "normal-screen",
            "normal-dpi",
            "medium-dpi",
            "high-dpi",
            "startup-log-terminals",
            "startup-wireless-terminals",
            "startup-initial-applications",
            "startup-misc",
        ];

        match menu::select("M-x ", &options).as_deref() {
            Some("logout") => logout(),
            Some("tops") => tops(),
            Some("show-logs") => logs::show_for_focused(state, conn),
            Some("redshift-toggle") => toggles::toggle_redshift(state),
            Some("touchpad-toggle") => toggles::toggle_touchpad(state),
            Some("dunst-toggle") => notify::dunst_toggle(),
            Some("lock") => toggles::lock_switching(state),
            Some("unlock") => toggles::unlock_switching(state),
            Some("connect-headphones") => bluetooth::connect(bluetooth::Device::Headphones),
            Some("disconnect-headphones") => bluetooth::disconnect(bluetooth::Device::Headphones),
            Some("connect-receiver") => bluetooth::connect(bluetooth::Device::Receiver),
            Some("disconnect-receiver") => bluetooth::disconnect(bluetooth::Device::Receiver),
            Some("update-backgrounds-list") => background::update(),
            Some("bg-white") => background::white(),
            Some("spotify-clear-cache") => spotify::clear_cache(),
            Some("screenshot-ocr") => capture::screenshot_ocr(),
            // Synonyms: the bluetooth adapter coming back is what this is
            // usually for, and that is the name it gets remembered by.
            Some("usb-reset" | "bluetooth-reset") => capture::usb_reset(),
            Some("gist-hs") => capture::gist("paste.hs"),
            Some("gist-md") => capture::gist("paste.md"),
            Some("gist-txt") => capture::gist("paste.txt"),
            Some("invert-screen") => rotate_screen("inverted"),
            Some("normal-screen") => rotate_screen("normal"),
            Some("normal-dpi") => set_dpi("1", "1"),
            Some("medium-dpi") => set_dpi("1.5", "0.75"),
            Some("high-dpi") => set_dpi("2", "0.75"),
            Some("startup-log-terminals") => startup::log_terminals(),
            Some("startup-wireless-terminals") => startup::wireless_terminals(),
            Some("startup-initial-applications") => startup::initial_applications(),
            Some("startup-misc") => startup::misc(),
            Some(other) => notify(&format!("No action matching {other}")),
            None => (),
        }

        Ok(())
    })
}

/// Scale GTK programs started from here on.
///
/// Only affects what is spawned afterwards, since a program reads these once at
/// startup — which is why they are menu entries rather than a setting.
fn set_dpi(scale: &str, text_scale: &str) {
    let env = env::get();

    env.set_override("GDK_SCALE", scale);
    env.set_override("GDK_DPI_SCALE", text_scale);

    notify(&format!("GDK_SCALE={scale}, GDK_DPI_SCALE={text_scale} for new windows"));
}

/// The laptop panel, for reading something on a screen that is upside down.
fn rotate_screen(rotation: &str) {
    if let Err(e) = process::spawn("xrandr", &["--output", "eDP-1", "--rotate", rotation]) {
        error!(%e, rotation, "unable to rotate the screen");
    }
}

/// A tmux session showing what the machine is doing.
fn tops() {
    if let Err(e) = process::tmux_terminal("tops", "nvtop") {
        error!(%e, "unable to start the tops terminal");
    }
}
