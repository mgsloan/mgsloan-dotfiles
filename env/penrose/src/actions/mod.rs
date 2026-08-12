//! Key handlers that are more than a single `modify_with`, and the actions
//! behind them.
//!
//! Everything in this directory is something a binding or an `M-x` entry does.
//! Nothing here knows about penrose beyond the handler signature.

pub mod audio;
pub mod background;
pub mod bluetooth;
pub mod capture;
pub mod logs;
pub mod notes;
pub mod spotify;
pub mod toggles;

use std::thread;

use penrose::{
    builtin::actions::key_handler,
    core::{bindings::KeyEventHandler, conn::ConnExt as _},
};
use tracing::{error, info};

use crate::{
    Conn, EXIT_LOGOUT, EXIT_RESTART, env, menu, notify, notify::notify, process, programs, startup,
    urgency,
};

/// `M-q`: rebuild the config and restart into it.
///
/// Rebuilding is slow, so it happens on a thread; the supervisor relaunches us
/// when the process exits 0.
///
/// One config builds two window managers, and only the running one is worth
/// waiting for: the rebuild script is told which that is, builds it first, and
/// returns as soon as it is installed. The other backend is built behind us and
/// says so itself if it fails -- by then this process is gone.
///
/// Under X11 there is no state to hand over: tags and focus come back from EWMH
/// properties on the next startup. River has no property store, so both are
/// written to a file here instead -- before the rebuild rather than after it,
/// since by then this thread no longer has the state. A few seconds of window
/// movement is the most that can be missed.
///
/// The rebuild's exit code comes back through `process::status`, since nothing
/// inside a running window manager can wait for a child.
pub fn restart() -> Box<dyn KeyEventHandler<Conn>> {
    key_handler(|_state, _conn| {
        #[cfg(feature = "river")]
        crate::conn::save_state(_state, _conn);

        thread::spawn(|| {
            notify("Recompile + restart");

            match process::status(&env::get().penrose_script("rebuild-penrose.sh"), &[BACKEND]) {
                Ok(0) => {
                    info!(BACKEND, "rebuild succeeded, restarting");
                    notify(&format!("Restarting; {OTHER_BACKEND} building behind it"));
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

/// Which window manager this build is, and which the rebuild leaves for later.
#[cfg(feature = "x11")]
const BACKEND: &str = "x11";
#[cfg(feature = "x11")]
const OTHER_BACKEND: &str = "river";
#[cfg(feature = "river")]
const BACKEND: &str = "river";
#[cfg(feature = "river")]
const OTHER_BACKEND: &str = "x11";

/// `M-s`: lock the screen with whichever locker this backend has.
///
/// On a thread, because the locker runs until the password is typed and a
/// handler that waits for that would stop window management for the duration --
/// under river it would stop the compositor's input, which is the one thing that
/// could type it.
pub fn lock_screen() -> Box<dyn KeyEventHandler<Conn>> {
    key_handler(|_, _| {
        thread::spawn(|| {
            if let Err(e) = programs::lock_screen() {
                error!(%e, "unable to lock the screen");
            }
        });

        Ok(())
    })
}

/// `C-;` under river: waynav, zoomed to the focused window.
///
/// keynav did this with `windowzoom`, which waynav cannot implement: a Wayland
/// client cannot ask where another client's window is. The window manager can,
/// having put it there, so it does that half itself -- warp the pointer to the
/// middle of the window, then hand waynav a `cursorzoom` the size of the window,
/// which is centred on the pointer. Same result, from the only side that knows.
///
/// Pressing it again dismisses waynav, as keynav's `toggle-start` did: see
/// [waynav_dismissed].
#[cfg(feature = "river")]
pub fn waynav_window() -> Box<dyn KeyEventHandler<Conn>> {
    use penrose::core::conn::Conn as _;

    key_handler(|state, conn: &mut Conn| {
        if waynav_dismissed() {
            return Ok(());
        }

        let Some(&id) = state.client_set.current_client() else {
            // Nothing focused, so nothing to zoom to: the whole screen it is.
            process::spawn("waynav", &[])?;
            return Ok(());
        };

        let r = conn.client_geometry(id)?;
        conn.warp_pointer_to_window(id)?;

        match waynav_window_rc(r.w, r.h) {
            Ok(rc) => process::spawn("waynav", &["-c", &rc])?,
            Err(e) => {
                error!(%e, "unable to write the waynav config");
                process::spawn("waynav", &[])?;
            }
        }

        Ok(())
    })
}

/// `C-S-;` under river: waynav over the whole screen.
///
/// keynav's `toggle-start,warp`, which is what plain `waynav` does: no `-c`, so
/// it reads `~/.config/waynav/waynavrc` and uses the start line there. The
/// window zoom needs a config of its own only because the size is the window's.
#[cfg(feature = "river")]
pub fn waynav_screen() -> Box<dyn KeyEventHandler<Conn>> {
    key_handler(|_, _| {
        if !waynav_dismissed() {
            process::spawn("waynav", &[])?;
        }

        Ok(())
    })
}

/// Dismiss a waynav that is already up, reporting whether there was one.
///
/// This is the toggle half of both entry points, and it cannot be left to
/// waynav: river matches xkb bindings before it consults keyboard focus, so
/// neither key reaches waynav's own grab while the overlay is up, and a second
/// waynav finds the lock in XDG_RUNTIME_DIR held and exits silently.
#[cfg(feature = "river")]
fn waynav_dismissed() -> bool {
    matches!(process::status("pkill", &["-x", "waynav"]), Ok(0))
}

/// Write a waynav config that starts zoomed to a `w` by `h` region.
///
/// The size is the focused window's, so this cannot be a static file like
/// `paste-rc`. It is the user's own config with a start line appended: the
/// navigation keys have to come from somewhere, and `store_start_commands` takes
/// the last `start` line it sees, so appending wins.
#[cfg(feature = "river")]
fn waynav_window_rc(w: u32, h: u32) -> std::io::Result<String> {
    let base = std::fs::read_to_string(env::get().home(".config/waynav/waynavrc"))?;
    let path = format!(
        "{}/waynav-window-rc",
        std::env::var("XDG_RUNTIME_DIR").unwrap_or_else(|_| "/tmp".to_owned())
    );

    std::fs::write(
        &path,
        format!("{base}\n# Written by penrose: zoomed to the focused window.\nsuper+z start,cursorzoom {w} {h},warp\n"),
    )?;

    Ok(path)
}

/// `M-v` under river: waynav's middle-click-paste entry point.
///
/// `program` cannot express it because the config path is the home directory's,
/// which is not known until runtime.
#[cfg(feature = "river")]
pub fn waynav_paste() -> Box<dyn KeyEventHandler<Conn>> {
    key_handler(|_, _| {
        let rc = env::get().home(".config/waynav/paste-rc");
        process::spawn("waynav", &["-c", &rc])?;

        Ok(())
    })
}

/// End the session.
///
/// gdm-x-session runs the window manager as the session script, so exiting used
/// to end the session on its own. Under the supervisor loop a plain exit means
/// "restart", so logging out needs its own exit code for the loop to break on.
///
/// River is not run by the display manager as the session, the compositor is, so
/// exiting only ends the window manager and leaves a compositor with nobody
/// managing it -- a blank screen. Asking river to end the session is what
/// actually logs out there.
fn logout(_conn: &mut Conn) -> ! {
    info!("exiting for logout");

    #[cfg(feature = "river")]
    _conn.exit_session();

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

/// Launch a program, through the journal and with any environment overrides.
///
/// `penrose::builtin::actions::spawn` would do, but it splits a string on
/// whitespace, bypasses `systemd-cat` and ignores the overrides in §20 — so
/// nothing here uses it.
pub fn program(cmd: &'static str, args: &'static [&'static str]) -> Box<dyn KeyEventHandler<Conn>> {
    key_handler(move |_, _| {
        process::spawn(cmd, args)?;

        Ok(())
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
            "goto-urgent",
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
            Some("logout") => logout(conn),
            Some("tops") => tops(),
            Some("show-logs") => logs::show_for_focused(state, conn),
            Some("goto-urgent") => goto_urgent(state, conn),
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

/// Focus whatever last asked for attention.
///
/// With no status bar, this is the other half of `urgency.rs`: the notification
/// says something wants you, and this is how to get there.
fn goto_urgent(state: &mut penrose::core::State<Conn>, conn: &mut Conn) {
    let Some(id) = urgency::most_recent(state) else {
        notify("Nothing is asking for attention");
        return;
    };

    if let Err(e) = conn.modify_and_refresh(state, |cs| cs.focus_client(&id)) {
        error!(%e, %id, "unable to focus the urgent window");
    }
}

/// Scale GTK programs started from here on.
///
/// Only affects what is spawned afterwards, since a program reads these once at
/// startup — which is why they are menu entries rather than a setting.
fn set_dpi(scale: &str, text_scale: &str) {
    let env = env::get();

    env.set_override("GDK_SCALE", scale);
    env.set_override("GDK_DPI_SCALE", text_scale);

    notify(&format!(
        "GDK_SCALE={scale}, GDK_DPI_SCALE={text_scale} for new windows"
    ));
}

/// The laptop panel, for reading something on a screen that is upside down.
fn rotate_screen(rotation: &str) {
    if let Err(e) = programs::rotate_screen(rotation) {
        error!(%e, rotation, "unable to rotate the screen");
    }
}

/// A tmux session showing what the machine is doing.
fn tops() {
    if let Err(e) = process::tmux_terminal("tops", "nvtop") {
        error!(%e, "unable to start the tops terminal");
    }
}
