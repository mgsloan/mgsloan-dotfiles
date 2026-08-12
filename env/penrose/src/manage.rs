//! Manage hooks: where new windows go, and which ones float.
//!
//! Placement is class-based. Each program that needs a home gets a distinct
//! window class — terminals via `alacritty --class` in `startup.rs` — so a
//! declarative rule matches it, and there is no pid tracking to keep in step
//! with the spawn sites.

use penrose::{
    Result, WinId,
    core::{
        conn::{Conn as _, Query},
        hooks::ManageHook,
    },
    extensions::hooks::manage::{FloatingCentered, SetWorkspace},
    manage_hooks,
};

#[cfg(feature = "x11")]
use penrose::core::State;
#[cfg(feature = "x11")]
use penrose::x::{
    Atom, XConn,
    property::Prop,
    query::{ClassName, Title},
};

use crate::Conn;

/// Placement under X11, matching on window properties.
#[cfg(feature = "x11")]
pub fn hooks() -> Box<dyn ManageHook<Conn>> {
    manage_hooks! {
        // `WM_CLASS` holds two strings — the instance name and the class name —
        // and `ClassName` matches the second. `alacritty --class NAME` sets both
        // to NAME, so the terminals match on the name startup.rs gave them, but
        // a program that names itself matches on *its* spelling: Spotify's
        // instance is "spotify" and its class is "Spotify". Getting that wrong
        // fails silently, leaving the window wherever it opened.
        ClassName("syslog") => SetWorkspace("9"),
        ClassName("errlog") => SetWorkspace("9"),
        ClassName("bt") => SetWorkspace("0"),
        ClassName("wifi") => SetWorkspace("0"),
        ClassName("Spotify") => SetWorkspace("8"),
        Title("Desktop") => SetWorkspace("0"),
        // Browsers driven by puppeteer/playwright, kept off the current tag.
        AutomatedBrowser => SetWorkspace("7"),
        IsDialog => FloatingCentered::new(0.6, 0.6),
        // Anything that calls itself a notification places itself, so it is
        // floated where it asked to be rather than anywhere of our choosing.
        IsNotification => FloatInPlace,
    }
}

/// Placement under river, matching on `app_id`.
///
/// The same rules, minus the two X11 has and river does not. There is no window
/// type, so a window that calls itself a notification cannot be recognised as
/// one -- and no `WM_CLASS` instance name, though nothing here matched on that.
/// Placement itself is unaffected: `alacritty --class NAME` sets `app_id` under
/// Wayland too, so the terminals `startup.rs` names still land where they should.
#[cfg(all(feature = "river", not(feature = "x11")))]
pub fn hooks() -> Box<dyn ManageHook<Conn>> {
    use penrose::river::query::{AppId, IsChild, Title};

    manage_hooks! {
        AppId("syslog") => SetWorkspace("9"),
        AppId("errlog") => SetWorkspace("9"),
        AppId("bt") => SetWorkspace("0"),
        AppId("wifi") => SetWorkspace("0"),
        // Both spellings, because which one arrives depends on how Spotify
        // launched. River reports a native Wayland client's own app_id, and for
        // an XWayland client it reports the X11 *class* instead (Window.zig:
        // "X11 clients don't have an app_id but the class serves a similar
        // role"). Spotify is Electron: its class is "Spotify" and its Wayland
        // app_id is "spotify", and which one it uses depends on whether Electron
        // picked Wayland or fell back to Xwayland that day.
        //
        // The terminals need no such care: `alacritty --class NAME` sets both
        // strings to NAME.
        AppId("Spotify") => SetWorkspace("8"),
        AppId("spotify") => SetWorkspace("8"),
        Title("Desktop") => SetWorkspace("0"),
        AutomatedBrowser => SetWorkspace("7"),
        // River has no _NET_WM_WINDOW_TYPE, so a parent is the only thing that
        // marks a dialog. Penrose floats transient windows of its own accord;
        // this is what centres them.
        IsChild => FloatingCentered::new(0.6, 0.6),
    }
}

/// Float a window exactly where it put itself.
///
/// Only reachable under X11: it is what `IsNotification` places with, and river
/// has no window type for a notification to declare.
///
/// xmonad's `doFloat`. Penrose's floating manage hooks all impose a position —
/// centred, fixed, or relative — which is right for dialogs and wrong for
/// anything that has already chosen where to be.
#[cfg(feature = "x11")]
pub struct FloatInPlace;

#[cfg(feature = "x11")]
impl ManageHook<Conn> for FloatInPlace {
    fn call(&mut self, id: WinId, state: &mut State<Conn>, conn: &mut Conn) -> Result<()> {
        let r = XConn::client_geometry(conn, id)?;

        state.client_set.float(id, r)
    }
}

/// Matches windows declaring themselves notifications.
///
/// Rarely reaches a manage hook at all: most notification daemons, dunst
/// included, use override-redirect windows, which never generate a MapRequest
/// and so are never managed. This is for the ones that do it the other way.
#[cfg(feature = "x11")]
pub struct IsNotification;

#[cfg(feature = "x11")]
impl Query<Conn> for IsNotification {
    fn run(&self, id: WinId, conn: &mut Conn) -> Result<bool> {
        Ok(has_window_type(id, conn, Atom::NetWindowTypeNotification))
    }
}

/// Does this window declare the given `_NET_WM_WINDOW_TYPE`?
#[cfg(feature = "x11")]
fn has_window_type(id: WinId, conn: &mut Conn, want: Atom) -> bool {
    matches!(
        conn.get_prop(id, Atom::NetWmWindowType.as_ref()),
        Ok(Some(Prop::Atom(atoms))) if atoms.iter().any(|a| a == want.as_ref())
    )
}

/// Matches transient windows and anything declaring itself a dialog.
///
/// xmonad's `isDialog` checks `_NET_WM_WINDOW_TYPE`; `WM_TRANSIENT_FOR` catches
/// the older clients that set no window type at all.
#[cfg(feature = "x11")]
pub struct IsDialog;

#[cfg(feature = "x11")]
impl Query<Conn> for IsDialog {
    fn run(&self, id: WinId, conn: &mut Conn) -> Result<bool> {
        if has_window_type(id, conn, Atom::NetWindowTypeDialog) {
            return Ok(true);
        }

        Ok(conn.client_transient_parent(id).is_some())
    }
}

/// Matches a browser started by a test automation tool.
///
/// The window properties are no help on their own: an automated Chrome has the
/// same class as a normal one. But puppeteer and playwright both pass
/// `--enable-automation`, and `_NET_WM_PID` points at that process, so the flag
/// can be read back out of /proc.
pub struct AutomatedBrowser;

impl Query<Conn> for AutomatedBrowser {
    fn run(&self, id: WinId, conn: &mut Conn) -> Result<bool> {
        let Some(pid) = conn.client_pid(id) else {
            return Ok(false);
        };

        let Ok(cmdline) = std::fs::read(format!("/proc/{pid}/cmdline")) else {
            return Ok(false);
        };

        // Arguments in cmdline are NUL separated and NUL terminated.
        Ok(cmdline
            .split(|b| *b == 0)
            .any(|arg| arg == b"--enable-automation"))
    }
}
