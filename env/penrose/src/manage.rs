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
    x::{
        Atom, XConn as _,
        property::Prop,
        query::{ClassName, Title},
    },
};

use crate::Conn;

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
    }
}

/// Matches transient windows and anything declaring itself a dialog.
///
/// xmonad's `isDialog` checks `_NET_WM_WINDOW_TYPE`; `WM_TRANSIENT_FOR` catches
/// the older clients that set no window type at all.
pub struct IsDialog;

impl Query<Conn> for IsDialog {
    fn run(&self, id: WinId, conn: &mut Conn) -> Result<bool> {
        if let Ok(Some(Prop::Atom(atoms))) = conn.get_prop(id, Atom::NetWmWindowType.as_ref())
            && atoms.iter().any(|a| a == Atom::NetWindowTypeDialog.as_ref())
        {
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
