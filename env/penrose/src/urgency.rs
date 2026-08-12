//! Which windows have asked for attention.
//!
//! Penrose has no urgency concept at all, and this config suppresses the one
//! mechanism that would otherwise be visible: an application that sends
//! `_NET_ACTIVE_WINDOW` is refused rather than obeyed (`ewmh.rs`), because
//! being yanked off the current workspace is never wanted. That leaves the
//! question it was asking unanswered — Chrome wanted *something*.
//!
//! So the request is recorded instead of granted, which is what
//! `setEwmhActivateHook doAskUrgent` did under xmonad. With `border_width: 0`
//! there is nowhere to render it, so it arrives as a notification and the
//! window is remembered for `M-x goto-urgent`.
//!
//! Under river nothing marks a window: the request being recorded here is an
//! X11 client message, and river's window management protocol has no
//! counterpart -- no activation request, and no urgency hint. So the list is
//! always empty there and `M-x goto-urgent` has nothing to find, which is the
//! same as xmonad's behaviour before the activate hook existed.

use penrose::{
    Result, WinId,
    core::{State, hooks::StateHook},
};

use crate::Conn;

// Only the marking side is X11's; the list and `M-x goto-urgent` are not.
#[cfg(feature = "x11")]
use crate::notify::notify;
#[cfg(feature = "x11")]
use penrose::core::conn::Conn as _;
#[cfg(feature = "x11")]
use tracing::info;

/// Windows that have asked for attention, oldest first.
#[derive(Debug, Default)]
pub struct Urgent(Vec<WinId>);

/// Record a window as wanting attention, and say so.
///
/// Repeats are dropped: an application that asks twice while still unattended
/// should not notify twice.
#[cfg(feature = "x11")]
pub fn mark(state: &mut State<Conn>, conn: &mut Conn, id: WinId) {
    let urgent = state.extension_or_default::<Urgent>();

    if urgent.borrow().0.contains(&id) {
        return;
    }

    let name = describe(conn, id);
    urgent.borrow_mut().0.push(id);

    info!(%id, name, "window asked for attention");
    notify(&format!("{name} wants attention"));
}

/// The most recent window to have asked, if any.
pub fn most_recent(state: &mut State<Conn>) -> Option<WinId> {
    state
        .extension_or_default::<Urgent>()
        .borrow()
        .0
        .last()
        .copied()
}

/// Forget a window, because it has been seen or has gone away.
pub fn clear(state: &mut State<Conn>, id: WinId) {
    state
        .extension_or_default::<Urgent>()
        .borrow_mut()
        .0
        .retain(|urgent| *urgent != id);
}

/// Clear the focused window on every refresh.
///
/// Focus is the only definition of "seen" available here — there is no bar to
/// click and nothing else the user could do to acknowledge it.
pub fn refresh_hook() -> Box<dyn StateHook<Conn>> {
    Box::new(|state: &mut State<Conn>, _: &mut Conn| -> Result<()> {
        // Cheap in the common case: nothing is urgent, so this is a length
        // check on every refresh and no more.
        if state.extension_or_default::<Urgent>().borrow().0.is_empty() {
            return Ok(());
        }

        if let Some(focused) = state.client_set.current_client().copied() {
            clear(state, focused);
        }

        Ok(())
    })
}

/// A window's title, falling back to its id.
#[cfg(feature = "x11")]
fn describe(conn: &mut Conn, id: WinId) -> String {
    match conn.client_title(id) {
        Ok(title) if !title.is_empty() => title,
        _ => format!("Window {id}"),
    }
}
