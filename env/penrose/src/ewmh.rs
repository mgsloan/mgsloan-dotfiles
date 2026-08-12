//! Keep EWMH, drop its focus stealing.
//!
//! `add_ewmh_hooks` has to run: its refresh hook writes the `_NET_WM_DESKTOP`
//! and `_NET_ACTIVE_WINDOW` properties that a restart reads back to put windows
//! on the tags they were on.
//!
//! Its event hook is another matter. On an incoming `_NET_ACTIVE_WINDOW` client
//! message it focuses the window and switches to its tag — Chrome sends one on
//! startup, and being yanked off the current workspace is never what is wanted.
//! xmonad answered this with `setEwmhActivateHook doAskUrgent`, and so does
//! this: the request is refused, and the window is marked as wanting attention
//! instead (`urgency.rs`).
//!
//! Composed *before* penrose's own hook, returning `false` to stop the default
//! handling. Only the inbound messages are suppressed: the outbound property
//! writes live in the refresh hook and are untouched.

use penrose::{
    Result,
    core::{State, hooks::EventHook},
    x::{Atom, XEvent},
};
use tracing::debug;

use crate::{Conn, urgency};

pub fn hook() -> Box<dyn EventHook<Conn>> {
    Box::new(
        |event: &XEvent, state: &mut State<Conn>, conn: &mut Conn| -> Result<bool> {
            let XEvent::ClientMessage(msg) = event else {
                return Ok(true);
            };

            if msg.dtype == Atom::NetActiveWindow.as_ref() {
                debug!(id = %msg.id, "refusing an activation request");
                urgency::mark(state, conn, msg.id);

                return Ok(false);
            }

            // A window may also ask by setting the state directly, which penrose
            // does not act on — but it is the same request, and worth the same
            // answer.
            if msg.dtype == Atom::NetWmState.as_ref() && demands_attention(msg, conn)? {
                debug!(id = %msg.id, "window set _NET_WM_STATE_DEMANDS_ATTENTION");
                urgency::mark(state, conn, msg.id);

                return Ok(false);
            }

            Ok(true)
        },
    )
}

/// Is this `_NET_WM_STATE` message adding `DEMANDS_ATTENTION`?
///
/// The data is `[action, first_property, second_property, ..]`, where the
/// action is 0 to remove, 1 to add and 2 to toggle. Only adding counts:
/// removing it is a window withdrawing the request.
fn demands_attention(msg: &penrose::x::event::ClientMessage, conn: &mut Conn) -> Result<bool> {
    use penrose::{x::XConn as _, x::event::ClientMessageData};

    const ADD: u32 = 1;
    const TOGGLE: u32 = 2;

    let ClientMessageData::U32(data) = msg.data else {
        return Ok(false);
    };

    if data[0] != ADD && data[0] != TOGGLE {
        return Ok(false);
    }

    let wanted = *conn.intern_atom(Atom::NetWmStateDemandsAttention.as_ref())?;

    Ok(data[1] == wanted || data[2] == wanted)
}
