//! Keep EWMH, drop its focus stealing.
//!
//! `add_ewmh_hooks` has to run: its refresh hook writes the `_NET_WM_DESKTOP`
//! and `_NET_ACTIVE_WINDOW` properties that a restart reads back to put windows
//! on the tags they were on.
//!
//! Its event hook is another matter. On an incoming `_NET_ACTIVE_WINDOW` client
//! message it focuses the window and switches to its tag — Chrome sends one on
//! startup, and being yanked off the current workspace is never what is wanted.
//! xmonad answered this with `setEwmhActivateHook doAskUrgent`; penrose has no
//! urgency concept, so the message is simply dropped.
//!
//! Composed *before* penrose's own hook, returning `false` to stop the default
//! handling. Only the inbound message is suppressed: the outbound property
//! writes live in the refresh hook and are untouched.

use penrose::{
    Result,
    core::{State, hooks::EventHook},
    x::{Atom, XConn, XEvent},
};
use tracing::debug;

pub fn suppress_activation<X: XConn>() -> Box<dyn EventHook<X>> {
    Box::new(|event: &XEvent, _: &mut State<X>, x: &mut X| -> Result<bool> {
        if let XEvent::ClientMessage(msg) = event
            && msg.dtype == Atom::NetActiveWindow.as_ref()
        {
            debug!(id = %msg.id, "ignoring _NET_ACTIVE_WINDOW activation request");
            let _ = x;
            return Ok(false);
        }

        Ok(true)
    })
}
