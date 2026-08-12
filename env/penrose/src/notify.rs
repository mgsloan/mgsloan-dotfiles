//! Desktop notifications, and control messages to the notification daemon.
//!
//! Everything here is fire-and-forget. `notify-send` exits as soon as the
//! daemon has the message, and nothing in a running window manager can observe
//! that anyway (`process.rs`).
//!
//! Dunst owns `M-n`, `M-S-n` and `` M-` `` for clearing and history, so those
//! keys stay out of `bindings.rs`.

use tracing::{error, info};

use crate::{env, process};

/// How much of a notification body is worth showing.
///
/// The clipboard actions echo what they captured, and a clipboard can be a
/// megabyte.
const BODY_LIMIT: usize = 300;

/// Show a notification, matching the xmonad config's `notify`.
pub fn notify(msg: &str) {
    info!(msg, "notify");

    let icon = env::get().home("env/xmonad.png");

    if let Err(e) = process::spawn("notify-send", &["-i", &icon, "Penrose", msg]) {
        error!(%e, "unable to send notification");
    }
}

/// Show a notification, cutting the body down to something readable.
#[allow(dead_code, reason = "used by the clipboard actions, design.md §17")]
pub fn notify_truncated(msg: &str) {
    match msg.char_indices().nth(BODY_LIMIT) {
        Some((byte, _)) => notify(&format!("{}...", &msg[..byte])),
        None => notify(msg),
    }
}

/// Show a notification that goes away on its own.
///
/// For things that repeat quickly — every volume step, every Spotify skip —
/// where a stack of notifications is worse than the information is useful.
pub fn transient(title: &str, msg: &str) {
    if let Err(e) = process::spawn("notify-send", &["-t", "1000", title, msg]) {
        error!(%e, "unable to send notification");
    }
}

/// Toggle whether dunst shows notifications at all.
///
/// This is a control message rather than a notification: dunst interprets the
/// body and displays nothing.
#[allow(dead_code, reason = "an M-x entry, design.md §20")]
pub fn dunst_toggle() {
    if let Err(e) = process::spawn("notify-send", &["DUNST_COMMAND_TOGGLE"]) {
        error!(%e, "unable to toggle dunst");
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    /// Truncation is by character, so a multi-byte body cannot be split
    /// mid-character into a panic.
    #[test]
    fn truncation_lands_on_a_character_boundary() {
        let msg = "é".repeat(BODY_LIMIT * 2);

        let (byte, _) = msg.char_indices().nth(BODY_LIMIT).expect("past the limit");
        assert_eq!(msg[..byte].chars().count(), BODY_LIMIT);
    }

    #[test]
    fn short_bodies_are_left_alone() {
        assert!("short".char_indices().nth(BODY_LIMIT).is_none());
    }
}
