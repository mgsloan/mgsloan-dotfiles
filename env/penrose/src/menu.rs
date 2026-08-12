//! rofi as the prompt.
//!
//! Penrose ships a `DMenu` helper, but it execs `dmenu`/`dmenu_run` by name
//! and only knows dmenu's flags, so rofi needs its own thin wrapper. What is
//! given up relative to `XMonad.Prompt` is the emacs keymap, the
//! gsettings-driven light/dark switch and per-prompt history; rofi supplies its
//! own history for `-show run`.
//!
//! Note that this blocks the event loop until rofi exits. That is deliberate:
//! the selection is the point, and rofi holds its own keyboard grab while it is
//! up, so there is nothing for the window manager to do in the meantime.

use tracing::warn;

use crate::process;

/// Show a rofi menu over `options` and return the chosen line.
///
/// Returns `None` if the user cancelled. A typed line that matches nothing is
/// returned as-is, which is what makes the action menu forgiving.
pub fn select(prompt: &str, options: &[&str]) -> Option<String> {
    let input = options.join("\n");

    // Reading to EOF rather than waiting: nothing inside a running window
    // manager can wait for a child, so `wait_with_output` would fail here
    // regardless of what the user picked, and silently discard every
    // selection. See process.rs.
    let output = match process::read_output_with_input("rofi", &["-dmenu", "-i", "-p", prompt], &input) {
        Ok(output) => output,
        Err(e) => {
            warn!(%e, "unable to run rofi");
            return None;
        }
    };

    let choice = output.trim().to_owned();
    if choice.is_empty() { None } else { Some(choice) }
}

/// Ask for a line of text, with nothing to complete against.
///
/// `rofi -dmenu` with no options on stdin is a plain text prompt, which is what
/// the note and byzanz bindings want. Empty input counts as cancelled, since
/// every caller here has nothing to do with an empty string.
pub fn prompt(prompt: &str) -> Option<String> {
    select(prompt, &[])
}
