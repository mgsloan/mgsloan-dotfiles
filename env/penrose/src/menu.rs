//! rofi as the prompt.
//!
//! Penrose ships a `DMenu` helper, but it execs `dmenu`/`dmenu_run` by name
//! and only knows dmenu's flags, so rofi needs its own thin wrapper. What is
//! given up relative to `XMonad.Prompt` is the emacs keymap, the
//! gsettings-driven light/dark switch and per-prompt history; rofi supplies its
//! own history for `-show run`.

use std::{
    io::Write,
    process::{Command, Stdio},
};

use tracing::warn;

/// Show a rofi menu over `options` and return the chosen line.
///
/// Returns `None` if the user cancelled. A typed line that matches nothing is
/// returned as-is, which is what makes the action menu forgiving.
pub fn select(prompt: &str, options: &[&str]) -> Option<String> {
    let mut child = match Command::new("rofi")
        .args(["-dmenu", "-i", "-p", prompt])
        .stdin(Stdio::piped())
        .stdout(Stdio::piped())
        .spawn()
    {
        Ok(c) => c,
        Err(e) => {
            warn!(%e, "unable to run rofi");
            return None;
        }
    };

    if let Some(mut stdin) = child.stdin.take() {
        let input = options.join("\n");
        if let Err(e) = stdin.write_all(input.as_bytes()) {
            warn!(%e, "unable to write options to rofi");
        }
        // Dropping stdin closes it, which is what lets rofi start matching.
    }

    let output = match child.wait_with_output() {
        Ok(o) => o,
        Err(e) => {
            warn!(%e, "rofi exited badly");
            return None;
        }
    };

    let choice = String::from_utf8_lossy(&output.stdout).trim().to_owned();
    if choice.is_empty() { None } else { Some(choice) }
}
