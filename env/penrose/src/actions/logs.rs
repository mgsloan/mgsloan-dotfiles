//! What the focused window has been writing to the journal.
//!
//! Everything started from here goes through `systemd-cat` (`process.rs`), so
//! a window's output is in the journal under its own pid — but a terminal
//! running tmux running a program is three pids deep, and only the outermost
//! is on the window. So the filter is the whole ancestry, from the window's pid
//! up to the window manager itself.

use penrose::core::{State, conn::Conn as _};
use tracing::warn;

use crate::{Conn, notify::notify, process};

/// Open a pager on the journal for the focused window and its children.
pub fn show_for_focused(state: &mut State<Conn>, conn: &mut Conn) {
    let Some(pid) = state
        .client_set
        .current_client()
        .copied()
        .and_then(|id| conn.client_pid(id))
    else {
        notify("No pid for the focused window");
        return;
    };

    let filters: Vec<String> = ancestry(pid)
        .into_iter()
        .map(|pid| format!("_PID={pid}"))
        .collect();

    let command = format!(
        "journalctl --boot --follow {} | ccze -A | less -R",
        filters.join(" ")
    );

    if let Err(e) = process::tmux_terminal("logs", &command) {
        warn!(%e, "unable to open the log terminal");
    }
}

/// A pid and its ancestors, stopping at this process.
///
/// Bounded rather than looped-until-init: a `/proc` read that surprises us
/// should not spin, and nothing legitimate is more than a few levels deep.
fn ancestry(pid: u32) -> Vec<u32> {
    const MAX_DEPTH: usize = 16;

    let own = std::process::id();
    let mut pids = vec![pid];
    let mut current = pid;

    for _ in 0..MAX_DEPTH {
        match parent_of(current) {
            Some(parent) if parent != own && parent > 1 => {
                pids.push(parent);
                current = parent;
            }
            _ => break,
        }
    }

    pids
}

/// The fourth field of `/proc/<pid>/stat` is the parent pid.
///
/// The second field is the executable name in parentheses and may itself
/// contain spaces or parentheses, so the fields are counted from the last
/// `)` rather than from the start of the line.
fn parent_of(pid: u32) -> Option<u32> {
    let stat = std::fs::read_to_string(format!("/proc/{pid}/stat")).ok()?;
    let after_name = stat.rsplit_once(')')?.1;

    after_name.split_whitespace().nth(1)?.parse().ok()
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn this_process_has_a_parent() {
        assert!(parent_of(std::process::id()).is_some());
    }

    #[test]
    fn a_pid_that_does_not_exist_has_no_parent() {
        assert_eq!(parent_of(u32::MAX), None);
    }

    #[test]
    fn ancestry_starts_with_the_pid_it_was_given() {
        let pid = std::process::id();

        assert_eq!(ancestry(pid).first(), Some(&pid));
    }
}
