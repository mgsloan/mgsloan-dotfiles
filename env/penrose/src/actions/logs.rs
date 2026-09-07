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

/// The deadline is shared with errlog-filter and expires without a WM timer.
pub fn inhibit_error_alerts(minutes: &str) {
    let now = jiff::Timestamp::now().as_second();
    let Some(deadline) = error_alert_deadline(minutes, now) else {
        notify(&format!("Not a number of minutes (0 to resume): {minutes}"));
        return;
    };
    let path = crate::env::get().state("error-alerts-inhibit-until");
    let result = (|| -> std::io::Result<()> {
        if let Some(directory) = std::path::Path::new(&path).parent() {
            std::fs::create_dir_all(directory)?;
        }
        let temporary = format!("{path}.tmp");
        std::fs::write(&temporary, format!("{deadline}\n"))?;
        std::fs::rename(temporary, path)
    })();
    if let Err(error) = result {
        warn!(%error, "unable to save error alert mute deadline");
        notify("Unable to mute error rate alerts");
    } else if deadline <= now {
        notify("Error rate alerts enabled");
    } else {
        notify(&format!("Error rate alerts muted for {}m", minutes.trim()));
    }
}

fn error_alert_deadline(minutes: &str, now: i64) -> Option<i64> {
    let minutes: f64 = minutes.trim().parse().ok()?;
    let seconds = (minutes * 60.0).ceil();
    if !minutes.is_finite() || minutes < 0.0 || seconds >= i64::MAX as f64 {
        return None;
    }
    #[allow(
        clippy::cast_possible_truncation,
        reason = "finite, nonnegative and bounded above"
    )]
    now.checked_add(seconds as i64)
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
    fn error_alert_mute_accepts_minutes_and_zero_to_resume() {
        assert_eq!(error_alert_deadline(" 1.5 ", 100), Some(190));
        assert_eq!(error_alert_deadline("0", 100), Some(100));
        assert_eq!(error_alert_deadline("0.001", 100), Some(101));
        for invalid in ["", "-1", "NaN", "inf", "1e30"] {
            assert_eq!(error_alert_deadline(invalid, 100), None);
        }
        assert_eq!(error_alert_deadline("1", i64::MAX), None);
    }

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
