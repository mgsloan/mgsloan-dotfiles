//! `M-x caffeinate`: pause screen blanking and suspend for a while.
//!
//! There is no runtime pause in either idle daemon (`programs.rs`'s table), so
//! this asks the same question xmonad never had to: stop the daemon outright,
//! and start a fresh one once the requested time is up. That is exactly what
//! `M-x startup-misc` already does after a config change, so the only new part
//! here is the wait -- a thread that sleeps and then calls the same
//! [programs::start_idle_daemon] startup calls, per design.md §10.
//!
//! What the thread is waiting for is a deadline in a file, and the file rather
//! than the thread is what says whether idle is inhibited (design.md §14). Two
//! things fall out of that. A second `M-x caffeinate` replaces the deadline, so
//! the thread the first one left behind wakes to find a deadline it was not
//! armed for and exits without touching the daemon -- the newest call wins,
//! whether it asked for longer or for shorter. And a restart, which kills every
//! thread in this process, can read the deadline back and arm a new thread for
//! whatever is left of it, instead of leaving the daemon down until something
//! starts it by hand.
//!
//! The daemon itself needs no such help: it is spawned, not supervised, so it
//! outlives an `M-q` on its own. Only the pause has to be put back together.

use std::{
    sync::Mutex,
    thread,
    time::{Duration, SystemTime, UNIX_EPOCH},
};

use tracing::{error, info, warn};

use crate::{env, notify::notify, programs};

/// One restore at a time.
///
/// Two threads armed for the same second -- `M-x caffeinate 1` twice in a row,
/// say -- would otherwise interleave their kill and their spawn, and a `killall`
/// that runs between the other's kill and spawn leaves two daemons blanking on
/// two schedules (see [programs::stop_idle_daemon]). Holding this across the
/// re-read of the deadline means the loser sees the winner's work and returns.
static RESTORING: Mutex<()> = Mutex::new(());

/// `M-x caffeinate`: ask how many hours, then hold off blanking and
/// suspend for that long.
pub fn inhibit(hours: &str) {
    let Some(deadline) = deadline_from(hours, now()) else {
        notify(&format!("Not a number of hours: {hours}"));
        return;
    };

    // Before stopping anything: the file is what will restore the daemon, via
    // this call's thread or via a later start-up, so a daemon stopped without
    // it is a daemon nothing is going to bring back.
    if let Err(e) = save(deadline) {
        error!(%e, "unable to write the idle inhibit deadline");
        notify("Unable to inhibit idle");
        return;
    }

    programs::stop_idle_daemon();
    notify(&format!("Idle inhibited for {}h", hours.trim()));

    wait_until(deadline);
}

/// Put the idle daemon back and forget any inhibition.
///
/// `M-x startup-misc` restarts the daemon on purpose, which ends a caffeinate
/// whether or not it was meant to; clearing the deadline here is what stops the
/// next restart from reviving one that is already over.
pub fn restore() {
    clear();
    programs::start_idle_daemon();
}

/// Pick an inhibition up where the previous process left it.
///
/// A restart kills the waiting thread, and nothing else restarts the daemon
/// after one, so without this an `M-q` during a caffeinate leaves the screen
/// awake for good. `restarted` is the same split the other persistent state
/// uses (design.md §14): a new session starts the daemon in `first_run` and has
/// no business honouring a deadline from a session that is over.
pub fn startup(restarted: bool) {
    if !restarted {
        clear();
        return;
    }

    let Some(deadline) = load() else {
        return;
    };

    let remaining = deadline.saturating_sub(now());

    if remaining == 0 {
        // The deadline passed with nobody awake to act on it, so this is a
        // relaunch after a crash rather than an `M-q`. Late is still better
        // than never.
        info!(
            deadline,
            "the idle inhibit expired while we were not running"
        );
        restore();
        return;
    }

    // The deadline is the authority on this, not the process table: if it says
    // idle is inhibited then the daemon is meant to be down, and a stray one
    // would blank the screen halfway through.
    programs::stop_idle_daemon();
    notify(&format!("Idle still inhibited for {}", describe(remaining)));

    wait_until(deadline);
}

/// Restore the daemon once `deadline` passes, if it is still in force by then.
fn wait_until(deadline: u64) {
    thread::spawn(move || {
        loop {
            match load() {
                // Restored already, by `M-x startup-misc` or by a thread that
                // was armed for the same moment as this one.
                None => {
                    info!(deadline, "the idle inhibit is already over");
                    return;
                }
                // A later `M-x caffeinate`, longer or shorter, with a thread of
                // its own to end it.
                Some(current) if current != deadline => {
                    info!(deadline, current, "superseded by a later idle inhibit");
                    return;
                }
                Some(_) => (),
            }

            let remaining = deadline.saturating_sub(now());

            if remaining == 0 {
                break;
            }

            // Sleeping is against a monotonic clock and the deadline is against
            // the wall clock, so a clock correction can land this here early.
            // Looping rather than restoring is what keeps a backwards jump from
            // cutting the inhibition short.
            #[allow(clippy::disallowed_methods, reason = "not the event loop thread")]
            thread::sleep(Duration::from_secs(remaining));
        }

        let _guard = RESTORING.lock().expect("idle restore lock");

        // Re-read under the lock, since the checks above were made outside it.
        if load() != Some(deadline) {
            info!(deadline, "the idle inhibit ended while waking up");
            return;
        }

        info!(deadline, "restoring the idle daemon");
        restore();
        notify("Idle daemon restored");
    });
}

/// The unix time an answer to the prompt runs out at, if it is one.
///
/// Rejects the absurd along with the invalid: hours large enough to overflow
/// the clock are a typo, and every one of them would be a deadline no thread is
/// ever going to reach.
fn deadline_from(hours: &str, now: u64) -> Option<u64> {
    let parsed: f64 = hours.trim().parse().ok()?;

    if !parsed.is_finite() || parsed <= 0.0 {
        return None;
    }

    let secs = (parsed * 60.0 * 60.0).round();

    if secs >= u64::MAX as f64 {
        return None;
    }

    #[allow(
        clippy::cast_sign_loss,
        clippy::cast_possible_truncation,
        reason = "positive, finite and bounded just above"
    )]
    now.checked_add(secs as u64)
}

/// How long is left, for a notification to say.
fn describe(secs: u64) -> String {
    if secs >= 60 * 60 {
        #[allow(clippy::cast_precision_loss, reason = "hours, shown to one decimal")]
        return format!("{:.1}h", secs as f64 / (60.0 * 60.0));
    }

    format!("{}m", secs.div_ceil(60))
}

fn now() -> u64 {
    SystemTime::now()
        .duration_since(UNIX_EPOCH)
        .unwrap_or_default()
        .as_secs()
}

fn path() -> String {
    env::get().state("idle-inhibit-until")
}

/// The deadline in force, if there is one.
fn load() -> Option<u64> {
    let path = path();

    match std::fs::read_to_string(&path) {
        Ok(contents) => match contents.trim().parse() {
            Ok(deadline) => Some(deadline),
            Err(e) => {
                warn!(%e, path, "unable to parse the idle inhibit deadline");
                None
            }
        },
        // Nothing is inhibited, which is the usual case.
        Err(e) if e.kind() == std::io::ErrorKind::NotFound => None,
        Err(e) => {
            warn!(%e, path, "unable to read the idle inhibit deadline");
            None
        }
    }
}

fn save(deadline: u64) -> std::io::Result<()> {
    let path = path();

    if let Some(dir) = std::path::Path::new(&path).parent() {
        std::fs::create_dir_all(dir)?;
    }

    std::fs::write(path, format!("{deadline}\n"))
}

/// Forget the deadline, so that nothing later acts on it.
///
/// A file that cannot be removed would restore the daemon a second time after
/// the next restart, which is a stray notification and a daemon restarted under
/// itself -- worth a line in the log, not worth stopping for.
fn clear() {
    let path = path();

    match std::fs::remove_file(&path) {
        Ok(()) => info!(path, "cleared the idle inhibit deadline"),
        Err(e) if e.kind() == std::io::ErrorKind::NotFound => (),
        Err(e) => warn!(%e, path, "unable to clear the idle inhibit deadline"),
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn hours_become_a_deadline() {
        assert_eq!(deadline_from("2", 1_000), Some(1_000 + 2 * 60 * 60));
        assert_eq!(deadline_from(" 0.5 ", 0), Some(30 * 60));
    }

    #[test]
    fn nonsense_has_no_deadline() {
        for hours in ["", "soon", "0", "-1", "NaN", "inf", "1e30"] {
            assert_eq!(deadline_from(hours, 1_000), None, "{hours}");
        }
    }

    /// The point of the file: a second call replaces the deadline, and the
    /// first call's thread can tell that it did.
    #[test]
    fn a_later_call_gets_a_different_deadline() {
        let first = deadline_from("8", 1_000);
        let shorter = deadline_from("1", 1_000);
        let longer = deadline_from("12", 1_000);

        assert_ne!(first, shorter);
        assert_ne!(first, longer);
    }

    #[test]
    fn remaining_time_reads_as_hours_or_minutes() {
        assert_eq!(describe(9 * 60 * 60), "9.0h");
        assert_eq!(describe(150 * 60), "2.5h");
        assert_eq!(describe(45 * 60), "45m");
        assert_eq!(describe(90), "2m", "rounded up, never 'in 0m'");
        assert_eq!(describe(1), "1m");
    }
}
