//! `M-x inhibit-idle`: pause screen blanking and suspend for a while.
//!
//! There is no runtime pause in either idle daemon (`programs.rs`'s table), so
//! this asks the same question xmonad never had to: stop the daemon outright,
//! and start a fresh one once the requested time is up. That is exactly what
//! `M-x startup-misc` already does after a config change, so the only new part
//! here is the wait -- a thread that sleeps and then calls the same
//! [programs::start_idle_daemon] startup calls, per design.md §10.
//!
//! Not a timer that survives `M-q`, same as the hourly background rotation: a
//! restart kills the thread along with everything else on it, and the daemon
//! stays down until something restarts it by hand. Rare enough, and a `M-x
//! startup-misc` away, not to be worth surviving a restart for.

use std::{thread, time::Duration};

use tracing::info;

use crate::{notify::notify, programs};

/// `M-x inhibit-idle`: ask how many hours, then hold off blanking and
/// suspend for that long.
pub fn inhibit(hours: &str) {
    let parsed: f64 = hours.trim().parse().unwrap_or(f64::NAN);

    if !parsed.is_finite() || parsed <= 0.0 {
        notify(&format!("Not a number of hours: {hours}"));
        return;
    }

    programs::stop_idle_daemon();
    notify(&format!("Idle inhibited for {}h", hours.trim()));

    thread::spawn(move || {
        #[allow(clippy::disallowed_methods, reason = "not the event loop thread")]
        thread::sleep(Duration::from_secs_f64(parsed * 60.0 * 60.0));

        info!(hours = parsed, "restoring the idle daemon");
        programs::start_idle_daemon();
        notify("Idle daemon restored");
    });
}
