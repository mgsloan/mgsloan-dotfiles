//! `M-x blindfold`: pause the periodic webcam capture for a while.
//!
//! Capture runs from a systemd user timer (`~/env/desktop/scripts/webcam-capture.sh`),
//! outside this process, so unlike `idle::inhibit` there is no daemon here to
//! stop and restart. Instead this writes an expiry deadline that the script
//! checks on every run and skips while now is before it -- self-expiring, so
//! nothing here has to un-inhibit it later. The same script also skips while
//! the screen is locked, unconditionally, with no state file involved.
//!
//! The sleeping thread only sends notifications before and at expiry. Startup
//! rearms it from the deadline file; capture does not depend on it.

use std::{
    thread,
    time::{SystemTime, UNIX_EPOCH},
};

use tracing::error;

use crate::{env, notify::notify};

/// `M-x blindfold`: ask how many minutes, then skip captures for that long.
pub fn inhibit(minutes: &str) {
    let parsed: f64 = minutes.trim().parse().unwrap_or(f64::NAN);

    if !parsed.is_finite() || parsed <= 0.0 {
        notify(&format!("Not a number of minutes: {minutes}"));
        return;
    }

    let secs = parsed * 60.0;
    let until = now() + secs.round() as u64;

    if let Err(e) = write(until) {
        error!(%e, "unable to write the webcam inhibit deadline");
        notify("Unable to blindfold");
        return;
    }

    notify(&format!("Blindfolded for {}m", minutes.trim()));

    notify_until(until);
}

pub fn startup() {
    if let Some(until) = load().filter(|until| *until > now()) {
        notify_until(until);
    }
}

fn notify_until(until: u64) {
    thread::spawn(move || {
        for (deadline, message) in [
            (until.saturating_sub(60), "Unblindfolding in 1 minute"),
            (until, "Unblindfolded"),
        ] {
            // Short pauses and restarts within the last minute cannot give a
            // full minute's warning.
            if deadline < until && deadline < now() {
                continue;
            }
            if let Err(error) = crate::time::sleep_until(deadline) {
                error!(%error, "unable to wait for the blindfold deadline");
                return;
            }
            if load() != Some(until) {
                return;
            }
            // Suspend may have carried us past both notification deadlines.
            if deadline < until && now() >= until {
                continue;
            }
            notify(message);
        }
    });
}

fn now() -> u64 {
    SystemTime::now()
        .duration_since(UNIX_EPOCH)
        .unwrap_or_default()
        .as_secs()
}

fn load() -> Option<u64> {
    std::fs::read_to_string(path()).ok()?.trim().parse().ok()
}

fn path() -> String {
    env::get().state("webcam-inhibit-until")
}

fn write(until: u64) -> std::io::Result<()> {
    let path = path();

    if let Some(dir) = std::path::Path::new(&path).parent() {
        std::fs::create_dir_all(dir)?;
    }

    std::fs::write(path, format!("{until}\n"))
}
