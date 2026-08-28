//! `M-x blindfold`: pause the periodic webcam capture for a while.
//!
//! Capture runs from a systemd user timer (`~/env/scripts/webcam-capture.sh`),
//! outside this process, so unlike `idle::inhibit` there is no daemon here to
//! stop and restart. Instead this writes an expiry deadline that the script
//! checks on every run and skips while now is before it -- self-expiring, so
//! nothing here has to un-inhibit it later. The same script also skips while
//! the screen is locked, unconditionally, with no state file involved.
//!
//! The sleeping thread below exists only to send a "resumed" notification once
//! the deadline passes; it does not do anything the script itself needs.

use std::{
    thread,
    time::{Duration, SystemTime, UNIX_EPOCH},
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
    let now = SystemTime::now()
        .duration_since(UNIX_EPOCH)
        .unwrap_or_default()
        .as_secs();
    let until = now + secs.round() as u64;

    if let Err(e) = write(until) {
        error!(%e, "unable to write the webcam inhibit deadline");
        notify("Unable to inhibit the webcam");
        return;
    }

    notify(&format!("Webcam capture inhibited for {}m", minutes.trim()));

    thread::spawn(move || {
        #[allow(clippy::disallowed_methods, reason = "not the event loop thread")]
        thread::sleep(Duration::from_secs_f64(secs));

        notify("Webcam capture resumed");
    });
}

fn path() -> String {
    match std::env::var("XDG_STATE_HOME") {
        Ok(dir) if !dir.is_empty() => format!("{dir}/penrose/webcam-inhibit-until"),
        _ => env::get().home(".local/state/penrose/webcam-inhibit-until"),
    }
}

fn write(until: u64) -> std::io::Result<()> {
    let path = path();

    if let Some(dir) = std::path::Path::new(&path).parent() {
        std::fs::create_dir_all(dir)?;
    }

    std::fs::write(path, format!("{until}\n"))
}
