//! Performance vs. power-saver, or automatic based on AC status.
//!
//! Unlike `toggles`, there is no in-process state to restore: the mode lives
//! in a file that a udev rule and a boot-time systemd unit also read, so
//! "auto" keeps tracking AC status even when penrose is not running. This
//! module only has to write that file and run the same script they do -- see
//! `~/env/scripts/cpu-governor-apply.sh`, `~/env/udev-rules/99-cpu-governor.rules`
//! and `~/env/systemd/cpu-governor.service`.

use tracing::error;

use crate::{env, menu, notify::notify, process};

enum Mode {
    Performance,
    Powersave,
    Auto,
}

impl Mode {
    fn as_str(&self) -> &'static str {
        match self {
            Self::Performance => "performance",
            Self::Powersave => "powersave",
            Self::Auto => "auto",
        }
    }
}

/// `M-x cpu-governer`: ask which mode, then set it.
pub fn menu() {
    match menu::select("cpu governor: ", &["performance", "powersave", "auto"]).as_deref() {
        Some("performance") => set(Mode::Performance),
        Some("powersave") => set(Mode::Powersave),
        Some("auto") => set(Mode::Auto),
        Some(other) => notify(&format!("No cpu governor mode matching {other}")),
        None => (),
    }
}

/// Re-resolve and re-apply whatever mode is currently stored.
///
/// Run on every startup and restart (`startup::every_run`), alongside the
/// udev rule and boot unit, so the profile is right immediately after login
/// even if AC status changed while nothing was watching.
pub fn apply() {
    if let Err(e) = process::status(&env::get().script("cpu-governor-apply.sh"), &[]) {
        error!(%e, "unable to apply the cpu governor");
    }
}

fn set(mode: Mode) {
    if let Err(e) = write(&mode) {
        error!(%e, mode = mode.as_str(), "unable to write the cpu governor mode");
        notify("Unable to set the CPU governor mode");
        return;
    }

    apply();
    notify(&format!("CPU governor: {}", mode.as_str()));
}

fn path() -> String {
    match std::env::var("XDG_STATE_HOME") {
        Ok(dir) if !dir.is_empty() => format!("{dir}/penrose/cpu-governor-mode"),
        _ => env::get().home(".local/state/penrose/cpu-governor-mode"),
    }
}

fn write(mode: &Mode) -> std::io::Result<()> {
    let path = path();

    if let Some(dir) = std::path::Path::new(&path).parent() {
        std::fs::create_dir_all(dir)?;
    }

    std::fs::write(path, format!("{}\n", mode.as_str()))
}
