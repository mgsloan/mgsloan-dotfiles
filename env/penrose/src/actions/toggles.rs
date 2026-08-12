//! The three settings that have to outlive a restart.
//!
//! In xmonad these are `PersistentExtension`s, serialized into the state file
//! that `M-q` writes and reads back. Penrose has no equivalent — extension
//! state is rebuilt from defaults on every start — and `M-q` is a frequent
//! operation here, so without somewhere to put them redshift would restart, the
//! touchpad would re-enable itself and a focus lock would silently release on
//! every rebuild.
//!
//! So: penrose extension state for the live value, and a small JSON file
//! underneath it. Deriving them from the world instead (`pgrep redshift`,
//! `synclient -l`) would work for two of the three and is impossible for the
//! lock, which exists only in the window manager's head.

use std::{cell::RefCell, sync::Arc};

use penrose::core::State;
use serde::{Deserialize, Serialize};
use tracing::{error, info, warn};

use crate::{Conn, env, notify::notify, programs};

#[derive(Debug, Clone, Copy, Serialize, Deserialize)]
#[serde(default)]
pub struct Toggles {
    /// Whether redshift is tinting the display.
    pub redshift: bool,
    /// Whether the touchpad accepts input.
    pub touchpad: bool,
    /// Whether tag switching is refused, for staying on one thing.
    pub switching_locked: bool,
}

/// Matches xmonad's `initialValue` for each of the three.
impl Default for Toggles {
    fn default() -> Self {
        Self {
            redshift: true,
            touchpad: false,
            switching_locked: false,
        }
    }
}

/// Load the stored values and apply the ones that drive something.
///
/// `restarted` mirrors what the xmonad config did with its two startup halves:
/// redshift is only started once per session, and the touchpad is forced off at
/// the beginning of a session but left alone across a rebuild.
pub fn startup(state: &mut State<Conn>, restarted: bool) {
    let stored = load();

    if !restarted {
        apply_redshift(stored.redshift);
        apply_touchpad(false);
    }

    let toggles = Toggles {
        touchpad: if restarted { stored.touchpad } else { false },
        ..stored
    };

    *get(state).borrow_mut() = toggles;
    info!(?toggles, "loaded toggles");

    if toggles.switching_locked {
        notify("Tag switching is still locked");
    }
}

/// Is tag switching currently refused?
///
/// Checked by the `M-<tag>` bindings, which are the only route to a switch.
pub fn switching_locked(state: &mut State<Conn>) -> bool {
    get(state).borrow().switching_locked
}

pub fn toggle_redshift(state: &mut State<Conn>) {
    update(state, |t| t.redshift = !t.redshift);
    let on = get(state).borrow().redshift;

    apply_redshift(on);
    notify(if on { "Redshift on" } else { "Redshift off" });
}

pub fn toggle_touchpad(state: &mut State<Conn>) {
    update(state, |t| t.touchpad = !t.touchpad);
    let on = get(state).borrow().touchpad;

    apply_touchpad(on);
    notify(if on { "Touchpad on" } else { "Touchpad off" });
}

pub fn lock_switching(state: &mut State<Conn>) {
    update(state, |t| t.switching_locked = true);
    notify("Tag switching locked");
}

pub fn unlock_switching(state: &mut State<Conn>) {
    update(state, |t| t.switching_locked = false);
    notify("Tag switching unlocked");
}

/// The live values, defaulted on first access.
fn get(state: &mut State<Conn>) -> Arc<RefCell<Toggles>> {
    state.extension_or_default::<Toggles>()
}

/// Change the values and write them out, so the change survives `M-q`.
fn update(state: &mut State<Conn>, f: impl FnOnce(&mut Toggles)) {
    let toggles = get(state);
    let mut toggles = toggles.borrow_mut();

    f(&mut toggles);
    save(&toggles);
}

fn apply_redshift(on: bool) {
    // Location and temperature range as in the xmonad config; which daemon
    // provides it is programs.rs's business, since redshift is X11 only.
    let result = if on {
        programs::start_night_colours()
    } else {
        programs::stop_night_colours()
    };

    if let Err(e) = result {
        error!(%e, on, "unable to change the night colours");
    }
}

fn apply_touchpad(on: bool) {
    if let Err(e) = programs::set_touchpad(on) {
        error!(%e, on, "unable to change the touchpad");
    }
}

/// `~/.local/state/penrose/toggles.json`, per the XDG state directory.
fn path() -> String {
    match std::env::var("XDG_STATE_HOME") {
        Ok(dir) if !dir.is_empty() => format!("{dir}/penrose/toggles.json"),
        _ => env::get().home(".local/state/penrose/toggles.json"),
    }
}

/// Read the stored values, falling back to defaults.
///
/// A missing file is the normal first-run case and says nothing; anything else
/// is worth a line in the log, since the values are about to be silently
/// forgotten.
fn load() -> Toggles {
    let path = path();

    let contents = match std::fs::read_to_string(&path) {
        Ok(contents) => contents,
        Err(e) if e.kind() == std::io::ErrorKind::NotFound => return Toggles::default(),
        Err(e) => {
            warn!(%e, path, "unable to read toggles, using defaults");
            return Toggles::default();
        }
    };

    match serde_json::from_str(&contents) {
        Ok(toggles) => toggles,
        Err(e) => {
            warn!(%e, path, "unable to parse toggles, using defaults");
            Toggles::default()
        }
    }
}

/// Write the values out, replacing the file atomically.
///
/// Writing in place would leave a truncated file behind if the window manager
/// died mid-write, and this is read at startup — the one moment where a corrupt
/// file is least welcome.
fn save(toggles: &Toggles) {
    let path = path();
    let tmp = format!("{path}.tmp");

    let json = match serde_json::to_string_pretty(toggles) {
        Ok(json) => json,
        Err(e) => {
            error!(%e, "unable to serialize toggles");
            return;
        }
    };

    if let Some(dir) = std::path::Path::new(&path).parent()
        && let Err(e) = std::fs::create_dir_all(dir)
    {
        error!(%e, ?dir, "unable to create the state directory");
        return;
    }

    if let Err(e) = std::fs::write(&tmp, json) {
        error!(%e, tmp, "unable to write toggles");
        return;
    }

    if let Err(e) = std::fs::rename(&tmp, &path) {
        error!(%e, path, "unable to replace toggles");
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn defaults_match_the_xmonad_initial_values() {
        let d = Toggles::default();

        assert!(d.redshift, "RedShiftEnabled");
        assert!(!d.touchpad, "TouchpadInactive");
        assert!(!d.switching_locked, "SwitchingNotLocked");
    }

    #[test]
    fn round_trips_through_json() {
        let toggles = Toggles {
            redshift: false,
            touchpad: true,
            switching_locked: true,
        };

        let json = serde_json::to_string(&toggles).unwrap();
        let read: Toggles = serde_json::from_str(&json).unwrap();

        assert_eq!(read.redshift, toggles.redshift);
        assert_eq!(read.touchpad, toggles.touchpad);
        assert_eq!(read.switching_locked, toggles.switching_locked);
    }

    /// A file written by an older version, or hand-edited, should not throw
    /// away the settings it does contain.
    #[test]
    fn missing_fields_fall_back_to_defaults() {
        let read: Toggles = serde_json::from_str(r#"{"redshift": false}"#).unwrap();

        assert!(!read.redshift);
        assert!(!read.touchpad, "defaulted");
        assert!(!read.switching_locked, "defaulted");
    }
}
