//! The desktop background.
//!
//! A random image from `~/env/untracked/backgrounds`, changed hourly and on
//! demand. The list is cached because the directory changes about as often as
//! the pictures in it do, and walking it on every change would be pointless
//! work an hour at a time.

use std::{path::PathBuf, sync::Mutex, thread, time::Duration};

use penrose::{builtin::actions::key_handler, core::bindings::KeyEventHandler};
use tracing::{info, warn};

use crate::{Conn, env, notify::notify, programs};

const HOURLY: Duration = Duration::from_secs(60 * 60);

/// The known images, built on first use and rebuilt on demand.
static BACKGROUNDS: Mutex<Option<Vec<PathBuf>>> = Mutex::new(None);

/// `M-b M-g`: change the background now.
pub fn random_binding() -> Box<dyn KeyEventHandler<Conn>> {
    key_handler(|_, _| {
        thread::spawn(random);

        Ok(())
    })
}

/// Pick an image and put it up.
pub fn random() {
    let backgrounds = match ensure() {
        Ok(backgrounds) => backgrounds,
        Err(e) => {
            warn!(error = %e, "not changing the background");
            return;
        }
    };

    let Some(path) = choose(&backgrounds) else {
        warn!("no backgrounds to choose from");
        return;
    };

    set(&path.to_string_lossy());
}

/// Start the hourly rotation.
///
/// Deliberately not a timer that survives `M-q`: a restart simply starts a new
/// one, which at worst means one background lasts less than an hour.
pub fn start_rotation() {
    thread::spawn(|| {
        loop {
            // Our own thread, so this blocks nothing: the whole point of
            // spawning it is that the event loop keeps running.
            #[allow(clippy::disallowed_methods, reason = "not the event loop thread")]
            thread::sleep(HOURLY);

            random();
        }
    });
}

/// A plain white background, for screen sharing.
pub fn white() {
    set(&env::get().home("env/solid_white.png"));
}

/// Rebuild the cached list, for after adding images.
pub fn update() {
    *BACKGROUNDS.lock().expect("background lock") = None;

    match ensure() {
        Ok(backgrounds) => notify(&format!("{} backgrounds", backgrounds.len())),
        Err(e) => notify(&format!("Backgrounds: {e}")),
    }
}

fn set(path: &str) {
    if let Err(e) = programs::set_background(path) {
        warn!(%e, path, "unable to set the background");
    }
}

/// The cached list, building it if this is the first call since a rebuild.
fn ensure() -> Result<Vec<PathBuf>, String> {
    let mut cached = BACKGROUNDS
        .lock()
        .map_err(|e| format!("poisoned lock: {e}"))?;

    if let Some(backgrounds) = cached.as_ref() {
        return Ok(backgrounds.clone());
    }

    let dir = env::get().home("env/untracked/backgrounds");
    let backgrounds = scan(&dir)?;

    info!(count = backgrounds.len(), dir, "found backgrounds");
    *cached = Some(backgrounds.clone());

    Ok(backgrounds)
}

/// Every `.jpg` under `dir`, recursively.
fn scan(dir: &str) -> Result<Vec<PathBuf>, String> {
    let entries = std::fs::read_dir(dir).map_err(|e| format!("{dir}: {e}"))?;
    let mut found = Vec::new();

    for entry in entries.flatten() {
        let path = entry.path();

        if path.is_dir() {
            // A failed subdirectory should not lose the ones that worked.
            match scan(&path.to_string_lossy()) {
                Ok(nested) => found.extend(nested),
                Err(e) => warn!(error = %e, "skipping subdirectory"),
            }
        } else if path
            .extension()
            .is_some_and(|e| e.eq_ignore_ascii_case("jpg"))
        {
            found.push(path);
        }
    }

    Ok(found)
}

/// Pick one, arbitrarily.
///
/// The clock's nanoseconds are the entropy: choosing a wallpaper does not
/// justify a random number generator, and consecutive presses land on different
/// images, which is the whole requirement.
fn choose(backgrounds: &[PathBuf]) -> Option<&PathBuf> {
    if backgrounds.is_empty() {
        return None;
    }

    let nanos = jiff::Timestamp::now().subsec_nanosecond().unsigned_abs() as usize;

    backgrounds.get(nanos % backgrounds.len())
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn nothing_to_choose_from_is_not_a_panic() {
        assert!(choose(&[]).is_none());
    }

    #[test]
    fn choosing_stays_in_bounds() {
        let backgrounds: Vec<PathBuf> = (0..3).map(|i| PathBuf::from(format!("{i}.jpg"))).collect();

        for _ in 0..100 {
            assert!(choose(&backgrounds).is_some());
        }
    }
}
