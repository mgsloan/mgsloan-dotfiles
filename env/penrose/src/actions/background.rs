//! The desktop background.
//!
//! A random image from `~/pics/wiki-loves-earth`, changed hourly and on
//! demand. The list is cached because the directory changes about as often as
//! the pictures in it do, and walking it on every change would be pointless
//! work an hour at a time.

use std::{
    fs::File,
    io::{BufRead, BufReader},
    path::{Path, PathBuf},
    sync::Mutex,
    thread,
    time::Duration,
};

use penrose::{builtin::actions::key_handler, core::bindings::KeyEventHandler};
use serde::Deserialize;
use tracing::{info, warn};

use crate::{Conn, env, notify::notify, process, programs};

const HOURLY: Duration = Duration::from_secs(60 * 60);
const CURRENT_FILE: &str = "penrose-current-background";

/// The known images, built on first use and rebuilt on demand.
static BACKGROUNDS: Mutex<Option<Vec<PathBuf>>> = Mutex::new(None);

/// The background selected by this process.
static CURRENT: Mutex<Option<PathBuf>> = Mutex::new(None);

#[derive(Deserialize)]
struct Metadata {
    commons_title: String,
    commons_page: String,
    local_file: PathBuf,
    description: Option<String>,
    artist: Option<String>,
}

#[derive(Deserialize)]
struct Selection {
    year: u16,
    title: String,
}

struct BackgroundInfo {
    year: u16,
    description: Option<String>,
    artist: Option<String>,
    commons_page: String,
}

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
    set(&env::get().home("env/desktop/solid_white.png"));
}

/// Show the Wiki Loves Earth details for the current background.
pub fn info() {
    let Some(path) = current() else {
        notify("No current background");
        return;
    };

    match metadata(&path) {
        Ok(Some(metadata)) => {
            let description = metadata.description.as_deref().unwrap_or("No description");
            let artist = metadata.artist.as_deref().unwrap_or("Unknown artist");
            notify(&format!(
                "Wiki Loves Earth {} winner\n{description}\n— {artist}",
                metadata.year
            ));
        }
        Ok(None) => notify("The current background has no Wiki Loves Earth metadata"),
        Err(e) => notify(&format!("Background metadata: {e}")),
    }
}

/// Open the current background's Wikimedia Commons page.
pub fn open() {
    let Some(path) = current() else {
        notify("No current background");
        return;
    };

    match metadata(&path) {
        Ok(Some(metadata)) => {
            if let Err(e) = process::spawn("xdg-open", &[&metadata.commons_page]) {
                warn!(%e, "unable to open background page");
            }
        }
        Ok(None) => notify("The current background has no Wikimedia Commons page"),
        Err(e) => notify(&format!("Background metadata: {e}")),
    }
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
    } else {
        *CURRENT.lock().expect("current background lock") = Some(PathBuf::from(path));
        if let Err(e) = std::fs::write(current_file(), path) {
            warn!(%e, "unable to record current background");
        }
    }
}

fn current_file() -> PathBuf {
    PathBuf::from(std::env::var("XDG_RUNTIME_DIR").unwrap_or_else(|_| "/tmp".to_owned()))
        .join(CURRENT_FILE)
}

fn current() -> Option<PathBuf> {
    CURRENT.lock().expect("current background lock").clone()
}

fn metadata(path: &Path) -> Result<Option<BackgroundInfo>, String> {
    let directory = PathBuf::from(env::get().home("pics/wiki-loves-earth"));
    let metadata: Option<Metadata> =
        find_json_line(directory.join("metadata.jsonl"), |metadata: &Metadata| {
            metadata.local_file.file_name() == path.file_name()
        })?;
    let Some(metadata) = metadata else {
        return Ok(None);
    };
    let selection: Option<Selection> = find_json_line(
        directory.join("selection.jsonl"),
        |selection: &Selection| selection.title == metadata.commons_title,
    )?;
    let Some(selection) = selection else {
        return Err(format!("no selection for {}", metadata.commons_title));
    };

    Ok(Some(BackgroundInfo {
        year: selection.year,
        description: metadata.description,
        artist: metadata.artist,
        commons_page: metadata.commons_page,
    }))
}

fn find_json_line<T: for<'de> Deserialize<'de>>(
    path: PathBuf,
    predicate: impl Fn(&T) -> bool,
) -> Result<Option<T>, String> {
    let file = File::open(&path).map_err(|e| format!("{}: {e}", path.display()))?;
    for line in BufReader::new(file).lines() {
        let line = line.map_err(|e| format!("{}: {e}", path.display()))?;
        let value = serde_json::from_str(&line).map_err(|e| format!("{}: {e}", path.display()))?;
        if predicate(&value) {
            return Ok(Some(value));
        }
    }

    Ok(None)
}

/// The cached list, building it if this is the first call since a rebuild.
fn ensure() -> Result<Vec<PathBuf>, String> {
    let mut cached = BACKGROUNDS
        .lock()
        .map_err(|e| format!("poisoned lock: {e}"))?;

    if let Some(backgrounds) = cached.as_ref() {
        return Ok(backgrounds.clone());
    }

    let dir = env::get().home("pics/wiki-loves-earth");
    let backgrounds = scan(&dir)?;

    info!(count = backgrounds.len(), dir, "found backgrounds");
    *cached = Some(backgrounds.clone());

    Ok(backgrounds)
}

/// Every image in the aspect-ratio-filtered manifest.
fn scan(dir: &str) -> Result<Vec<PathBuf>, String> {
    let manifest = Path::new(dir).join("wallpapers.txt");
    let file = File::open(&manifest).map_err(|e| format!("{}: {e}", manifest.display()))?;

    BufReader::new(file)
        .lines()
        .map(|line| {
            line.map(|relative| Path::new(dir).join(relative))
                .map_err(|e| format!("{}: {e}", manifest.display()))
        })
        .collect()
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
