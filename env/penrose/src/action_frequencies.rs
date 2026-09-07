//! Counts completed bindings and recognized M-x selections, before their handlers run.

use std::{
    collections::BTreeMap,
    fs, io,
    path::PathBuf,
    sync::{Mutex, OnceLock},
    thread,
    time::Duration,
};

use tracing::error;

static FREQUENCIES: OnceLock<Mutex<Frequencies>> = OnceLock::new();

struct Frequencies {
    path: PathBuf,
    counts: BTreeMap<String, u64>,
    dirty: bool,
}

impl Frequencies {
    fn load(path: PathBuf) -> io::Result<Self> {
        let counts = match fs::read(&path) {
            Ok(contents) => serde_json::from_slice(&contents)?,
            Err(error) if error.kind() == io::ErrorKind::NotFound => BTreeMap::new(),
            Err(error) => return Err(error),
        };
        Ok(Self {
            path,
            counts,
            dirty: false,
        })
    }

    fn record(&mut self, name: &str) {
        let count = self.counts.entry(name.to_owned()).or_default();
        let next = count.saturating_add(1);
        self.dirty |= next != *count;
        *count = next;
    }

    fn flush(&mut self) -> io::Result<bool> {
        if !self.dirty {
            return Ok(false);
        }

        if let Some(parent) = self.path.parent() {
            fs::create_dir_all(parent)?;
        }
        // Rename keeps an interrupted write from destroying the previous totals.
        let temporary = self.path.with_extension("json.tmp");
        fs::write(&temporary, serde_json::to_vec_pretty(&self.counts)?)?;
        fs::rename(temporary, &self.path)?;
        self.dirty = false;
        Ok(true)
    }
}

pub fn init() {
    let path = PathBuf::from(crate::env::get().home("env/untracked/action-frequencies.json"));
    let frequencies = match Frequencies::load(path) {
        Ok(frequencies) => frequencies,
        Err(error) => {
            // Preserve unreadable totals instead of replacing them with an empty map.
            error!(%error, "unable to load action frequencies; tracking disabled");
            return;
        }
    };
    if FREQUENCIES.set(Mutex::new(frequencies)).is_err() {
        return;
    }

    thread::spawn(|| {
        loop {
            #[allow(clippy::disallowed_methods, reason = "on the frequency writer thread")]
            thread::sleep(Duration::from_secs(5 * 60));
            flush();
        }
    });
}

pub fn record(name: &str) {
    if let Some(frequencies) = FREQUENCIES.get() {
        frequencies.lock().expect("frequency lock").record(name);
    }
}

pub fn flush() {
    if let Some(frequencies) = FREQUENCIES.get()
        && let Err(error) = frequencies.lock().expect("frequency lock").flush()
    {
        error!(%error, "unable to save action frequencies; will retry");
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn persists_totals_only_when_changed_and_retries_failed_writes() {
        let directory =
            std::env::temp_dir().join(format!("penrose-frequencies-{}", std::process::id()));
        fs::create_dir_all(&directory).unwrap();
        let path = directory.join("action-frequencies.json");
        fs::write(&path, r#"{"M-m M-l": 3}"#).unwrap();
        let mut frequencies = Frequencies::load(path.clone()).unwrap();
        assert!(!frequencies.flush().unwrap());
        frequencies.record("M-m M-l");
        frequencies.record("bg-white");

        let temporary = path.with_extension("json.tmp");
        fs::create_dir(&temporary).unwrap();
        assert!(frequencies.flush().is_err());
        assert_eq!(
            Frequencies::load(path.clone()).unwrap().counts["M-m M-l"],
            3
        );
        fs::remove_dir(&temporary).unwrap();
        assert!(frequencies.flush().unwrap());
        let modified = fs::metadata(&path).unwrap().modified().unwrap();
        assert!(!frequencies.flush().unwrap());
        assert_eq!(fs::metadata(&path).unwrap().modified().unwrap(), modified);

        let loaded = Frequencies::load(path.clone()).unwrap();
        assert_eq!(loaded.counts["M-m M-l"], 4);
        assert_eq!(loaded.counts["bg-white"], 1);
        fs::write(&path, "invalid JSON").unwrap();
        assert!(Frequencies::load(path).is_err());
        fs::remove_dir_all(directory).unwrap();
    }
}
