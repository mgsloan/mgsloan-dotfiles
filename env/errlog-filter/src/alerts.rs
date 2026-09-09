use std::{
    collections::VecDeque,
    fs::{self, OpenOptions},
    io::{self, Read, Seek, Write},
    os::{fd::AsRawFd, unix::fs::OpenOptionsExt},
    path::PathBuf,
    process::{Command, Stdio},
    thread,
};

const WINDOW: i64 = 60_000;
const INTERVAL: i64 = 300_000;

struct Rate {
    threshold: usize,
    recent: VecDeque<i64>,
}

impl Rate {
    fn record(&mut self, timestamp: i64, now: i64) -> bool {
        // Clock corrections must not leave future entries in the window.
        if self.recent.back().is_some_and(|last| timestamp < *last) {
            self.recent.clear();
        }
        while self
            .recent
            .front()
            .is_some_and(|first| now - first >= WINDOW)
        {
            self.recent.pop_front();
        }
        if timestamp > now || now - timestamp >= WINDOW {
            return false;
        }
        if self.recent.len() == self.threshold {
            self.recent.pop_front();
        }
        self.recent.push_back(timestamp);
        self.recent.len() == self.threshold
    }
}

pub struct Alerts {
    rate: Rate,
    started: i64,
    next_check: i64,
    directory: PathBuf,
    inhibit: PathBuf,
}

impl Alerts {
    pub fn new(threshold: usize) -> crate::Result<Self> {
        let state = std::env::var_os("XDG_STATE_HOME")
            .filter(|value| !value.is_empty())
            .map(PathBuf::from)
            .unwrap_or(crate::home()?.join(".local/state"));
        Ok(Self {
            rate: Rate {
                threshold,
                recent: VecDeque::new(),
            },
            started: jiff::Timestamp::now().as_millisecond(),
            next_check: 0,
            directory: state.join("errlog-filter"),
            inhibit: state.join("penrose/error-alerts-inhibit-until"),
        })
    }

    pub fn record(&mut self, line: &[u8]) {
        let Some(timestamp) = line
            .split(|byte| *byte == b' ')
            .next()
            .and_then(|value| std::str::from_utf8(value).ok())
            .and_then(|value| value.parse::<jiff::Timestamp>().ok())
            .map(|value| value.as_millisecond())
        else {
            return;
        };
        // Replayed history remains visible, but cannot trigger notifications.
        if timestamp < self.started {
            return;
        }
        let now = jiff::Timestamp::now().as_millisecond();
        if !self.rate.record(timestamp, now) || now < self.next_check {
            return;
        }
        self.next_check = now + 1_000;
        match self.claim_notification(now) {
            Ok(true) => {
                self.next_check = now + INTERVAL;
                let body = format!(
                    "At least {} unfiltered errors in the last minute. M-x earplugs to mute these alerts temporarily.",
                    self.rate.threshold
                );
                match Command::new("notify-send")
                    .args([
                        "--app-name=errlog-filter",
                        "--urgency=normal",
                        "High error frequency",
                        &body,
                    ])
                    .stdin(Stdio::null())
                    .stdout(Stdio::null())
                    .stderr(Stdio::null())
                    .spawn()
                {
                    Ok(mut child) => {
                        thread::spawn(move || {
                            let _ = child.wait();
                        });
                    }
                    Err(error) => eprintln!("errlog-filter: unable to notify: {error}"),
                }
            }
            Ok(false) => {}
            Err(error) => {
                self.next_check = now + INTERVAL;
                eprintln!("errlog-filter: unable to save alert state: {error}");
            }
        }
    }

    fn claim_notification(&self, now: i64) -> io::Result<bool> {
        match fs::read_to_string(&self.inhibit) {
            Ok(contents)
                if contents
                    .trim()
                    .parse::<i64>()
                    .is_ok_and(|until| until > now / 1000) =>
            {
                return Ok(false);
            }
            Err(error) if error.kind() != io::ErrorKind::NotFound => return Err(error),
            _ => {}
        }
        fs::create_dir_all(&self.directory)?;
        let mut file = OpenOptions::new()
            .read(true)
            .write(true)
            .create(true)
            .truncate(false)
            .mode(0o600)
            .open(self.directory.join("last-alert"))?;
        // Share the cooldown across terminal restarts and concurrent filters.
        if unsafe { libc::flock(file.as_raw_fd(), libc::LOCK_EX | libc::LOCK_NB) } != 0 {
            let error = io::Error::last_os_error();
            return if error.kind() == io::ErrorKind::WouldBlock {
                Ok(false)
            } else {
                Err(error)
            };
        }
        let mut contents = String::new();
        file.read_to_string(&mut contents)?;
        if contents
            .trim()
            .parse::<i64>()
            .is_ok_and(|last| now.saturating_sub(last) < INTERVAL)
        {
            return Ok(false);
        }
        file.rewind()?;
        file.set_len(0)?;
        write!(file, "{now}")?;
        Ok(true)
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn rolling_window_is_bounded_and_expires_after_silence() {
        let mut rate = Rate {
            threshold: 3,
            recent: VecDeque::new(),
        };
        assert!(!rate.record(0, 0));
        assert!(!rate.record(10_000, 10_000));
        assert!(rate.record(59_999, 59_999));
        assert!(rate.record(60_000, 60_000));
        for timestamp in 60_001..70_000 {
            assert!(rate.record(timestamp, timestamp));
        }
        assert_eq!(rate.recent.len(), 3);
        assert!(!rate.record(130_000, 130_000));
    }

    #[test]
    fn delayed_future_and_backwards_timestamps_do_not_inflate_rate() {
        let mut rate = Rate {
            threshold: 2,
            recent: VecDeque::new(),
        };
        assert!(!rate.record(0, 60_000));
        assert!(!rate.record(70_000, 60_000));
        assert!(!rate.record(60_000, 60_000));
        assert!(!rate.record(50_000, 50_000));
        assert!(rate.record(51_000, 51_000));
    }

    #[test]
    fn mute_expiry_and_cooldown_survive_new_monitors() {
        let directory = std::env::temp_dir().join(format!("errlog-alerts-{}", std::process::id()));
        fs::create_dir_all(&directory).unwrap();
        let make = || Alerts {
            rate: Rate {
                threshold: 2,
                recent: VecDeque::new(),
            },
            started: 0,
            next_check: 0,
            directory: directory.clone(),
            inhibit: directory.join("mute"),
        };
        let alerts = make();
        fs::write(&alerts.inhibit, "1000\n").unwrap();
        assert!(!alerts.claim_notification(999_000).unwrap());
        assert!(alerts.claim_notification(1_000_000).unwrap());
        assert!(!make().claim_notification(1_299_999).unwrap());
        assert!(make().claim_notification(1_300_000).unwrap());
        fs::remove_dir_all(directory).unwrap();
    }
}
