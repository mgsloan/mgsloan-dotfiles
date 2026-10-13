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
    fs, io,
    ops::RangeInclusive,
    path::Path,
    thread,
    time::{SystemTime, UNIX_EPOCH},
};

use jiff::{
    Timestamp,
    civil::{Date, Time},
    tz::TimeZone,
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

/// `M-x retro-blindfold`: delete JPEGs from an interval in minutes ago.
pub fn retro_blindfold(input: &str) {
    let Some(interval) = parse_interval(input, Timestamp::now().as_second()) else {
        notify("Use minutes ago: 30 or 30-10 (older-newer)");
        return;
    };

    thread::spawn(move || {
        match delete_captures(
            Path::new(&env::get().home("pics/webcam")),
            &interval,
            &TimeZone::system(),
        ) {
            Ok((count, archives)) => {
                let mut message = format!("Retro-blindfold deleted {count} webcam JPEGs");
                if !archives.is_empty() {
                    message.push_str(&format!(
                        "\nWarning: HEVC archives may contain captures from this interval (unchanged): {}",
                        archives.join(", ")
                    ));
                }
                notify(&message);
            }
            Err(error) => {
                error!(%error, "unable to finish retro-blindfold");
                notify(&format!("Unable to finish retro-blindfold: {error}"));
            }
        }
    });
}

fn parse_interval(input: &str, current: i64) -> Option<RangeInclusive<i64>> {
    let mut parts = input.trim().split(['-', '–', '—']);
    let seconds = |minutes: &str| {
        let minutes: f64 = minutes.trim().parse().ok()?;
        let seconds = (minutes * 60.0).round();
        (seconds.is_finite() && minutes >= 0.0 && seconds <= current as f64)
            .then_some(seconds as i64)
    };
    let older = seconds(parts.next()?)?;
    let newer = parts.next().map(seconds).unwrap_or(Some(0))?;
    if parts.next().is_some() || older <= newer {
        return None;
    }
    Some(current.checked_sub(older)?..=current.checked_sub(newer)?)
}

fn delete_captures(
    directory: &Path,
    interval: &RangeInclusive<i64>,
    timezone: &TimeZone,
) -> io::Result<(usize, Vec<String>)> {
    let entries = match fs::read_dir(directory) {
        Ok(entries) => entries,
        Err(error) if error.kind() == io::ErrorKind::NotFound => return Ok((0, Vec::new())),
        Err(error) => return Err(error),
    };
    let local_date = |seconds| {
        Timestamp::from_second(seconds)
            .map(|timestamp| timestamp.to_zoned(timezone.clone()).date())
            .map_err(io::Error::other)
    };
    let dates = local_date(*interval.start())?..=local_date(*interval.end())?;
    let mut captures = Vec::new();
    let mut archives = Vec::new();
    for entry in entries {
        let entry = entry?;
        let file_type = entry.file_type()?;
        let name = entry.file_name();
        let Some(name) = name.to_str() else {
            continue;
        };
        let archive = file_type.is_file() && name.ends_with(".hevc");
        let name = if archive {
            &name[..name.len() - 5]
        } else {
            name
        };
        if name.len() != 10 || (!archive && !file_type.is_dir()) {
            continue;
        }
        let Ok(date) = name.parse::<Date>() else {
            continue;
        };
        if !dates.contains(&date) {
            continue;
        }
        if archive {
            let timestamps = directory.join(format!("{name}-timestamps"));
            if archive_overlaps(&timestamps, date, interval, timezone) {
                archives.push(name.to_owned());
            }
            continue;
        }
        for capture in fs::read_dir(entry.path())? {
            let capture = capture?;
            if !capture.file_type()?.is_file() {
                continue;
            }
            let name = capture.file_name();
            let Some(name) = name.to_str().filter(|name| name.len() == 12) else {
                continue;
            };
            let Some(time) = name.strip_suffix(".jpg") else {
                continue;
            };
            let Ok(time) = Time::strptime("%H-%M-%S", time) else {
                continue;
            };
            // Capture names contain local time, not filesystem modification time.
            if interval.contains(&capture_timestamp(date, time, timezone)?) {
                captures.push(capture.path());
            }
        }
    }

    let mut count = 0;
    for capture in captures {
        match fs::remove_file(capture) {
            Ok(()) => count += 1,
            // The daily encoder can remove originals while we scan them.
            Err(error) if error.kind() == io::ErrorKind::NotFound => (),
            Err(error) => return Err(error),
        }
    }
    archives.sort();
    Ok((count, archives))
}

fn capture_timestamp(date: Date, time: Time, timezone: &TimeZone) -> io::Result<i64> {
    date.to_datetime(time)
        .to_zoned(timezone.clone())
        .map(|zoned| zoned.timestamp().as_second())
        .map_err(io::Error::other)
}

fn archive_overlaps(
    timestamps: &Path,
    date: Date,
    interval: &RangeInclusive<i64>,
    timezone: &TimeZone,
) -> bool {
    // Missing or malformed timestamps cannot establish that an archive is clear.
    let Ok(contents) = fs::read_to_string(timestamps) else {
        return true;
    };
    if contents.trim().is_empty() {
        return true;
    }
    contents.lines().any(|line| {
        Time::strptime("%H:%M:%S", line)
            .map_err(io::Error::other)
            .and_then(|time| capture_timestamp(date, time, timezone))
            .map(|timestamp| interval.contains(&timestamp))
            .unwrap_or(true)
    })
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

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn parses_minutes_ago() {
        assert_eq!(parse_interval("30", 10_000), Some(8_200..=10_000));
        assert_eq!(parse_interval(" 30 - 10 ", 10_000), Some(8_200..=9_400));
        assert_eq!(parse_interval("1.5–0.5", 10_000), Some(9_910..=9_970));
        assert_eq!(parse_interval("1—0", 10_000), Some(9_940..=10_000));
        for invalid in [
            "", "0", "-5", "5--1", "10-30", "5-5", "NaN", "inf", "1e300", "5-NaN", "5-", "1-0-0",
            "0.001", "200", "minutes",
        ] {
            assert_eq!(parse_interval(invalid, 10_000), None, "{invalid}");
        }
    }

    #[test]
    fn deletes_only_captures_in_interval_across_midnight() {
        let directory =
            std::env::temp_dir().join(format!("penrose-retro-blindfold-{}", std::process::id()));
        let _ = fs::remove_dir_all(&directory);
        let removed = [
            "2026-10-10/23-55-00.jpg",
            "2026-10-10/23-59-30.jpg",
            "2026-10-11/00-00-00.jpg",
            "2026-10-11/00-05-00.jpg",
        ];
        let retained = [
            "2026-10-10/23-54-59.jpg",
            "2026-10-11/00-05-01.jpg",
            "2026-10-11/00-03-00.png",
            "2026-10-11/unrelated.jpg",
            "2026-10-09/23-59-30.jpg",
            "unrelated/00-00-00.jpg",
            "2026-10-10.hevc",
            "2026-10-11.hevc",
            "2026-10-09.hevc",
            "2026-10-10-timestamps",
            "2026-10-11-timestamps",
        ];
        for name in removed.iter().chain(&retained) {
            let path = directory.join(name);
            fs::create_dir_all(path.parent().unwrap()).unwrap();
            fs::write(path, "fixture").unwrap();
        }
        fs::write(directory.join("2026-10-10-timestamps"), "23:55:00\n").unwrap();
        fs::write(directory.join("2026-10-11-timestamps"), "00:05:01\n").unwrap();
        let link = directory.join("2026-10-11/00-01-00.jpg");
        std::os::unix::fs::symlink(directory.join(retained[0]), &link).unwrap();
        std::os::unix::fs::symlink(directory.join("unrelated"), directory.join("2026-10-12"))
            .unwrap();

        // Midnight here is 06:00 UTC; capture names must use the supplied timezone.
        let current: Timestamp = "2026-10-11T06:05:00Z".parse().unwrap();
        let interval = parse_interval("10", current.as_second()).unwrap();
        let timezone = TimeZone::get("America/Denver").unwrap();
        assert_eq!(
            delete_captures(&directory, &interval, &timezone).unwrap(),
            (4, vec!["2026-10-10".to_owned()])
        );
        for name in removed {
            assert!(!directory.join(name).exists(), "{name}");
        }
        for name in retained {
            assert!(directory.join(name).exists(), "{name}");
        }
        assert!(link.is_symlink());

        // A missing or malformed timestamp list still warns for an overlapping day.
        fs::remove_file(directory.join("2026-10-10-timestamps")).unwrap();
        fs::write(directory.join("2026-10-11-timestamps"), "invalid\n").unwrap();
        assert_eq!(
            delete_captures(&directory, &interval, &timezone).unwrap(),
            (0, vec!["2026-10-10".to_owned(), "2026-10-11".to_owned()])
        );
        fs::remove_dir_all(&directory).unwrap();
        assert_eq!(
            delete_captures(&directory, &interval, &timezone).unwrap(),
            (0, Vec::new())
        );
    }
}
