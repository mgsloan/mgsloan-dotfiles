use serde::{Deserialize, Serialize};
use std::{
    collections::BTreeMap,
    fs::{self, File, OpenOptions},
    io::{self, Write},
    os::unix::fs::OpenOptionsExt,
    path::Path,
};

use crate::{Options, Result, journal::Journal, rules::Rule};

const DAY: u64 = 86_400_000_000;

#[derive(Deserialize, Serialize)]
struct Statistics {
    rule: Rule,
    since: u64,
    count: u64,
    last_seen: Option<u64>,
}

#[derive(Deserialize, Serialize)]
struct Gap {
    detected_at: u64,
    reason: String,
}

#[derive(Deserialize, Serialize)]
struct State {
    version: u32,
    scope: String,
    cursor: Option<String>,
    until: u64,
    coverage_since: u64,
    complete: bool,
    gaps: Vec<Gap>,
    rules: BTreeMap<String, Statistics>,
}

impl State {
    fn new(scope: String, since: u64) -> Self {
        Self {
            version: 1,
            scope,
            cursor: None,
            until: since,
            coverage_since: since,
            complete: false,
            gaps: Vec::new(),
            rules: BTreeMap::new(),
        }
    }

    fn reconcile(&mut self, rules: &[Rule]) {
        self.rules
            .retain(|identifier, _| rules.iter().any(|rule| &rule.id == identifier));
        for rule in rules {
            match self.rules.get_mut(&rule.id) {
                Some(statistics) if statistics.rule.same_matcher(rule) => {
                    statistics.rule = rule.clone()
                }
                _ => {
                    self.rules.insert(
                        rule.id.clone(),
                        Statistics {
                            rule: rule.clone(),
                            since: self.until,
                            count: 0,
                            last_seen: None,
                        },
                    );
                }
            }
        }
    }

    fn gap(&mut self, now: u64, reason: &str) {
        self.coverage_since = now;
        self.gaps.push(Gap {
            detected_at: now,
            reason: reason.into(),
        });
        eprintln!("errlog-filter: coverage gap: {reason}; inactivity observation restarts now");
    }

    fn observe(&mut self, program: &[u8], message: &[u8], timestamp: u64) {
        for statistics in self.rules.values_mut() {
            if statistics.rule.matches(program, message) {
                statistics.count += 1;
                statistics.last_seen = Some(statistics.last_seen.unwrap_or(0).max(timestamp));
            }
        }
    }

    fn stale(&self, statistics: &Statistics, days: u64) -> bool {
        let since = statistics
            .since
            .max(self.coverage_since)
            .max(statistics.last_seen.unwrap_or(0));
        self.complete && self.until.saturating_sub(since) >= days * DAY
    }
}

fn now() -> u64 {
    jiff::Timestamp::now().as_microsecond().max(0) as u64
}

fn scope() -> Result<String> {
    let status = fs::read_to_string("/proc/self/status")?;
    let credentials = status
        .lines()
        .filter(|line| {
            line.starts_with("Uid:") || line.starts_with("Gid:") || line.starts_with("Groups:")
        })
        .collect::<Vec<_>>()
        .join("\n");
    Ok(format!(
        "{}\n{credentials}",
        fs::read_to_string("/etc/machine-id")?.trim()
    ))
}

fn read(path: &Path) -> Result<Option<State>> {
    let contents = match fs::read(path) {
        Ok(contents) => contents,
        Err(error) if error.kind() == io::ErrorKind::NotFound => return Ok(None),
        Err(error) => return Err(error.into()),
    };
    let state: State = serde_json::from_slice(&contents)?;
    if state.version != 1 {
        return Err(format!("unsupported audit state version: {}", state.version).into());
    }
    Ok(Some(state))
}

fn parent(path: &Path) -> &Path {
    path.parent()
        .filter(|parent| !parent.as_os_str().is_empty())
        .unwrap_or(Path::new("."))
}

fn save(path: &Path, state: &State) -> Result<()> {
    let temporary = path.with_extension(format!("tmp.{}", std::process::id()));
    let mut file = OpenOptions::new()
        .write(true)
        .create_new(true)
        .mode(0o600)
        .open(&temporary)?;
    let result = (|| -> Result<()> {
        serde_json::to_writer_pretty(&mut file, state)?;
        file.write_all(b"\n")?;
        file.sync_all()?;
        fs::rename(&temporary, path)?;
        File::open(parent(path))?.sync_all()?;
        Ok(())
    })();
    if temporary.exists() {
        let _ = fs::remove_file(temporary);
    }
    result
}

pub fn run(options: &Options, rules: &[Rule]) -> Result<()> {
    fs::create_dir_all(parent(&options.state))?;
    let lock = OpenOptions::new()
        .read(true)
        .write(true)
        .create(true)
        .truncate(false)
        .mode(0o600)
        .open(options.state.with_extension("lock"))?;
    lock.try_lock()
        .map_err(|error| format!("another audit may be running: {error}"))?;
    let started = now();
    let mut journal = Journal::open()?;
    if !journal.head()? {
        return Err("no readable journal entries; check journal permissions".into());
    }
    let oldest = journal.timestamp()?;
    let saved = read(&options.state)?;
    if saved.is_some() && options.since_days.is_some() {
        return Err(
            "--since-days only applies to a new state; choose another --state path to backfill"
                .into(),
        );
    }
    let current_scope = scope()?;
    let mut state = saved.unwrap_or_else(|| {
        State::new(
            current_scope.clone(),
            started
                .saturating_sub(options.since_days.unwrap_or(30) * DAY)
                .max(oldest),
        )
    });
    if state.scope != current_scope {
        return Err(
            "machine or journal access credentials changed; use a separate --state path".into(),
        );
    }
    state.reconcile(rules);
    state.complete = false;
    if let Some(cursor) = state.cursor.clone() {
        journal.seek_cursor(&cursor)?;
        if !journal.next()? || !journal.at_cursor(&cursor)? {
            state.gap(
                started,
                "saved cursor is no longer available (rotation, deletion, or changed access)",
            );
            state.cursor = None;
            journal.seek_time(state.until.saturating_add(1))?;
        }
    } else {
        journal.seek_time(state.until)?;
    }
    let mut processed = 0;
    let mut errors = 0;
    let mut previous_timestamp = state.until;
    while processed < options.max_entries {
        if !journal.next()? {
            state.complete = true;
            break;
        }
        let timestamp = journal.timestamp()?;
        if timestamp < previous_timestamp && state.coverage_since != started {
            state.gap(started, "journal clock moved backwards");
        }
        previous_timestamp = timestamp;
        if journal
            .field(c"PRIORITY")?
            .is_some_and(|priority| matches!(priority.as_slice(), b"0" | b"1" | b"2" | b"3"))
        {
            let program = match journal.field(c"SYSLOG_IDENTIFIER")? {
                Some(program) => program,
                None => journal.field(c"_COMM")?.unwrap_or_default(),
            };
            // Most errors are from programs with no rules; avoid reading their messages.
            if rules.iter().any(|rule| rule.program.as_bytes() == program) {
                let message = journal.field(c"MESSAGE")?.unwrap_or_default();
                state.observe(&program, &message, timestamp);
            }
            errors += 1;
        }
        state.until = state.until.max(timestamp);
        processed += 1;
        if processed % 10_000 == 0 {
            state.cursor = Some(journal.cursor()?);
            save(&options.state, &state)?;
            eprintln!("errlog-filter: checkpoint: {processed} entries, {errors} errors");
        }
    }
    // At EOF libsystemd retains the last current entry. Without any entries,
    // retain the previous cursor rather than trying to read an unpositioned one.
    if processed > 0 {
        state.cursor = Some(journal.cursor()?);
    }
    if state.complete {
        let finished = now();
        if finished < state.until {
            state.gap(finished, "wall clock is behind the saved observation time");
        }
        state.until = finished;
    }
    save(&options.state, &state)?;
    eprintln!(
        "errlog-filter: processed {processed} entries ({errors} errors); {}",
        if state.complete {
            "caught up"
        } else {
            "limit reached; rerun audit to continue"
        }
    );
    display(options, rules, &state)
}

pub fn report(options: &Options, rules: &[Rule]) -> Result<()> {
    let state = read(&options.state)?.ok_or("no audit state; run errlog-filter audit first")?;
    display(options, rules, &state)
}

fn timestamp(value: u64) -> String {
    i64::try_from(value)
        .ok()
        .and_then(|value| jiff::Timestamp::from_microsecond(value).ok())
        .map(|value| value.to_string())
        .unwrap_or_else(|| value.to_string())
}

fn display(options: &Options, rules: &[Rule], state: &State) -> Result<()> {
    #[derive(Serialize)]
    struct Row<'a> {
        id: &'a str,
        count: u64,
        last_seen: Option<String>,
        observed_since: String,
        status: &'static str,
    }
    let rows: Vec<_> = rules
        .iter()
        .map(|rule| {
            let statistics = state
                .rules
                .get(&rule.id)
                .filter(|statistics| statistics.rule.same_matcher(rule));
            Row {
                id: &rule.id,
                count: statistics.map_or(0, |statistics| statistics.count),
                last_seen: statistics
                    .and_then(|statistics| statistics.last_seen)
                    .map(timestamp),
                observed_since: timestamp(
                    statistics
                        .map_or(state.until, |statistics| statistics.since)
                        .max(state.coverage_since),
                ),
                status: match statistics {
                    None => "unaudited",
                    Some(_) if !state.complete => "incomplete",
                    Some(statistics) if state.stale(statistics, options.stale_days) => "stale",
                    Some(statistics) if statistics.count == 0 => "observing",
                    Some(_) => "seen",
                },
            }
        })
        .collect();
    let mut output = io::stdout().lock();
    if options.json {
        serde_json::to_writer_pretty(
            &mut output,
            &serde_json::json!({
                "as_of": timestamp(state.until), "complete": state.complete,
                "coverage_since": timestamp(state.coverage_since), "gaps": state.gaps,
                "stale_days": options.stale_days, "rules": rows,
            }),
        )?;
        writeln!(output)?;
    } else {
        writeln!(
            output,
            "As of {} | coverage since {} | {} gap(s) | {}",
            timestamp(state.until),
            timestamp(state.coverage_since),
            state.gaps.len(),
            if state.complete {
                "caught up"
            } else {
                "incomplete; rerun audit"
            }
        )?;
        writeln!(
            output,
            "{:<28} {:>10}  {:<27}  {:<27}  STATUS",
            "RULE", "COUNT", "LAST SEEN (UTC)", "OBSERVED SINCE (UTC)"
        )?;
        for row in rows {
            writeln!(
                output,
                "{:<28} {:>10}  {:<27}  {:<27}  {}",
                row.id,
                row.count,
                row.last_seen.as_deref().unwrap_or("never"),
                row.observed_since,
                row.status
            )?;
        }
        writeln!(
            output,
            "Stale = at least {} observed days without a match. Review rules.toml before removing a rule.",
            options.stale_days
        )?;
    }
    Ok(())
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::rules::Action;

    fn rule(identifier: &str) -> Rule {
        Rule {
            id: identifier.into(),
            program: "kernel".into(),
            action: Action::Ignore,
            exact: None,
            suffix: Some("message".into()),
            note: String::new(),
        }
    }

    #[test]
    fn overlapping_rules_count_independently_and_zero_hits_are_retained() {
        let mut state = State::new("test".into(), DAY);
        let mut absent = rule("absent");
        absent.program = "other".into();
        state.reconcile(&[rule("one"), rule("two"), absent]);
        state.observe(b"kernel", b"prefix message", 2 * DAY);
        assert_eq!(state.rules["one"].count, 1);
        assert_eq!(state.rules["two"].count, 1);
        assert_eq!(state.rules["absent"].count, 0);
        assert_eq!(state.rules["one"].last_seen, Some(2 * DAY));
    }

    #[test]
    fn matcher_edits_reset_counts_but_notes_and_actions_do_not() {
        let mut state = State::new("test".into(), DAY);
        let mut rule = rule("one");
        state.reconcile(&[rule.clone()]);
        state.observe(b"kernel", b"message", DAY);
        state.until = 3 * DAY;
        rule.note = "reviewed".into();
        rule.action = Action::Uncertain;
        state.reconcile(&[rule.clone()]);
        assert_eq!(state.rules["one"].count, 1);
        rule.suffix = Some("different".into());
        state.reconcile(&[rule]);
        assert_eq!(state.rules["one"].count, 0);
        assert_eq!(state.rules["one"].since, 3 * DAY);
        state.reconcile(&[]);
        assert!(state.rules.is_empty());
    }

    #[test]
    fn stale_requires_completed_coverage_after_last_hit_rule_creation_and_gap() {
        let mut state = State::new("test".into(), DAY);
        state.reconcile(&[rule("one")]);
        state.until = 40 * DAY;
        assert!(!state.stale(&state.rules["one"], 30));
        state.complete = true;
        assert!(state.stale(&state.rules["one"], 30));
        state.gap(40 * DAY, "test rotation");
        assert!(!state.stale(&state.rules["one"], 30));
        state.until = 70 * DAY;
        assert!(state.stale(&state.rules["one"], 30));
        state.observe(b"kernel", b"message", 69 * DAY);
        assert!(!state.stale(&state.rules["one"], 30));
        state.reconcile(&[rule("one"), rule("new")]);
        assert!(!state.stale(&state.rules["new"], 30));
    }

    #[test]
    fn state_round_trip_preserves_resume_position_and_counts() {
        let mut state = State::new("test".into(), DAY);
        state.cursor = Some("opaque cursor".into());
        state.reconcile(&[rule("one")]);
        state.observe(b"kernel", b"message", 2 * DAY);
        let restored: State = serde_json::from_slice(&serde_json::to_vec(&state).unwrap()).unwrap();
        assert_eq!(restored.cursor, state.cursor);
        assert_eq!(restored.rules["one"].count, 1);
        assert_eq!(restored.rules["one"].last_seen, Some(2 * DAY));
    }
}
