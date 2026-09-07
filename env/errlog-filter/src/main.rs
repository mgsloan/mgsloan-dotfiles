mod audit;
mod journal;
mod rules;

use rules::{Action, Rule};
use std::{
    env, fs,
    io::{self, BufRead, Write},
    path::PathBuf,
};

type Result<T> = std::result::Result<T, Box<dyn std::error::Error>>;

const HELP: &str = "errlog-filter [filter|audit|report] [options]

filter (default)  Classify journalctl short-precise input; flush each visible line.
audit             Incrementally count rules against the local error journal.
report            Show saved counts, last seen, coverage, and stale candidates.

--rules PATH      TOML rules (default: ~/env/errlog-filter/rules.toml, or installed rules)
--state PATH      Audit state (default: $XDG_STATE_HOME/errlog-filter/audit.json)
--since-days N    Initial audit lookback, default 30; only valid without saved state
--stale-days N    Required observed inactivity before suggesting pruning, default 30
--max-entries N   Bound work per audit, default 100000; rerun to continue
--json            Emit the audit/report as JSON
--help            Show this help

Rules are loaded at startup. Audit never deletes rules. Only priorities 0 through 3
are audited, matching journalctl --priority err. See README.md for coverage limits.";

struct Options {
    command: String,
    rules: PathBuf,
    state: PathBuf,
    since_days: Option<u64>,
    stale_days: u64,
    json: bool,
    max_entries: u64,
}

fn home() -> Result<PathBuf> {
    env::var_os("HOME")
        .map(PathBuf::from)
        .ok_or_else(|| "HOME is not set".into())
}

fn default_rules() -> Result<PathBuf> {
    if let Some(path) = env::var_os("ERRLOG_FILTER_RULES") {
        return Ok(path.into());
    }
    let checkout = home()?.join("env/errlog-filter/rules.toml");
    if checkout.exists() {
        return Ok(checkout);
    }
    Ok(env::current_exe()?
        .parent()
        .ok_or("executable has no parent")?
        .join("../share/errlog-filter/rules.toml"))
}

fn options() -> Result<Option<Options>> {
    let arguments: Vec<String> = env::args().skip(1).collect();
    if arguments
        .iter()
        .any(|argument| argument == "--help" || argument == "-h")
    {
        println!("{HELP}");
        return Ok(None);
    }
    let mut options = Options {
        command: "filter".into(),
        rules: default_rules()?,
        state: env::var_os("XDG_STATE_HOME")
            .map(PathBuf::from)
            .unwrap_or(home()?.join(".local/state"))
            .join("errlog-filter/audit.json"),
        since_days: None,
        stale_days: 30,
        json: false,
        max_entries: 100_000,
    };
    let mut arguments = arguments.into_iter().peekable();
    if arguments
        .peek()
        .is_some_and(|argument| !argument.starts_with('-'))
    {
        options.command = arguments.next().unwrap();
    }
    if !["filter", "audit", "report"].contains(&options.command.as_str()) {
        return Err(format!("unknown command: {}", options.command).into());
    }
    while let Some(argument) = arguments.next() {
        if argument == "--json" {
            options.json = true;
            continue;
        }
        let value = arguments
            .next()
            .ok_or_else(|| format!("missing value for {argument}"))?;
        match argument.as_str() {
            "--rules" => options.rules = value.into(),
            "--state" => options.state = value.into(),
            "--since-days" => options.since_days = Some(value.parse()?),
            "--stale-days" => options.stale_days = value.parse()?,
            "--max-entries" => options.max_entries = value.parse()?,
            _ => return Err(format!("unknown option: {argument}").into()),
        }
    }
    if options.since_days.is_some() && options.command != "audit" {
        return Err("--since-days is only valid with audit".into());
    }
    if options.max_entries == 0 {
        return Err("--max-entries must be positive".into());
    }
    if options.stale_days == 0
        || options.stale_days > 36500
        || options.since_days.is_some_and(|days| days > 36500)
    {
        return Err("days must be at most 36500; --stale-days must be positive".into());
    }
    Ok(Some(options))
}

fn split_line<'a>(line: &'a [u8], hostname: &[u8]) -> Option<(&'a [u8], &'a [u8])> {
    let position = line
        .windows(hostname.len())
        .position(|part| part == hostname)?;
    let remainder = &line[position + hostname.len()..];
    let separator = remainder.windows(2).position(|part| part == b": ")?;
    let unit = &remainder[..separator];
    let program = &unit[..unit
        .iter()
        .position(|byte| *byte == b'[')
        .unwrap_or(unit.len())];
    Some((program, &remainder[separator + 2..]))
}

fn filter(
    mut input: impl BufRead,
    mut output: impl Write,
    rules: &[Rule],
    hostname: &[u8],
) -> io::Result<()> {
    let mut line = Vec::new();
    while input.read_until(b'\n', &mut line)? != 0 {
        if line.last() == Some(&b'\n') {
            line.pop();
        }
        let action = split_line(&line, hostname)
            .and_then(|(program, message)| rules::classify(rules, program, message));
        match action {
            Some(Action::Ignore) => {}
            action => {
                output.write_all(if action.is_some() {
                    b"(warn) "
                } else {
                    b"(error) "
                })?;
                output.write_all(&line)?;
                output.write_all(b"\n")?;
                output.flush()?;
            }
        }
        line.clear();
    }
    Ok(())
}

fn run() -> Result<()> {
    let Some(options) = options()? else {
        return Ok(());
    };
    let rules = rules::load(&options.rules)?;
    match options.command.as_str() {
        "filter" => {
            let hostname = format!(
                " {} ",
                fs::read_to_string("/proc/sys/kernel/hostname")?.trim()
            );
            filter(
                io::stdin().lock(),
                io::BufWriter::new(io::stdout().lock()),
                &rules,
                hostname.as_bytes(),
            )?;
        }
        "audit" => audit::run(&options, &rules)?,
        "report" => audit::report(&options, &rules)?,
        _ => unreachable!(),
    }
    Ok(())
}

fn main() {
    if let Err(error) = run() {
        if error
            .downcast_ref::<io::Error>()
            .is_some_and(|error| error.kind() == io::ErrorKind::BrokenPipe)
        {
            return;
        }
        eprintln!("errlog-filter: {error}");
        std::process::exit(1);
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn stream_preserves_bytes_unknown_lines_and_final_line() {
        let rules = vec![Rule {
            id: "test".into(),
            program: "kernel".into(),
            action: Action::Ignore,
            exact: Some("ignore".into()),
            suffix: None,
            note: String::new(),
        }];
        let input = b"Sep 07 01:02:03.456789 host kernel: ignore\nSep 07 01:02:03.456789 host kernel[1]: visible\nmalformed \xff\nlast";
        let mut output = Vec::new();
        filter(&input[..], &mut output, &rules, b" host ").unwrap();
        assert_eq!(output, b"(error) Sep 07 01:02:03.456789 host kernel[1]: visible\n(error) malformed \xff\n(error) last\n");
    }

    #[test]
    fn uncertain_is_visible_and_first_rule_wins() {
        let mut rule = Rule {
            id: "first".into(),
            program: "chrome".into(),
            action: Action::Uncertain,
            exact: None,
            suffix: Some("message".into()),
            note: String::new(),
        };
        let first = rule.clone();
        rule.id = "second".into();
        rule.action = Action::Ignore;
        let mut output = Vec::new();
        filter(
            &b"date host chrome[123]: prefix message\n"[..],
            &mut output,
            &[first, rule],
            b" host ",
        )
        .unwrap();
        assert_eq!(output, b"(warn) date host chrome[123]: prefix message\n");
    }
}
