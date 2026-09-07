# errlog-filter

A byte-oriented Rust replacement for the original Haskell filter. The live
pipeline is:

```sh
journalctl --output short-iso-precise --follow --priority err --boot | errlog-filter --notify | ccze -A
```

Unknown messages retain the `(error)` prefix. `uncertain` rules produce `(warn)`;
`ignore` rules suppress the line. The first matching rule wins. Unrecognized
lines and invalid UTF-8 pass through as unknown errors. Each visible line is
flushed immediately. Without `--notify`, the live process does no statistics I/O.

## Error rate alerts

`--notify` alerts when at least 30 unknown `(error)` messages occur in a rolling
minute; `--error-rate N` changes that threshold. Ignored and uncertain messages
do not count. Alerts repeat on incoming errors while the rate remains elevated,
at most once every five minutes, including across filter restarts. Quiet periods
expire the window; they produce no notifications.

Alerting requires `journalctl --output short-iso-precise` timestamps. Entries from
before the filter started, entries older than a minute, future timestamps, and
unparseable lines remain visible but do not count toward alerts. This prevents
startup history from triggering an alert. Timestamps measure journal entries,
not the number of physical lines in a multiline message.

`M-x earplugs` (alias `ignore-high-error-frequency`) asks for minutes to mute
these alerts. Fractional minutes work; `0` enables them immediately, subject to
the five-minute cooldown. Repeating the command replaces the deadline. Filtering
and counting continue while muted. The deadline expires automatically even if
Penrose or the filter restarts; there is no separate resumption notification.

The monitor retains at most N timestamps, has no idle polling or monitoring
thread, and checks mute/cooldown files at most once per second while elevated
and eligible to notify. Only notifications spawn `notify-send` and a short-lived
child-reaping thread. The public Nix package supplies `notify-send`.

State lives under `$XDG_STATE_HOME` (default `~/.local/state`):
`errlog-filter/last-alert` shares the notification cooldown between processes;
`penrose/error-alerts-inhibit-until` holds the mute expiry in Unix seconds.

## Rules

Edit `~/env/errlog-filter/rules.toml`, then restart the filter. Rules are read once
at startup. `--rules PATH` overrides `ERRLOG_FILTER_RULES`, which overrides the
checkout path. Without the checkout, the installed `share/errlog-filter/rules.toml`
is used. Invalid configuration stops the command with an error.

```toml
[[rules]]
id = "example"
program = "kernel"
action = "ignore" # or "uncertain"
exact = "An investigated message" # alternatively: suffix = "message ending"
note = "Why this message is considered harmless, with a reference if available."
```

IDs must be unique and contain only ASCII letters, digits, hyphens, or underscores.
Each rule requires exactly one nonempty `exact` or `suffix` matcher. Matching is
case-sensitive and constrained to the program name, with a PID removed in the
live stream. The ten original rules and their classifications are preserved.

## Audit and pruning

```sh
errlog-filter audit                 # initially look back 30 days
errlog-filter audit                 # resume; repeat if the report says incomplete
errlog-filter report --stale-days 60
errlog-filter report --json
```

The audit reads the local journal directly through libsystemd. It traverses
entries so its cursor advances even during periods without errors, but reads
messages only for priorities 0–3 from programs with rules. It does not export or
retain the log text. All matching rules receive a hit, including overlaps and
`uncertain` rules. Reports include zero-hit rules, last-seen timestamps, and the
start of usable observation for each rule.

A run processes at most 100,000 journal entries by default. Adjust with
`--max-entries N`. State is checkpointed every 10,000 entries and on normal exit;
an interrupted run resumes from the last checkpoint without double-counting.
The state file and cursor are saved together using an atomic rename. A file lock
prevents concurrent audits from overwriting each other.

State defaults to `$XDG_STATE_HOME/errlog-filter/audit.json`, or
`~/.local/state/errlog-filter/audit.json`. Use `--state PATH` for independent rule
sets, machines, or backfills. A new state accepts `--since-days N`; use `0` to
start observing now, or a larger value to inspect retained history. Existing
state rejects `--since-days` to avoid silently mixing overlapping scans.

`stale` means there has been no match for the requested number of observed days,
measured at the last completed audit. It is a candidate for manual removal from
`rules.toml`, not proof that the message can never recur. Incomplete scans never
suggest pruning. `report` reads saved results without scanning or advancing time.

Changing a rule's program or matcher resets its count and observation start on
the next audit. Editing its note or action preserves statistics. Added rules
start at the saved audit boundary; they are not automatically applied to older
history. To backfill them, use a separate state file. Removed rules disappear
from state on the next audit.

If the saved cursor is unavailable after rotation, deletion, or changed access,
the audit records a coverage gap, resumes after its saved time, and restarts the
inactivity observation period. Backward clock movement also restarts that
period. Counts and last-seen values survive gaps, but counts may then be partial.
Machine identity and access credentials must match the saved state.

Coverage is limited to the default local journal namespace readable by the
current user. Retention before the first audit, missing files elsewhere in the
history, permission changes that leave the cursor readable, and logs never
written to the journal cannot be proved complete by a cursor check. Review
coverage and the rule's purpose before pruning, especially for rare boot errors.

## Build

Rust 1.89+, Cargo, pkg-config, and libsystemd development files are required.
Development dependencies are declared by this package's public Nix derivation.

```sh
cargo test --manifest-path errlog-filter/Cargo.toml
cargo clippy --manifest-path errlog-filter/Cargo.toml --all-targets -- -D warnings
cargo run --release --manifest-path errlog-filter/Cargo.toml -- --help
env-nix build errlog-filter --working-tree
```

The Nix package is included in `source-tools`, `environment`, and flake checks.
Before new source files have been human-staged, `env-nix` requires explicit
`--include errlog-filter/PATH` arguments for the new Cargo files, `build.rs`,
`rules.toml`, and the Rust sources. Its allowlist excludes audit state and build
outputs. No Haskell toolchain is needed for this package.

## Process logging

The package also installs `journal-run [--identifier NAME] -- COMMAND [ARGS...]`,
used by Penrose's logged process helpers. Stdout defaults to priority 6 (info),
stderr to 3 (error). Leading `info`, `warn`, or `warning` (case-insensitive) with
space, colon, brackets, or a `(scope):` sets the appropriate priority and removes
the level word. A scope is retained: `warning(renderer): fallback` becomes
`(renderer): fallback` at priority 4. Unrecognized prefixes remain unchanged.

Both streams reach the native journal, including final unterminated lines and
non-UTF-8 bytes. Lines exceeding 64 KiB are split into bounded chunks with the
same priority. Journal write failures fall back to stderr. Each logged process
has a small forwarding parent with two blocking reader threads and no polling;
it stays until the command exits and both streams close. Programs that detach
should close or redirect inherited output streams.

`SYSLOG_IDENTIFIER` defaults to the command basename; `SYSLOG_PID` identifies the
child. The trusted `_PID` is the forwarding parent, which remains an ancestor
of the application for Penrose's `show-logs`. Existing applications keep their
original logging until restarted. Captured-output helpers still return stdout
to their caller instead of journalling it.
