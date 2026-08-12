//! Spawning children, and getting something back from them.
//!
//! Two helpers, and they cannot be one. `spawn` runs a process under
//! `systemd-cat`, which works by *owning* the child's stdout so it can forward
//! it to the journal — which is the same pipe [read_output] needs to read. So
//! logged spawns give their output to the journal, and captured spawns give it
//! to the caller.
//!
//! Neither may wait for the child. `WindowManager::run` sets `SIGCHLD` to
//! `SIG_IGN`, so the kernel reaps children itself and `waitpid` answers
//! `ECHILD`: `status()`, `output()` and `wait()` all fail with "No child
//! processes" regardless of how the child actually did. Reading a pipe to EOF
//! works anyway, because the pipe closes when the child exits, so that is how
//! everything here observes completion — and [status] recovers an exit code by
//! having the child report its own.

use std::{
    io::{self, Read, Write},
    process::{Child, Command, Stdio},
};

use tracing::{debug, warn};

use crate::env;

/// `--stderr-priority` is from a personal systemd patch, hence the check in
/// `env::check_systemd_cat`; `--level-prefix=false` stops the first characters
/// of a line being eaten as a priority marker.
pub const SYSTEMD_CAT_ARGS: [&str; 2] = ["--level-prefix=false", "--stderr-priority=err"];

/// The line [status] asks the child to print. Long enough not to collide with
/// ordinary output, since it is matched against the child's own stdout.
const RC_MARKER: &str = "__penrose_rc=";

/// Run a command, sending its output to the journal.
///
/// Fire and forget: nothing here learns whether it worked. Use [status] when
/// that matters.
pub fn spawn(cmd: &str, args: &[&str]) -> io::Result<()> {
    debug!(cmd, ?args, "spawning");
    logged_command(cmd, args).stdin(Stdio::null()).spawn()?;

    Ok(())
}

/// Run a command and return its stdout.
///
/// Bypasses `systemd-cat` of necessity — see the module docs — so this
/// process's output is not journalled. Its stderr still goes wherever the
/// window manager's does.
#[allow(dead_code, reason = "used by the amixer and clipboard actions, design.md §15 and §17")]
pub fn read_output(cmd: &str, args: &[&str]) -> io::Result<String> {
    debug!(cmd, ?args, "spawning for output");

    let child = Command::new(cmd)
        .args(args)
        .envs(env::get().overrides())
        .stdin(Stdio::null())
        .stdout(Stdio::piped())
        .spawn()?;

    read_to_eof(child)
}

/// Run a command under the journal and return its exit code.
///
/// The child reports its own status on stdout, because this process cannot ask
/// for it. Everything the command itself writes still goes to the journal, so
/// the only thing on the pipe is the marker line.
pub fn status(cmd: &str, args: &[&str]) -> io::Result<i32> {
    debug!(cmd, ?args, "spawning for exit status");

    // "$0" "$@" passes the command and its arguments through the shell as data
    // rather than as text to be re-parsed, so nothing here needs quoting.
    let script = if env::get().systemd_cat_works {
        format!(
            r#"systemd-cat {} "$0" "$@"; echo "{RC_MARKER}$?""#,
            SYSTEMD_CAT_ARGS.join(" ")
        )
    } else {
        format!(r#""$0" "$@"; echo "{RC_MARKER}$?""#)
    };

    let child = Command::new("sh")
        .arg("-c")
        .arg(script)
        .arg(cmd)
        .args(args)
        .envs(env::get().overrides())
        .stdin(Stdio::null())
        .stdout(Stdio::piped())
        .spawn()?;

    let output = read_to_eof(child)?;

    parse_rc(&output).ok_or_else(|| {
        io::Error::other(format!("no exit status in child output: {output:?}"))
    })
}

/// A command with the journal wrapper applied, when it is available.
fn logged_command(cmd: &str, args: &[&str]) -> Command {
    let mut c = if env::get().systemd_cat_works {
        let mut c = Command::new("systemd-cat");
        c.args(SYSTEMD_CAT_ARGS).arg(cmd).args(args);
        c
    } else {
        let mut c = Command::new(cmd);
        c.args(args);
        c
    };

    c.envs(env::get().overrides());
    c
}

/// Read a child's stdout until it closes, which is what standing in for
/// `wait()` amounts to here.
fn read_to_eof(mut child: Child) -> io::Result<String> {
    let mut stdout = child.stdout.take().expect("stdout to be piped");
    let mut buf = String::new();
    stdout.read_to_string(&mut buf)?;

    Ok(buf)
}

/// Pull the exit code out of what [status]'s shell wrapper printed.
///
/// Scans from the end: the command's own stdout is not on this pipe, but a
/// failure to run `systemd-cat` would put its complaint there.
fn parse_rc(output: &str) -> Option<i32> {
    output
        .lines()
        .rev()
        .find_map(|line| line.trim().strip_prefix(RC_MARKER))
        .and_then(|code| code.parse().ok())
}

/// Run a command, write `input` to its stdin, and return its stdout.
///
/// The shape every menu has: options in, selection out. Reading to EOF is what
/// stands in for waiting, as everywhere else here — `wait_with_output`, the
/// obvious way to write this, cannot work at all under the signal disposition
/// described above.
pub fn read_output_with_input(cmd: &str, args: &[&str], input: &str) -> io::Result<String> {
    debug!(cmd, ?args, "spawning for output, with input");

    let mut child = Command::new(cmd)
        .args(args)
        .envs(env::get().overrides())
        .stdin(Stdio::piped())
        .stdout(Stdio::piped())
        .spawn()?;

    // Dropping stdin closes it, which is what tells the child the input is
    // complete — without that, a filter would wait forever and so would we.
    {
        let mut stdin = child.stdin.take().expect("stdin to be piped");
        stdin.write_all(input.as_bytes())?;
    }

    read_to_eof(child)
}

/// Run a command, writing `input` to its stdin.
///
/// For the handful of programs that take their payload that way — `xclip`
/// above all. Fire and forget, like [spawn]: what matters is that the input
/// arrives, and stdin closing is what tells the child it has.
pub fn spawn_with_input(cmd: &str, args: &[&str], input: &str) -> io::Result<()> {
    debug!(cmd, ?args, "spawning with input");

    let mut child = logged_command(cmd, args)
        .stdin(Stdio::piped())
        .spawn()?;

    let mut stdin = child.stdin.take().expect("stdin to be piped");
    stdin.write_all(input.as_bytes())?;

    Ok(())
}

/// Run a command in a named tmux session inside a terminal of its own.
///
/// The class is what places the window (see `manage.rs`). The command runs
/// inside a shell that outlives it, with the command itself in that shell's
/// history — so when it exits, or is quit, the terminal stays and ↑ re-runs it.
pub fn tmux_terminal(name: &str, cmd: &str) -> io::Result<()> {
    // Killing first is what makes this idempotent, so the M-x entries that
    // re-run parts of startup replace their terminals rather than stacking up.
    if let Err(e) = spawn("tmux", &["kill-session", "-t", name]) {
        warn!(%e, name, "unable to kill existing tmux session");
    }

    let shell = interactive_shell_command(cmd);

    spawn(
        crate::TERMINAL,
        &[
            "--class", name, "-e", "tmux", "new-session", "-s", name, "-n", name, &shell,
        ],
    )
}

/// A bash invocation that runs `cmd` and then keeps the shell.
fn interactive_shell_command(cmd: &str) -> String {
    let with_history = format!("history -s {}; {cmd}", shell_quote(cmd));

    format!("bash --init-file <(echo {})", shell_quote(&with_history))
}

/// Quote a string as a single POSIX shell word.
///
/// Single quotes make everything literal, so the only case to handle is a
/// single quote itself: end the string, emit an escaped quote, start again.
fn shell_quote(s: &str) -> String {
    format!("'{}'", s.replace('\'', r"'\''"))
}

#[cfg(test)]
mod tests {
    use super::*;

    /// `status` and `read_output` both consult `Env`, and every test here may
    /// be the first to run.
    fn env() {
        env::init();
    }

    #[test]
    fn status_reports_what_the_child_exited_with() {
        env();

        assert_eq!(status("true", &[]).unwrap(), 0);
        assert_eq!(status("false", &[]).unwrap(), 1);
        assert_eq!(status("sh", &["-c", "exit 7"]).unwrap(), 7);
    }

    #[test]
    fn read_output_returns_stdout() {
        env();

        assert_eq!(read_output("echo", &["hello"]).unwrap(), "hello\n");
    }

    /// The shape `menu.rs` depends on: options in, selection out. `head -1`
    /// stands in for rofi, since it also reads until stdin closes.
    #[test]
    fn read_output_with_input_round_trips() {
        env();

        let selection = read_output_with_input("head", &["-1"], "first\nsecond\n").unwrap();
        assert_eq!(selection.trim(), "first");

        // A filter that consumes everything only terminates if stdin is closed.
        let all = read_output_with_input("cat", &[], "a\nb\nc").unwrap();
        assert_eq!(all, "a\nb\nc");
    }

    /// The condition this module exists for, checked directly.
    ///
    /// Ignored by default because the disposition is process-wide and would
    /// break any test that waits for a child — including penrose's own
    /// `xmodmap` call in `bindings.rs`. Run it alone:
    ///
    /// ```text
    /// cargo test -- --ignored sigchld
    /// ```
    #[test]
    #[ignore = "sets a process-wide signal disposition"]
    fn status_survives_sigchld_being_ignored() {
        unsafe extern "C" {
            fn signal(signum: i32, handler: usize) -> usize;
        }
        const SIGCHLD: i32 = 17;
        const SIG_IGN: usize = 1;

        env();

        // What WindowManager::run does, and what makes waiting impossible.
        unsafe { signal(SIGCHLD, SIG_IGN) };
        #[allow(clippy::disallowed_methods, reason = "asserting that it does not work")]
        let waited = std::process::Command::new("true").status();

        assert!(
            waited.is_err(),
            "if this passes, the premise of this module no longer holds"
        );

        assert_eq!(status("true", &[]).unwrap(), 0);
        assert_eq!(status("sh", &["-c", "exit 3"]).unwrap(), 3);
        assert_eq!(read_output("echo", &["still works"]).unwrap(), "still works\n");

        // The menu path (`menu.rs`), which used `wait_with_output` and so
        // discarded every selection under this disposition.
        let selection = read_output_with_input("head", &["-1"], "logout\ntops\n").unwrap();
        assert_eq!(selection.trim(), "logout");
    }

    #[test]
    fn rc_is_read_from_the_marker_line() {
        assert_eq!(parse_rc(&format!("{RC_MARKER}0\n")), Some(0));
        assert_eq!(parse_rc(&format!("{RC_MARKER}101\n")), Some(101));
    }

    #[test]
    fn rc_ignores_noise_before_the_marker() {
        let output = format!("systemd-cat: unrecognized option\n{RC_MARKER}2\n");
        assert_eq!(parse_rc(&output), Some(2));
    }

    #[test]
    fn missing_rc_is_not_a_success() {
        assert_eq!(parse_rc(""), None);
        assert_eq!(parse_rc("something went very wrong\n"), None);
    }

    #[test]
    fn quoting_survives_quotes() {
        assert_eq!(shell_quote("simple"), "'simple'");
        assert_eq!(shell_quote("it's"), r"'it'\''s'");
        assert_eq!(shell_quote("a; rm -rf /"), "'a; rm -rf /'");
    }

    #[test]
    fn interactive_shell_command_keeps_the_command_intact() {
        let built = interactive_shell_command("nmtui connect");

        assert!(built.starts_with("bash --init-file <(echo '"));
        assert!(built.contains("history -s "), "command should reach the history");
        assert!(built.ends_with(')'));
    }
}
