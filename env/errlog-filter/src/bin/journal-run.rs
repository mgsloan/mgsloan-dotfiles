//! Forward both output streams to the journal, interpreting explicit levels.

use std::{
    env,
    ffi::OsString,
    io::{self, BufRead, BufReader, Read, Write},
    os::unix::process::ExitStatusExt,
    path::Path,
    process::{Command, Stdio},
    sync::atomic::{AtomicI32, Ordering},
    thread,
};

unsafe extern "C" {
    fn sd_journal_sendv(fields: *const libc::iovec, count: libc::c_int) -> libc::c_int;
}

static CHILD: AtomicI32 = AtomicI32::new(0);

extern "C" fn forward_signal(signal: libc::c_int) {
    let child = CHILD.load(Ordering::Relaxed);
    if child > 0 {
        // kill is async-signal-safe; only the child receives the signal.
        unsafe { libc::kill(child, signal) };
    }
}

fn classify(line: &[u8], default: u8) -> (u8, &[u8]) {
    let (text, bracketed) = match line.strip_prefix(b"[") {
        Some(text) => (text, true),
        None => (line, false),
    };
    for (name, priority) in [(b"info".as_slice(), 6), (b"warning", 4), (b"warn", 4)] {
        if text.len() < name.len() || !text[..name.len()].eq_ignore_ascii_case(name) {
            continue;
        }
        let mut remainder = &text[name.len()..];
        if bracketed {
            let Some(rest) = remainder.strip_prefix(b"]") else {
                continue;
            };
            remainder = rest;
        } else if !remainder.is_empty() && !matches!(remainder[0], b' ' | b'\t' | b':' | b'(') {
            continue;
        }
        if remainder.starts_with(b"(") && !remainder.windows(2).any(|part| part == b"):") {
            continue;
        }
        remainder = remainder.strip_prefix(b":").unwrap_or(remainder);
        while remainder.first().is_some_and(u8::is_ascii_whitespace) {
            remainder = &remainder[1..];
        }
        return (priority, remainder);
    }
    (default, line)
}

fn send(
    identifier: &[u8],
    process: &[u8],
    priority: u8,
    line: &[u8],
    message: &mut Vec<u8>,
) -> io::Result<()> {
    message.clear();
    message.extend_from_slice(b"MESSAGE=");
    message.extend_from_slice(line);
    let priority = [
        b'P',
        b'R',
        b'I',
        b'O',
        b'R',
        b'I',
        b'T',
        b'Y',
        b'=',
        b'0' + priority,
    ];
    let fields = [identifier, process, &priority, message.as_slice()].map(|field| libc::iovec {
        iov_base: field.as_ptr().cast_mut().cast(),
        iov_len: field.len(),
    });
    // All field buffers remain live for the synchronous libsystemd call.
    let result = unsafe { sd_journal_sendv(fields.as_ptr(), fields.len() as libc::c_int) };
    if result < 0 {
        Err(io::Error::from_raw_os_error(-result))
    } else {
        Ok(())
    }
}

fn forward(input: impl Read, identifier: &[u8], process: &[u8], default: u8) -> io::Result<()> {
    const MAX_LINE: u64 = 64 * 1024;
    let mut input = BufReader::new(input);
    let mut line = Vec::new();
    let mut message = Vec::new();
    let mut continuation = false;
    let mut priority = default;
    let mut reported = false;
    loop {
        line.clear();
        if input.by_ref().take(MAX_LINE).read_until(b'\n', &mut line)? == 0 {
            return Ok(());
        }
        let newline = line.last() == Some(&b'\n');
        if newline {
            line.pop();
        }
        let text = if continuation {
            line.as_slice()
        } else {
            let classified = classify(&line, default);
            priority = classified.0;
            classified.1
        };
        if let Err(error) = send(identifier, process, priority, text, &mut message) {
            if !reported {
                eprintln!("journal-run: journal unavailable: {error}; forwarding to stderr");
                reported = true;
            }
            let mut fallback = io::stderr().lock();
            fallback.write_all(&line)?;
            if newline {
                fallback.write_all(b"\n")?;
            }
        }
        // Bound memory for unterminated output; retain its level across chunks.
        continuation = !newline;
    }
}

fn run() -> io::Result<i32> {
    let mut arguments = env::args_os().skip(1);
    let mut identifier = None;
    let command = loop {
        match arguments.next() {
            Some(argument) if argument == "--help" => {
                println!("journal-run [--identifier NAME] -- COMMAND [ARGUMENTS...]\n\nForward stdout (info) and stderr (error) to the journal. Explicit info/warn/warning\nprefixes override the priority and are removed. Scope labels are retained.");
                return Ok(0);
            }
            Some(argument) if argument == "--identifier" => {
                identifier = Some(arguments.next().ok_or_else(|| io::Error::other("missing identifier"))?);
            }
            Some(argument) if argument == "--" => break arguments.next(),
            command => break command,
        }
    }.ok_or_else(|| io::Error::other("missing command"))?;
    let identifier = identifier.unwrap_or_else(|| {
        Path::new(&command)
            .file_name()
            .unwrap_or(&command)
            .to_owned()
    });
    let identifier = [
        b"SYSLOG_IDENTIFIER=".as_slice(),
        identifier.as_encoded_bytes(),
    ]
    .concat();
    // Penrose ignores SIGCHLD. Restore it before spawning so wait returns status.
    unsafe { libc::signal(libc::SIGCHLD, libc::SIG_DFL) };
    let mut child = Command::new(&command)
        .args(arguments.collect::<Vec<OsString>>())
        .stdout(Stdio::piped())
        .stderr(Stdio::piped())
        .spawn()?;
    CHILD.store(child.id() as i32, Ordering::Relaxed);
    for signal in [libc::SIGTERM, libc::SIGINT, libc::SIGHUP] {
        unsafe { libc::signal(signal, forward_signal as *const () as usize) };
    }
    let process = format!("SYSLOG_PID={}", child.id()).into_bytes();
    let stdout = child.stdout.take().unwrap();
    let stderr = child.stderr.take().unwrap();
    thread::scope(|scope| {
        let stdout = scope.spawn(|| forward(stdout, &identifier, &process, 6));
        let stderr = scope.spawn(|| forward(stderr, &identifier, &process, 3));
        let status = child.wait()?;
        CHILD.store(0, Ordering::Relaxed);
        stdout
            .join()
            .map_err(|_| io::Error::other("stdout reader panicked"))??;
        stderr
            .join()
            .map_err(|_| io::Error::other("stderr reader panicked"))??;
        Ok(status.code().unwrap_or(128 + status.signal().unwrap_or(0)))
    })
}

fn main() {
    match run() {
        Ok(code) => std::process::exit(code),
        Err(error) => {
            eprintln!("journal-run: {error}");
            std::process::exit(1);
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn levels_are_removed_and_scopes_preserved() {
        for (input, priority, output) in [
            (b"info ready".as_slice(), 6, b"ready".as_slice()),
            (b"INFO: ready", 6, b"ready"),
            (b"[info] ready", 6, b"ready"),
            (b"warning(renderer): fallback", 4, b"(renderer): fallback"),
            (b"Warning: fallback", 4, b"fallback"),
            (b"warn\tfallback", 4, b"fallback"),
            (b"info: bad byte \xff", 6, b"bad byte \xff"),
        ] {
            assert_eq!(classify(input, 3), (priority, output));
        }
    }

    #[test]
    fn only_explicit_prefixes_override_the_stream_default() {
        for input in [
            b"information".as_slice(),
            b"warning_count=2",
            b"[info broken",
            b"info(unclosed",
            b"contains info: text",
            b"<3>literal",
        ] {
            for default in [3, 6] {
                assert_eq!(classify(input, default), (default, input));
            }
        }
    }
}
