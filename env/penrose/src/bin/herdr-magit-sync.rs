//! Herdr focus events -> Magit, gated by Penrose's displayed workspaces.
//!
//! This is a separate process, started before the WM ignores SIGCHLD. Blocking socket
//! reads, subprocess waits and reconnect delays belong here rather than in the WM.
#![allow(clippy::disallowed_methods)]

use std::{
    fs,
    io::{self, BufRead, BufReader, Write},
    os::unix::{fs::OpenOptionsExt, net::UnixStream},
    path::{Path, PathBuf},
    process::{Child, Command, Stdio},
    sync::mpsc::{self, Sender},
    thread,
    time::{Duration, Instant},
};

use serde_json::{Value, json};

const DEBOUNCE: Duration = Duration::from_millis(75);
const TICK: Duration = Duration::from_millis(50);

#[derive(Debug)]
enum Event {
    Focus(String),
    Reset,
}

fn socket_path() -> io::Result<PathBuf> {
    if let Some(path) = std::env::var_os("HERDR_SOCKET_PATH") {
        return Ok(path.into());
    }
    let config = match std::env::var_os("XDG_CONFIG_HOME") {
        Some(path) => PathBuf::from(path),
        None => PathBuf::from(
            std::env::var_os("HOME").ok_or_else(|| io::Error::other("HOME is unset"))?,
        )
        .join(".config"),
    };
    Ok(config.join("herdr/herdr.sock"))
}

fn send(stream: &mut UnixStream, method: &str, params: Value) -> io::Result<()> {
    serde_json::to_writer(
        &mut *stream,
        &json!({"id": "magit", "method": method, "params": params}),
    )?;
    stream.write_all(b"\n")
}

fn listen(path: &Path, events: &Sender<Event>) -> io::Result<()> {
    let mut stream = UnixStream::connect(path)?;
    stream.set_write_timeout(Some(Duration::from_secs(1)))?;
    send(
        &mut stream,
        "events.subscribe",
        json!({"subscriptions": [{"type": "pane.focused"}]}),
    )?;
    for line in BufReader::new(stream).lines() {
        let message: Value = serde_json::from_str(&line?)?;
        if let Some(error) = message.get("error") {
            return Err(io::Error::other(error.to_string()));
        }
        if message["event"] == "pane.focused"
            && let Some(pane) = message["data"]["pane_id"].as_str()
        {
            events
                .send(Event::Focus(pane.to_owned()))
                .map_err(io::Error::other)?;
        }
    }
    Err(io::Error::new(
        io::ErrorKind::UnexpectedEof,
        "Herdr disconnected",
    ))
}

fn current_pane(path: &Path) -> io::Result<Value> {
    let mut stream = UnixStream::connect(path)?;
    stream.set_read_timeout(Some(Duration::from_secs(1)))?;
    stream.set_write_timeout(Some(Duration::from_secs(1)))?;
    send(&mut stream, "pane.current", json!({}))?;
    let mut line = String::new();
    BufReader::new(stream).read_line(&mut line)?;
    let response: Value = serde_json::from_str(&line)?;
    if response["result"]["type"] != "pane_current" {
        return Err(io::Error::other(format!(
            "unexpected pane response: {response}"
        )));
    }
    Ok(response["result"]["pane"].clone())
}

fn directory(pane: &Value, expected: &str) -> Option<String> {
    if pane["pane_id"].as_str() != Some(expected) {
        return None;
    }
    ["foreground_cwd", "cwd"]
        .into_iter()
        .filter_map(|field| pane[field].as_str())
        .find(|directory| Path::new(directory).is_absolute())
        .map(str::to_owned)
}

fn eligible(path: &Path) -> bool {
    fs::read(path)
        .ok()
        .and_then(|bytes| serde_json::from_slice::<Value>(&bytes).ok())
        .is_some_and(|desktop| {
            desktop["herdr"] == true
                && desktop["frames"]
                    .as_array()
                    .is_some_and(|frames| !frames.is_empty())
        })
}

fn write_request(
    path: &Path,
    desktop: &Path,
    generation: u64,
    directory: Option<&str>,
) -> io::Result<()> {
    let temporary = path.with_extension("tmp");
    let file = fs::OpenOptions::new()
        .write(true)
        .create(true)
        .truncate(true)
        .mode(0o600)
        .open(&temporary)?;
    serde_json::to_writer(
        file,
        &json!({
            "desktop": desktop, "generation": generation, "directory": directory
        }),
    )?;
    fs::rename(temporary, path)
}

/// JSON strings share Lisp's quoting for paths, except JSON's unsupported \u escapes.
fn lisp_string(value: &str) -> String {
    let mut quoted = String::from("\"");
    for character in value.chars() {
        match character {
            '"' | '\\' => {
                quoted.push('\\');
                quoted.push(character);
            }
            '\n' => quoted.push_str("\\n"),
            '\r' => quoted.push_str("\\r"),
            _ => quoted.push(character),
        }
    }
    quoted.push('"');
    quoted
}

fn refresh(request: &Path, generation: u64) -> io::Result<Child> {
    let expression = format!(
        "(when (fboundp 'my-herdr-magit-sync) (my-herdr-magit-sync {} {generation}))",
        lisp_string(&request.to_string_lossy())
    );
    Command::new("emacsclient")
        .args(["--alternate-editor=false", "--quiet", "--eval", &expression])
        .stdin(Stdio::null())
        .stdout(Stdio::null())
        .spawn()
}

fn schedule(
    event: Event,
    request: &Path,
    desktop: &Path,
    generation: &mut u64,
    pending: &mut Option<(String, Instant)>,
) -> io::Result<()> {
    *generation += 1;
    // Invalidate even a request already queued inside a busy Emacs.
    write_request(request, desktop, *generation, None)?;
    *pending = match event {
        Event::Focus(pane) if eligible(desktop) => Some((pane, Instant::now() + DEBOUNCE)),
        _ => None,
    };
    Ok(())
}

fn run(desktop: &Path, parent: i32) -> io::Result<()> {
    let socket = socket_path()?;
    let request = desktop.with_extension("request.json");
    let (sender, receiver) = mpsc::channel();
    let subscription_socket = socket.clone();
    thread::spawn(move || {
        loop {
            // A reconnect invalidates pending work. Only a new switch starts another refresh.
            if sender.send(Event::Reset).is_err() {
                break;
            }
            if let Err(error) = listen(&subscription_socket, &sender)
                && error.kind() != io::ErrorKind::NotFound
                && error.kind() != io::ErrorKind::ConnectionRefused
            {
                eprintln!("herdr-magit-sync: {error}");
            }
            if sender.send(Event::Reset).is_err() {
                break;
            }
            thread::sleep(Duration::from_secs(1));
        }
    });

    let mut generation = 0;
    let mut pending: Option<(String, Instant)> = None;
    let mut child: Option<(Child, Instant)> = None;
    while rustix::process::getppid().map(|parent| parent.as_raw_nonzero().get()) == Some(parent) {
        if let Ok(event) = receiver.recv_timeout(TICK) {
            let mut latest = event;
            for event in receiver.try_iter() {
                latest = event;
            }
            schedule(latest, &request, desktop, &mut generation, &mut pending)?;
        }

        if let Some((process, started)) = &mut child {
            if process.try_wait()?.is_some() {
                child = None;
            } else if started.elapsed() > Duration::from_secs(15) {
                generation += 1;
                write_request(&request, desktop, generation, None)?;
                let _ = process.kill();
                let _ = process.wait();
                child = None;
            }
        }

        if child.is_none()
            && pending
                .as_ref()
                .is_some_and(|(_, deadline)| Instant::now() >= *deadline)
        {
            let (pane_id, _) = pending.take().unwrap();
            if !eligible(desktop) {
                continue;
            }
            match current_pane(&socket) {
                Ok(pane) => {
                    // A switch during the socket round trip supersedes this answer.
                    if let Some(event) = receiver.try_iter().last() {
                        schedule(event, &request, desktop, &mut generation, &mut pending)?;
                        continue;
                    }
                    if let Some(directory) = directory(&pane, &pane_id) {
                        write_request(&request, desktop, generation, Some(&directory))?;
                        match refresh(&request, generation) {
                            Ok(process) => child = Some((process, Instant::now())),
                            Err(error) => eprintln!("herdr-magit-sync: {error}"),
                        }
                    }
                }
                Err(error) => eprintln!("herdr-magit-sync: {error}"),
            }
        }
    }

    let _ = fs::remove_file(request);
    if let Some((mut child, _)) = child {
        let _ = child.kill();
        let _ = child.wait();
    }
    Ok(())
}

fn main() -> io::Result<()> {
    let arguments: Vec<_> = std::env::args_os().skip(1).collect();
    if arguments.len() != 2 {
        return Err(io::Error::other(
            "usage: herdr-magit-sync DESKTOP-SNAPSHOT PARENT-PID",
        ));
    }
    let parent = arguments[1]
        .to_str()
        .and_then(|value| value.parse().ok())
        .ok_or_else(|| io::Error::other("invalid parent PID"))?;
    run(Path::new(&arguments[0]), parent)
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn directory_belongs_to_the_selected_pane() {
        let pane = json!({"pane_id": "w1:p2", "foreground_cwd": "/work/checkout", "cwd": "/work"});
        assert_eq!(directory(&pane, "w1:p2").as_deref(), Some("/work/checkout"));
        assert_eq!(directory(&pane, "w1:p1"), None);
        assert_eq!(
            directory(&json!({"pane_id": "p", "cwd": "/fallback"}), "p").as_deref(),
            Some("/fallback")
        );
        assert_eq!(
            directory(&json!({"pane_id": "p", "cwd": "relative"}), "p"),
            None
        );
    }

    #[test]
    fn paths_are_lisp_data() {
        assert_eq!(lisp_string("/tmp/a\"\\\nλ"), "\"/tmp/a\\\"\\\\\\nλ\"");
    }

    #[test]
    fn subscriptions_receive_focus_events() {
        use std::os::unix::net::UnixListener;
        let path = std::env::temp_dir().join(format!("magit-events-{}.sock", std::process::id()));
        let listener = UnixListener::bind(&path).unwrap();
        let server = thread::spawn(move || {
            let (mut stream, _) = listener.accept().unwrap();
            let mut request = String::new();
            BufReader::new(stream.try_clone().unwrap())
                .read_line(&mut request)
                .unwrap();
            assert_eq!(
                serde_json::from_str::<Value>(&request).unwrap()["method"],
                "events.subscribe"
            );
            stream.write_all(b"{\"id\":\"magit\",\"result\":{\"type\":\"subscription_started\"}}\n{\"event\":\"pane.focused\",\"data\":{\"pane_id\":\"w1:p2\"}}\n").unwrap();
        });
        let (sender, receiver) = mpsc::channel();
        assert!(listen(&path, &sender).is_err());
        assert!(matches!(receiver.try_recv(), Ok(Event::Focus(pane)) if pane == "w1:p2"));
        server.join().unwrap();
        fs::remove_file(path).unwrap();
    }
}
