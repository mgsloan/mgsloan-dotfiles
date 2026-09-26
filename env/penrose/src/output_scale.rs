//! Configure River output scaling after startup and screen changes.

use std::{
    error::Error,
    io,
    sync::{
        OnceLock,
        mpsc::{self, Sender},
    },
    thread,
};

use penrose::core::{State, hooks::EventHook};
use serde::Deserialize;
use tracing::warn;

use crate::{Conn, process};

static CHANGES: OnceLock<Sender<()>> = OnceLock::new();

#[derive(Deserialize)]
struct Mode {
    width: u32,
    height: u32,
    current: bool,
}

#[derive(Deserialize)]
struct Output {
    name: String,
    make: String,
    model: String,
    serial: Option<String>,
    enabled: bool,
    scale: Option<f64>,
    modes: Vec<Mode>,
}

fn current_mode(output: &Output) -> Option<&Mode> {
    output.modes.iter().find(|mode| mode.current)
}

fn is_high_resolution(mode: &Mode) -> bool {
    let (width, height) = (mode.width.min(mode.height), mode.width.max(mode.height));
    width >= 2160 && height >= 3840
}

fn changes(outputs: &[Output]) -> Vec<String> {
    let mut arguments = Vec::new();

    for output in outputs.iter().filter(|output| output.enabled) {
        let Some(mode) = current_mode(output) else {
            continue;
        };

        // The Studio Display also exposes a half-width stream with the same
        // serial; it is not another screen.
        let duplicate_half = outputs.iter().any(|other| {
            other.enabled
                && other.name != output.name
                && other.make == output.make
                && other.model == output.model
                && output.serial.is_some()
                && other.serial == output.serial
                && other.modes.iter().any(|full_mode| {
                    mode.width.checked_mul(2) == Some(full_mode.width)
                        && mode.height == full_mode.height
                        && is_high_resolution(full_mode)
                })
        });

        if duplicate_half {
            arguments.extend(["--output", &output.name, "--off"].map(str::to_owned));
        } else if is_high_resolution(mode) && output.scale != Some(2.0) {
            arguments.extend(["--output", &output.name, "--scale", "2"].map(str::to_owned));
        }
    }

    arguments
}

fn configure() -> Result<(), Box<dyn Error + Send + Sync>> {
    let output = process::read_output("wlr-randr", &["--json"])?;
    let outputs: Vec<Output> = serde_json::from_str(&output)?;
    let arguments = changes(&outputs);
    if arguments.is_empty() {
        return Ok(());
    }

    let arguments: Vec<&str> = arguments.iter().map(String::as_str).collect();
    let status = process::status("wlr-randr", &arguments)?;
    if status != 0 {
        return Err(io::Error::other(format!("wlr-randr exited with status {status}")).into());
    }
    Ok(())
}

pub fn start() {
    if std::env::var_os("PENROSE_NO_STARTUP_HOOK").is_some() {
        return;
    }

    let (sender, receiver) = mpsc::channel();
    CHANGES
        .set(sender)
        .expect("output scale worker started only once");
    thread::spawn(move || {
        while receiver.recv().is_ok() {
            while receiver.try_recv().is_ok() {}
            if let Err(error) = configure() {
                warn!(%error, "unable to configure output scale");
            }
        }
    });
    request();
}

fn request() {
    if let Some(sender) = CHANGES.get() {
        let _ = sender.send(());
    }
}

pub fn hook() -> Box<dyn EventHook<Conn>> {
    Box::new(
        |event: &<Conn as penrose::core::conn::Conn>::Event,
         _state: &mut State<Conn>,
         _conn: &mut Conn| {
            if matches!(event, penrose::river::RiverEvent::ScreenChange) {
                request();
            }
            Ok(true)
        },
    )
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn scales_full_outputs_and_disables_duplicate_half() {
        let outputs: Vec<Output> = serde_json::from_str(
            r#"[
                {"name":"eDP-1","make":"Sharp","model":"Laptop","serial":null,"enabled":true,"scale":1,"modes":[{"width":1920,"height":1080,"current":true}]},
                {"name":"DP-2","make":"Apple","model":"StudioDisplay","serial":"same","enabled":true,"scale":1,"modes":[{"width":5120,"height":2880,"current":true}]},
                {"name":"DP-3","make":"Apple","model":"StudioDisplay","serial":"same","enabled":true,"scale":1,"modes":[{"width":2560,"height":2880,"current":true}]},
                {"name":"DP-4","make":"Other","model":"4K","serial":"different","enabled":true,"scale":1,"modes":[{"width":3840,"height":2160,"current":true}]}
            ]"#,
        ).unwrap();

        assert_eq!(
            changes(&outputs),
            [
                "--output", "DP-2", "--scale", "2", "--output", "DP-3", "--off", "--output",
                "DP-4", "--scale", "2"
            ]
        );
    }
}
