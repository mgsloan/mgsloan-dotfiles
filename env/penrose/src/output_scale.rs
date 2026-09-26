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
    #[serde(default)]
    preferred: bool,
}

#[derive(Deserialize)]
struct PhysicalSize {
    width: u32,
    height: u32,
}

#[derive(Deserialize)]
struct Output {
    name: String,
    make: String,
    model: String,
    serial: Option<String>,
    physical_size: Option<PhysicalSize>,
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

fn pixel_density(output: &Output, mode: &Mode) -> Option<f64> {
    let size = output.physical_size.as_ref()?;
    if size.width == 0 || size.height == 0 || mode.width == 0 || mode.height == 0 {
        return None;
    }
    let pixels = f64::from(mode.width).hypot(f64::from(mode.height));
    let millimeters = f64::from(size.width).hypot(f64::from(size.height));
    Some(pixels * 25.4 / millimeters)
}

fn desired_scale(output: &Output, mode: &Mode, reference_density: f64) -> f64 {
    if let Some(density) = pixel_density(output, mode) {
        return ((density / reference_density * 4.0).round() / 4.0).max(1.0);
    }

    // Keep the resolution-based fallback when physical dimensions are unknown.
    if is_high_resolution(mode) { 2.0 } else { 1.0 }
}

fn changes(outputs: &[Output]) -> Vec<String> {
    let mut arguments = Vec::new();
    let reference_density = outputs
        .iter()
        .filter(|output| output.name.starts_with("eDP-") || output.name.starts_with("LVDS-"))
        .find_map(|output| {
            // A disabled laptop panel can still supply its preferred mode.
            let mode =
                current_mode(output).or_else(|| output.modes.iter().find(|mode| mode.preferred))?;
            pixel_density(output, mode)
        })
        // Use the original laptop's density when no usable panel is reported.
        .unwrap_or(158.0);

    for output in outputs.iter().filter(|output| output.enabled) {
        let Some(mode) = current_mode(output) else {
            continue;
        };

        // The Studio Display also exposes a half-width stream with the same
        // serial; it is not another screen.
        // TODO: replace this heuristic with proper handling of its two outputs.
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
        } else {
            let scale = desired_scale(output, mode, reference_density);
            if output.scale != Some(scale) {
                arguments.extend(["--output", &output.name, "--scale"].map(str::to_owned));
                arguments.push(scale.to_string());
            }
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
    fn uses_laptop_density_even_when_panel_is_disabled() {
        let mut outputs: Vec<Output> = serde_json::from_str(
            r#"[
                {"name":"eDP-1","make":"Test","model":"Laptop","physical_size":{"width":400,"height":225},"enabled":true,"scale":1,"modes":[{"width":1920,"height":1080,"current":true,"preferred":true}]},
                {"name":"DP-1","make":"Test","model":"External","physical_size":{"width":400,"height":225},"enabled":true,"scale":1,"modes":[{"width":3840,"height":2160,"current":true}]}
            ]"#,
        ).unwrap();

        assert_eq!(changes(&outputs), ["--output", "DP-1", "--scale", "2"]);
        outputs[0].enabled = false;
        outputs[0].modes[0].current = false;
        assert_eq!(changes(&outputs), ["--output", "DP-1", "--scale", "2"]);

        outputs[0].modes[0].width = 2560;
        outputs[0].modes[0].height = 1440;
        assert_eq!(changes(&outputs), ["--output", "DP-1", "--scale", "1.5"]);

        outputs[0].physical_size = None;
        assert_eq!(changes(&outputs), ["--output", "DP-1", "--scale", "1.5"]);
    }

    #[test]
    fn scales_by_density_and_settles_after_applying() {
        for (width, height, physical_size, expected) in [
            (1920, 1080, Some((310, 170)), 1.0),
            (3840, 2160, Some((710, 400)), 1.0),
            (5120, 2880, Some((600, 340)), 1.25),
            (2880, 5120, Some((340, 600)), 1.25),
            (3840, 2160, Some((310, 170)), 2.0),
            (3840, 2160, None, 2.0),
            (3840, 2160, Some((0, 400)), 2.0),
            (3840, 2160, Some((710, 0)), 2.0),
            (1920, 1080, None, 1.0),
        ] {
            let mut output = Output {
                name: "DP-1".into(),
                make: "Test".into(),
                model: "Test".into(),
                serial: None,
                physical_size: physical_size.map(|(width, height)| PhysicalSize { width, height }),
                enabled: true,
                scale: Some(3.0),
                modes: vec![Mode {
                    width,
                    height,
                    current: true,
                    preferred: true,
                }],
            };
            assert_eq!(
                changes(std::slice::from_ref(&output)),
                ["--output", "DP-1", "--scale", &expected.to_string()]
            );
            output.scale = Some(expected);
            assert!(changes(std::slice::from_ref(&output)).is_empty());
            output.scale = Some(3.0);
            output.enabled = false;
            assert!(changes(std::slice::from_ref(&output)).is_empty());
            output.enabled = true;
            output.modes[0].current = false;
            assert!(changes(&[output]).is_empty());
        }
    }

    #[test]
    fn scales_full_outputs_and_disables_duplicate_half() {
        let outputs: Vec<Output> = serde_json::from_str(
            r#"[
                {"name":"eDP-1","make":"Sharp","model":"Laptop","serial":null,"physical_size":{"width":310,"height":170},"enabled":true,"scale":1,"modes":[{"width":1920,"height":1080,"current":true}]},
                {"name":"DP-2","make":"Apple","model":"StudioDisplay","serial":"same","physical_size":{"width":600,"height":340},"enabled":true,"scale":1,"modes":[{"width":5120,"height":2880,"current":true}]},
                {"name":"DP-3","make":"Apple","model":"StudioDisplay","serial":"same","enabled":true,"scale":1,"modes":[{"width":2560,"height":2880,"current":true}]},
                {"name":"DP-4","make":"Other","model":"4K","serial":"different","physical_size":{"width":710,"height":400},"enabled":true,"scale":2,"modes":[{"width":3840,"height":2160,"current":true}]}
            ]"#,
        ).unwrap();

        assert_eq!(
            changes(&outputs),
            [
                "--output", "DP-2", "--scale", "1.25", "--output", "DP-3", "--off", "--output",
                "DP-4", "--scale", "1"
            ]
        );
    }
}
