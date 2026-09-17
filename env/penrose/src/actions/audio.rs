//! Volume, channel mapping, microphone, brightness, and the media keys.
//!
//! `amixer` handles volume: it prints the new state, and that
//! line is what gets shown, so the notification says what actually happened
//! rather than what was asked for. Reading it back needs the child's stdout,
//! which is `process::read_output` and its EOF trick.

use std::{sync::Mutex, thread};

use penrose::{
    builtin::actions::key_handler,
    core::{bindings::KeyEventHandler, conn::Conn as _},
};
use tracing::warn;

use crate::{Conn, actions::spotify, env, notify, process, programs};

/// Step used by the volume up and down bindings, as in the xmonad config.
const VOLUME_STEP: &str = "5%";

pub fn volume_up() -> Box<dyn KeyEventHandler<Conn>> {
    key_handler(|_, _| {
        unmute();
        amixer(&["set", "Master", &format!("{VOLUME_STEP}+")]);

        Ok(())
    })
}

pub fn volume_down() -> Box<dyn KeyEventHandler<Conn>> {
    key_handler(|_, _| {
        unmute();
        amixer(&["set", "Master", &format!("{VOLUME_STEP}-")]);

        Ok(())
    })
}

pub fn volume_max() -> Box<dyn KeyEventHandler<Conn>> {
    key_handler(|_, _| {
        unmute();
        amixer(&["set", "Master", "100%"]);

        Ok(())
    })
}

pub fn mute_toggle() -> Box<dyn KeyEventHandler<Conn>> {
    key_handler(|_, _| {
        amixer(&["set", "Master", "toggle"]);

        Ok(())
    })
}

/// The microphone reports two lines worth of state, so it shows both.
pub fn microphone_toggle() -> Box<dyn KeyEventHandler<Conn>> {
    key_handler(|_, _| {
        let output = run_amixer(&["set", "Capture", "toggle"]);
        let tail: Vec<&str> = output.lines().rev().take(2).collect();

        notify::transient(
            "amixer",
            &tail.into_iter().rev().collect::<Vec<_>>().join("\n"),
        );

        Ok(())
    })
}

/// Brightness, in whatever units `brightness-set.sh` deals in.
pub fn brightness(script: &'static str, arg: &'static str) -> Box<dyn KeyEventHandler<Conn>> {
    key_handler(move |_, _| {
        let path = env::get().script(script);

        if let Err(e) = process::spawn(&path, &[arg]) {
            warn!(%e, script, "unable to change brightness");
        }

        Ok(())
    })
}

/// `XF86AudioPlay`: pause the video if one is focused, otherwise Spotify.
///
/// A media key that always went to Spotify would be wrong while watching
/// something, and the window title is the only signal available for telling
/// those apart — the same heuristic the xmonad config uses.
pub fn play_pause() -> Box<dyn KeyEventHandler<Conn>> {
    key_handler(|state, conn: &mut Conn| {
        let title = state
            .client_set
            .current_client()
            .copied()
            .and_then(|id| conn.client_title(id).ok())
            .unwrap_or_default();

        if is_video(&title) {
            pause_video();
            spotify::stop();
        } else {
            spotify::toggle_play();
        }

        Ok(())
    })
}

/// Pause whatever is playing that is not Spotify.
///
/// MPRIS over dbus, which addresses the player itself: no window, no focus, and
/// the same on either backend.
///
/// Excluding Spotify and Spotifast avoids fighting with the `stop` that follows.
/// Everything else — Chrome, mpv, vlc — is a
/// candidate, most recently active first, which is the one being watched.
fn pause_video() {
    if !programs::installed("playerctl") {
        warn!("playerctl is not installed: not pausing the video");
        return;
    }

    if let Err(e) = process::spawn(
        "playerctl",
        &["--ignore-player=spotify,fastpotify", "play-pause"],
    ) {
        warn!(%e, "unable to pause the video");
    }
}

/// Does this window title look like something playing video?
///
/// Deliberately a short list of what this is actually used with, rather than a
/// guess at every video site: a false positive silently swallows the key.
fn is_video(title: &str) -> bool {
    const SUFFIXES: [&str; 3] = [
        " - YouTube - Google Chrome",
        " | Prime Video - Google Chrome",
        " | Coursera - Google Chrome",
    ];

    title == "Netflix - Google Chrome" || SUFFIXES.iter().any(|s| title.ends_with(s))
}

fn unmute() {
    run_amixer(&["set", "Master", "unmute"]);
}

/// Run amixer and show the state it reports back.
fn amixer(args: &[&str]) {
    let output = run_amixer(args);

    if let Some(last) = output.lines().next_back() {
        notify::transient("amixer", last.trim());
    }
}

fn run_amixer(args: &[&str]) -> String {
    match process::read_output("amixer", args) {
        Ok(output) => output,
        Err(e) => {
            warn!(%e, ?args, "unable to run amixer");
            String::new()
        }
    }
}

const FLIPPED_SINK: &str = "penrose_flip_pan";
const PAN_REMINDER_SECONDS: u64 = 30 * 60;
static PAN_LOCK: Mutex<()> = Mutex::new(());

/// Audio queries and mutations share a lock with the reminder worker.
pub fn flip_pan(flipped: bool) {
    thread::spawn(move || {
        let _guard = PAN_LOCK.lock().unwrap();
        match set_pan(flipped) {
            Ok(()) => notify::notify(if flipped {
                "Headphone channels flipped; reminder every 30 minutes"
            } else {
                "Normal headphone channels restored"
            }),
            Err(error) => {
                warn!(%error, "unable to change headphone channels");
                notify::notify(&format!("Unable to change headphone channels: {error}"));
            }
        }
    });
}

fn pactl(arguments: &[&str]) -> Result<String, String> {
    // The shell reports failure in-band because SIGCHLD is ignored by the WM.
    let mut command = vec![
        "-c",
        r#"pactl "$@" 2>&1 || printf '\n__pactl_failed\n'"#,
        "pactl",
    ];
    command.extend_from_slice(arguments);
    let output = process::read_output("sh", &command).map_err(|error| error.to_string())?;
    if let Some(error) = output.strip_suffix("__pactl_failed\n") {
        return Err(error.trim().to_owned());
    }
    Ok(output.trim().to_owned())
}

fn audio_list(kind: &str) -> Result<Vec<serde_json::Value>, String> {
    serde_json::from_str(&pactl(&["--format=json", "list", kind])?)
        .map_err(|error| error.to_string())
}

fn pan_module() -> Result<Option<(String, String)>, String> {
    for line in pactl(&["list", "short", "modules"])?.lines() {
        let fields: Vec<_> = line.split('\t').collect();
        if fields.get(1) != Some(&"module-remap-sink") {
            continue;
        }
        let arguments = fields.get(2).copied().unwrap_or_default();
        if arguments
            .split_whitespace()
            .any(|argument| argument.strip_prefix("sink_name=") == Some(FLIPPED_SINK))
        {
            let master = arguments
                .split_whitespace()
                .find_map(|argument| argument.strip_prefix("master="))
                .ok_or("Flipped output has no master")?;
            return Ok(Some((fields[0].to_owned(), master.to_owned())));
        }
    }
    Ok(None)
}

fn move_audio(source: &str, destination: &str) -> Result<(), String> {
    let sinks = audio_list("sinks")?;
    let Some(source) = sinks.iter().find(|sink| sink["name"] == source) else {
        return Ok(());
    };
    for stream in audio_list("sink-inputs")? {
        if stream["sink"] == source["index"] {
            let index = stream["index"].as_u64().ok_or("Stream has no index")?;
            // A stream may finish between listing it and moving it.
            if let Err(error) = pactl(&["move-sink-input", &index.to_string(), destination]) {
                warn!(%error, "unable to move audio stream");
            }
        }
    }
    Ok(())
}

fn set_pan(flipped: bool) -> Result<(), String> {
    let module = pan_module()?;
    if flipped {
        if module.is_some() {
            return Ok(());
        }
        let master = pactl(&["get-default-sink"])?;
        let module = pactl(&[
            "load-module",
            "module-remap-sink",
            &format!("sink_name={FLIPPED_SINK}"),
            &format!("master={master}"),
            "channels=2",
            "channel_map=front-left,front-right",
            "master_channel_map=front-right,front-left",
            "remix=no",
            "sink_properties=device.description=Flipped_headphones",
        ])?;
        let result = (|| {
            pactl(&["set-default-sink", FLIPPED_SINK])?;
            move_audio(&master, FLIPPED_SINK)?;
            save_pan_deadline(pan_now() + PAN_REMINDER_SECONDS)
        })();
        if let Err(error) = result {
            let _ = pactl(&["set-default-sink", &master]);
            let _ = move_audio(FLIPPED_SINK, &master);
            let _ = pactl(&["unload-module", &module]);
            return Err(error);
        }
        remind_pan(module, pan_now() + PAN_REMINDER_SECONDS);
    } else if let Some((module, master)) = module {
        // Preserve an output selected independently while the channels were flipped.
        if pactl(&["get-default-sink"])? == FLIPPED_SINK {
            pactl(&["set-default-sink", &master])?;
        }
        move_audio(FLIPPED_SINK, &master)?;
        pactl(&["unload-module", &module])?;
        let _ = std::fs::remove_file(env::get().state("pan-reminder-at"));
    }
    Ok(())
}

fn pan_now() -> u64 {
    jiff::Timestamp::now().as_second().max(0) as u64
}

fn save_pan_deadline(deadline: u64) -> Result<(), String> {
    let path = env::get().state("pan-reminder-at");
    let directory = std::path::Path::new(&path).parent().unwrap();
    std::fs::create_dir_all(directory)
        .and_then(|()| std::fs::write(&path, deadline.to_string()))
        .map_err(|error| error.to_string())
}

/// Remapping survives WM restarts, so its reminder must resume too.
pub fn startup_pan() {
    thread::spawn(|| {
        let _guard = PAN_LOCK.lock().unwrap();
        match pan_module() {
            Ok(Some((module, _))) => {
                let deadline = std::fs::read_to_string(env::get().state("pan-reminder-at"))
                    .ok()
                    .and_then(|text| text.trim().parse().ok())
                    .unwrap_or_else(|| pan_now() + PAN_REMINDER_SECONDS);
                remind_pan(module, deadline);
            }
            Ok(None) => (),
            Err(error) => warn!(%error, "unable to restore headphone reminder"),
        }
    });
}

fn remind_pan(module: String, mut deadline: u64) {
    thread::spawn(move || {
        loop {
            if let Err(error) = crate::time::sleep_until(deadline) {
                warn!(%error, "unable to wait for headphone reminder");
                return;
            }
            let _guard = PAN_LOCK.lock().unwrap();
            match pan_module() {
                Ok(Some((current, _))) if current == module => {
                    notify::notify(
                        "Headphone channels are still flipped. M-x unflip-pan restores left and right.",
                    );
                }
                Ok(_) => return,
                Err(error) => warn!(%error, "unable to check headphone channel mapping"),
            }
            deadline = pan_now() + PAN_REMINDER_SECONDS;
            if let Err(error) = save_pan_deadline(deadline) {
                warn!(%error, "unable to save headphone reminder deadline");
            }
        }
    });
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    #[ignore = "requires a disposable audio server and XDG_STATE_HOME"]
    fn pan_round_trip() {
        assert!(std::env::var("PENROSE_TEST_AUDIO").is_ok());
        env::init();
        let _guard = PAN_LOCK.lock().unwrap();
        let master = pactl(&["get-default-sink"]).unwrap();
        assert!(pan_module().unwrap().is_none());
        set_pan(true).unwrap();
        let module = pan_module().unwrap().unwrap();
        assert_eq!(module.1, master);
        assert_eq!(pactl(&["get-default-sink"]).unwrap(), FLIPPED_SINK);
        let arguments = pactl(&["list", "short", "modules"]).unwrap();
        assert!(arguments.contains("master_channel_map=front-right,front-left"));
        assert!(arguments.contains("remix=no"));
        set_pan(true).unwrap();
        assert_eq!(pan_module().unwrap().unwrap(), module);
        assert!(std::path::Path::new(&env::get().state("pan-reminder-at")).exists());
        set_pan(false).unwrap();
        assert_eq!(pactl(&["get-default-sink"]).unwrap(), master);
        assert!(pan_module().unwrap().is_none());
        assert!(!std::path::Path::new(&env::get().state("pan-reminder-at")).exists());
        set_pan(false).unwrap();
        assert!(pactl(&["set-default-sink", "nonexistent-test-sink"]).is_err());
    }

    #[test]
    fn video_titles_are_recognised() {
        assert!(is_video("Netflix - Google Chrome"));
        assert!(is_video("Some Talk - YouTube - Google Chrome"));
        assert!(is_video("A Film | Prime Video - Google Chrome"));
        assert!(is_video("Week 3 | Coursera - Google Chrome"));
    }

    #[test]
    fn other_titles_are_not() {
        assert!(!is_video(""));
        assert!(!is_video("design.md - Emacs"));
        assert!(
            !is_video("YouTube - Google Chrome"),
            "the channel page, not a video"
        );
        assert!(!is_video("Netflix - Mozilla Firefox"));
    }
}
