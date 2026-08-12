//! Volume, microphone, brightness, and the media keys.
//!
//! `amixer` is the whole of the audio story: it prints the new state, and that
//! line is what gets shown, so the notification says what actually happened
//! rather than what was asked for. Reading it back needs the child's stdout,
//! which is `process::read_output` and its EOF trick.

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
/// Ignoring spotify because it is an MPRIS player too, and pausing it here would
/// fight with the `stop` that follows. Everything else — Chrome, mpv, vlc — is a
/// candidate, most recently active first, which is the one being watched.
fn pause_video() {
    if !programs::installed("playerctl") {
        warn!("playerctl is not installed: not pausing the video");
        return;
    }

    if let Err(e) = process::spawn("playerctl", &["--ignore-player=spotify", "play-pause"]) {
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

#[cfg(test)]
mod tests {
    use super::*;

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
