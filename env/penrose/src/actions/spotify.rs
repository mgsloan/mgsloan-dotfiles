//! Remote control of Spotifast through MPRIS and its CLI.
//! Blocking calls run on worker threads.

use std::thread;

use penrose::{builtin::actions::key_handler, core::bindings::KeyEventHandler};
use tracing::{debug, warn};

use crate::{Conn, env, menu, notify::notify, process};

pub fn search_play() -> Box<dyn KeyEventHandler<Conn>> {
    key_handler(|_, _| {
        thread::spawn(|| {
            let Some(query) = menu::prompt("Play") else {
                return;
            };
            let result = search_track(&query)
                .map_err(|error| format!("search failed: {error}"))
                .and_then(|uri| local_control(&["open", &uri]));
            if let Err(error) = result {
                report(error);
            }
        });
        Ok(())
    })
}

fn search_track(query: &str) -> Result<String, Box<dyn std::error::Error>> {
    let client_id = std::fs::read_to_string(env::get().home("ep/secrets/spotify/client_id"))?;
    let client_secret =
        std::fs::read_to_string(env::get().home("ep/secrets/spotify/client_secret"))?;
    let agent: ureq::Agent = ureq::Agent::config_builder()
        .timeout_global(Some(std::time::Duration::from_secs(15)))
        .build()
        .into();
    let token: serde_json::Value = agent
        .post("https://accounts.spotify.com/api/token")
        .send_form([
            ("grant_type", "client_credentials"),
            ("client_id", client_id.trim()),
            ("client_secret", client_secret.trim()),
        ])?
        .body_mut()
        .read_json()?;
    let token = token["access_token"].as_str().ok_or("no access token")?;
    let response: serde_json::Value = agent
        .get("https://api.spotify.com/v1/search")
        .header("Authorization", &format!("Bearer {token}"))
        .query("q", query)
        .query("type", "track")
        .query("limit", "1")
        // Client credentials have no user country, so supply a playback market.
        .query("market", "US")
        .call()?
        .body_mut()
        .read_json()?;
    Ok(response["tracks"]["items"][0]["uri"]
        .as_str()
        .ok_or("no matching track")?
        .to_owned())
}

pub fn toggle_play_binding() -> Box<dyn KeyEventHandler<Conn>> {
    key_handler(|_, _| {
        toggle_play();
        Ok(())
    })
}

pub fn next() -> Box<dyn KeyEventHandler<Conn>> {
    control("next")
}

pub fn previous() -> Box<dyn KeyEventHandler<Conn>> {
    control("previous")
}

/// Play or pause, whichever the player is not doing.
pub fn toggle_play() {
    dbus("play-pause");
}

/// Stop playback, used by the context-dependent play key in `audio.rs`.
pub fn stop() {
    dbus("pause");
}

/// Toggle whether the current track is saved in the user's library.
pub fn like() -> Box<dyn KeyEventHandler<Conn>> {
    key_handler(|_, _| {
        thread::spawn(|| match process::status("spotifast", &["like"]) {
            Ok(0) => (),
            Ok(code) => report(format!(
                "spotifast like failed ({code}); is Spotifast running with Linux like support?"
            )),
            Err(error) => report(format!("unable to run spotifast: {error}")),
        });

        Ok(())
    })
}

/// Say what is playing.
pub fn notify_track() -> Box<dyn KeyEventHandler<Conn>> {
    key_handler(|_, _| {
        thread::spawn(|| match track_info() {
            Ok(track) => notify(&format!("Current track: {}", track)),
            Err(e) => report(e),
        });

        Ok(())
    })
}

/// Set the volume on whatever device is playing.
pub fn set_volume(percent: i64) -> Box<dyn KeyEventHandler<Conn>> {
    key_handler(move |_, _| {
        thread::spawn(move || apply_volume(percent));

        Ok(())
    })
}

/// Nudge the volume, which means reading it first.
pub fn add_volume(delta: i64) -> Box<dyn KeyEventHandler<Conn>> {
    key_handler(move |_, _| {
        thread::spawn(move || match local_volume() {
            Ok(current) => apply_volume(current + delta),
            Err(error) => report(error),
        });

        Ok(())
    })
}

/// Log the player metadata.
pub fn debug_player_info() -> Box<dyn KeyEventHandler<Conn>> {
    key_handler(|_, _| {
        thread::spawn(|| match local_output(&["metadata"]) {
            Ok(info) => debug!(%info, "spotifast player info"),
            Err(error) => report(error),
        });

        Ok(())
    })
}

/// Clear the desktop client's cache.
///
/// Works around share links failing with "something went wrong", which has been
/// a Linux client bug for years.
pub fn clear_cache() {
    let env = env::get();

    for dir in [".cache/spotify", "snap/spotify/common/.cache"] {
        let path = env.home(dir);

        if let Err(e) = std::fs::remove_dir_all(&path) {
            debug!(%e, path, "nothing to clear");
        }
    }

    notify("Cleared the spotify cache");
}

fn control(command: &'static str) -> Box<dyn KeyEventHandler<Conn>> {
    key_handler(move |_, _| {
        dbus(command);
        Ok(())
    })
}

fn apply_volume(percent: i64) {
    let percent = percent.clamp(0, 100);

    if let Err(error) = local_control(&["volume", &(percent as f64 / 100.0).to_string()]) {
        report(error);
        return;
    }
    crate::notify::transient("spotify-control", &format!("Volume {percent}"));
}

/// What the notifications say about a track.
struct Track {
    name: String,
    artists: Vec<String>,
}

impl std::fmt::Display for Track {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{} by {}", self.name, self.artists.join(", "))
    }
}

fn track_info() -> Result<Track, String> {
    let metadata = local_output(&[
        "metadata",
        "--format",
        "{{xesam:url}}\n{{title}}\n{{artist}}",
    ])?;
    local_track(&metadata)
}

// Spotifast 0.8.0 retains its original MPRIS name on Linux.
const PLAYER: &str = "--player=fastpotify";

fn local_control(arguments: &[&str]) -> Result<(), String> {
    let arguments = [vec![PLAYER], arguments.to_vec()].concat();
    match process::status("playerctl", &arguments) {
        Ok(0) => Ok(()),
        Ok(code) => Err(format!("playerctl failed ({code}); is Spotifast running?")),
        Err(error) => Err(format!("unable to run playerctl: {error}")),
    }
}

fn local_output(arguments: &[&str]) -> Result<String, String> {
    let arguments = [vec![PLAYER], arguments.to_vec()].concat();
    let output = process::read_output("playerctl", &arguments)
        .map_err(|error| format!("unable to run playerctl: {error}"))?;
    if output.trim().is_empty() {
        return Err("no player information; is Spotifast running?".to_owned());
    }
    Ok(output)
}

fn local_volume() -> Result<i64, String> {
    let output = local_output(&["volume"])?;
    let volume: f64 = output
        .trim()
        .parse()
        .map_err(|_| "invalid Spotifast volume")?;
    if !volume.is_finite() {
        return Err("invalid Spotifast volume".to_owned());
    }
    Ok((volume * 100.0).round() as i64)
}

fn local_track(metadata: &str) -> Result<Track, String> {
    let mut lines = metadata.lines();
    lines
        .next()
        .and_then(|uri| uri.strip_prefix("spotify:track:"))
        .filter(|id| !id.is_empty())
        .ok_or("no Spotify track is playing")?;
    let name = lines
        .next()
        .filter(|name| !name.is_empty())
        .ok_or("no track title")?;
    Ok(Track {
        name: name.to_owned(),
        artists: lines.map(str::to_owned).collect(),
    })
}

fn dbus(command: &'static str) {
    thread::spawn(move || {
        if let Err(error) = local_control(&[command]) {
            report(error);
        }
    });
}

/// Surface a failure where it will be seen, not just in the journal.
fn report(e: String) {
    warn!(error = %e, "spotify");
    notify(&format!("Spotify: {e}"));
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn local_metadata_supplies_the_current_track() {
        let track = local_track("spotify:track:abc\nUnder Pressure\nQueen, David Bowie\n").unwrap();
        assert_eq!(track.to_string(), "Under Pressure by Queen, David Bowie");
    }

    #[test]
    fn local_metadata_rejects_missing_tracks_and_episodes() {
        for metadata in [
            "",
            "\n\n",
            "spotify:track:\nTitle\nArtist",
            "spotify:episode:abc\nEpisode\n",
        ] {
            assert!(local_track(metadata).is_err());
        }
    }

    #[test]
    fn a_track_reads_as_a_sentence() {
        let track = Track {
            name: "Blue Monday".to_owned(),
            artists: vec!["New Order".to_owned()],
        };

        assert_eq!(track.to_string(), "Blue Monday by New Order");
    }

    #[test]
    fn several_artists_are_listed() {
        let track = Track {
            name: "Under Pressure".to_owned(),
            artists: vec!["Queen".to_owned(), "David Bowie".to_owned()],
        };

        assert_eq!(track.to_string(), "Under Pressure by Queen, David Bowie");
    }
}
