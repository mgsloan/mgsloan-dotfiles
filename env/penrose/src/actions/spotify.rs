//! Spotify, over dbus or the Web API.
//!
//! Two transports, chosen by `SPOTIFY_NO_DBUS`. dbus talks to the desktop
//! client on this machine and is the default: no credentials, no network, no
//! latency. The Web API talks to whatever device the account is playing on,
//! which is the only way to reach a phone — and the only way to do the things
//! MPRIS does not expose at all: volume, liking a track, and knowing what is
//! playing.
//!
//! Everything here runs on a thread. The Web API calls block on the network,
//! and even the dbus ones spawn a process.

use std::{sync::Mutex, thread};

use jiff::{SignedDuration, Timestamp};
use penrose::{builtin::actions::key_handler, core::bindings::KeyEventHandler};
use serde_json::Value;
use tracing::{debug, error, warn};

use crate::{Conn, env, notify::notify, process};

const API: &str = "https://api.spotify.com/v1/me";
const TOKEN_URL: &str = "https://accounts.spotify.com/api/token";

/// Access tokens last an hour; refreshing slightly early avoids losing a race
/// with a request already in flight.
const EXPIRY_MARGIN: SignedDuration = SignedDuration::from_secs(5);

/// The current access token and when it stops being usable.
static ACCESS_TOKEN: Mutex<Option<(Timestamp, String)>> = Mutex::new(None);

pub fn toggle_play_binding() -> Box<dyn KeyEventHandler<Conn>> {
    key_handler(|_, _| {
        toggle_play();
        Ok(())
    })
}

pub fn next() -> Box<dyn KeyEventHandler<Conn>> {
    control("Next", Method::Post, "player/next")
}

pub fn previous() -> Box<dyn KeyEventHandler<Conn>> {
    control("Previous", Method::Post, "player/previous")
}

/// Play or pause, whichever the player is not doing.
///
/// dbus has one message for this; the Web API does not, so over the Web API it
/// has to ask first.
pub fn toggle_play() {
    if !env::get().spotify_no_dbus {
        dbus("PlayPause");
        return;
    }

    thread::spawn(|| match player_info() {
        Ok(info) => {
            let playing = info["is_playing"].as_bool().unwrap_or(false);

            if playing {
                web(Method::Put, "player/pause", &[]);
            } else {
                web(Method::Put, "player/play", &[]);
            }
        }
        Err(e) => report(e),
    });
}

/// Stop playback, used by the context-dependent play key in `audio.rs`.
pub fn stop() {
    if env::get().spotify_no_dbus {
        thread::spawn(|| web(Method::Put, "player/pause", &[]));
    } else {
        dbus("Pause");
    }
}

/// Add the current track to the user's saved tracks.
///
/// Web API only: there is no MPRIS equivalent, so this is one of the bindings
/// that does nothing useful without credentials.
pub fn like() -> Box<dyn KeyEventHandler<Conn>> {
    key_handler(|_, _| {
        thread::spawn(|| match track_info() {
            Ok(track) => {
                web(Method::Put, "tracks", &[("ids", &track.id)]);
                notify(&format!("Liked track: {}", track));
            }
            Err(e) => report(e),
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
        thread::spawn(move || match player_info() {
            Ok(info) => match info["device"]["volume_percent"].as_i64() {
                Some(current) => apply_volume(current + delta),
                None => report("no volume in the player info".to_owned()),
            },
            Err(e) => report(e),
        });

        Ok(())
    })
}

/// Log the whole player payload, for working out why something misbehaved.
pub fn debug_player_info() -> Box<dyn KeyEventHandler<Conn>> {
    key_handler(|_, _| {
        thread::spawn(|| match player_info() {
            Ok(info) => {
                debug!(info = %serde_json::to_string_pretty(&info).unwrap_or_default(), "spotify player info")
            }
            Err(e) => report(e),
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

/// A binding whose two transports differ only in which name they use.
fn control(
    dbus_cmd: &'static str,
    method: Method,
    path: &'static str,
) -> Box<dyn KeyEventHandler<Conn>> {
    key_handler(move |_, _| {
        if env::get().spotify_no_dbus {
            thread::spawn(move || web(method, path, &[]));
        } else {
            dbus(dbus_cmd);
        }

        Ok(())
    })
}

fn apply_volume(percent: i64) {
    let percent = percent.clamp(0, 100);

    web(
        Method::Put,
        "player/volume",
        &[("volume_percent", &percent.to_string())],
    );
    crate::notify::transient("spotify-control", &format!("Volume {percent}"));
}

/// What the notifications say about a track.
struct Track {
    id: String,
    name: String,
    artists: Vec<String>,
}

impl std::fmt::Display for Track {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{} by {}", self.name, self.artists.join(", "))
    }
}

fn track_info() -> Result<Track, String> {
    let info = player_info()?;
    let item = &info["item"];

    let id = item["id"]
        .as_str()
        .ok_or("no track id in the player info")?;
    let name = item["name"]
        .as_str()
        .ok_or("no track name in the player info")?;

    let artists = item["artists"]
        .as_array()
        .map(|artists| {
            artists
                .iter()
                .filter_map(|a| a["name"].as_str().map(str::to_owned))
                .collect()
        })
        .unwrap_or_default();

    Ok(Track {
        id: id.to_owned(),
        name: name.to_owned(),
        artists,
    })
}

fn player_info() -> Result<Value, String> {
    let token = access_token()?;

    let mut response = ureq::get(&format!("{API}/player"))
        .header("Authorization", &format!("Bearer {token}"))
        .call()
        .map_err(|e| format!("player request failed: {e}"))?;

    let body = response
        .body_mut()
        .read_to_string()
        .map_err(|e| format!("unable to read the player response: {e}"))?;

    // A 204 with an empty body is Spotify's way of saying nothing is playing.
    if body.trim().is_empty() {
        return Err("nothing is playing".to_owned());
    }

    serde_json::from_str(&body).map_err(|e| format!("unparseable player response: {e}"))
}

#[derive(Clone, Copy)]
enum Method {
    Put,
    Post,
}

/// Fire a Web API request that has no response worth reading.
fn web(method: Method, path: &str, query: &[(&str, &str)]) {
    if let Err(e) = try_web(method, path, query) {
        report(e);
    }
}

fn try_web(method: Method, path: &str, query: &[(&str, &str)]) -> Result<(), String> {
    let token = access_token()?;
    let url = format!("{API}/{path}");

    let mut request = match method {
        Method::Put => ureq::put(&url),
        Method::Post => ureq::post(&url),
    }
    .header("Authorization", &format!("Bearer {token}"));

    for (k, v) in query {
        request = request.query(*k, *v);
    }

    debug!(path, "spotify request");

    request
        .send_empty()
        .map(|_| ())
        .map_err(|e| format!("{path} failed: {e}"))
}

/// A usable access token, refreshed when the cached one has expired.
fn access_token() -> Result<String, String> {
    let mut cached = ACCESS_TOKEN
        .lock()
        .map_err(|e| format!("poisoned token lock: {e}"))?;

    if let Some((expiry, token)) = cached.as_ref()
        && Timestamp::now() < *expiry
    {
        return Ok(token.clone());
    }

    let (expiry, token) = refresh_token()?;
    *cached = Some((expiry, token.clone()));

    Ok(token)
}

/// Trade the refresh token for an access token.
fn refresh_token() -> Result<(Timestamp, String), String> {
    let env = env::get();

    let (Some(id), Some(secret), Some(refresh)) = (
        env.spotify_client_id.as_ref(),
        env.spotify_client_secret.as_ref(),
        env.spotify_refresh_token.as_ref(),
    ) else {
        return Err(
            "no credentials in ~/env/untracked (client_id, client_secret, refresh_token)"
                .to_owned(),
        );
    };

    debug!("refreshing the spotify access token");

    let mut response = ureq::post(TOKEN_URL)
        .header(
            "Authorization",
            &format!("Basic {}", base64(&format!("{id}:{secret}"))),
        )
        .send_form([
            ("grant_type", "refresh_token"),
            ("refresh_token", refresh.as_str()),
        ])
        .map_err(|e| format!("token refresh failed: {e}"))?;

    let body = response
        .body_mut()
        .read_to_string()
        .map_err(|e| format!("unable to read the token response: {e}"))?;

    let json: Value =
        serde_json::from_str(&body).map_err(|e| format!("unparseable token response: {e}"))?;

    let token = json["access_token"]
        .as_str()
        .ok_or("no access_token in the token response")?;

    let seconds = json["expires_in"].as_i64().unwrap_or(3600);
    let expiry = Timestamp::now() + SignedDuration::from_secs(seconds) - EXPIRY_MARGIN;

    Ok((expiry, token.to_owned()))
}

/// Standard base64, for the one header that needs it.
///
/// Twenty lines against a dependency that would otherwise be pulled in for a
/// single call site.
fn base64(input: &str) -> String {
    const ALPHABET: &[u8; 64] = b"ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz0123456789+/";

    let bytes = input.as_bytes();
    let mut out = String::with_capacity(bytes.len().div_ceil(3) * 4);

    for chunk in bytes.chunks(3) {
        let b = [
            chunk[0],
            *chunk.get(1).unwrap_or(&0),
            *chunk.get(2).unwrap_or(&0),
        ];
        let n = u32::from_be_bytes([0, b[0], b[1], b[2]]);

        for i in 0..4 {
            if i <= chunk.len() {
                out.push(ALPHABET[(n >> (18 - i * 6)) as usize & 0x3f] as char);
            } else {
                out.push('=');
            }
        }
    }

    out
}

fn dbus(cmd: &str) {
    let member = format!("org.mpris.MediaPlayer2.Player.{cmd}");

    let result = process::spawn(
        "dbus-send",
        &[
            "--print-reply",
            "--dest=org.mpris.MediaPlayer2.spotify",
            "/org/mpris/MediaPlayer2",
            &member,
        ],
    );

    if let Err(e) = result {
        error!(%e, cmd, "unable to send the dbus message");
    }
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
    fn base64_matches_known_values() {
        // RFC 4648 test vectors, which cover every padding case.
        assert_eq!(base64(""), "");
        assert_eq!(base64("f"), "Zg==");
        assert_eq!(base64("fo"), "Zm8=");
        assert_eq!(base64("foo"), "Zm9v");
        assert_eq!(base64("foob"), "Zm9vYg==");
        assert_eq!(base64("fooba"), "Zm9vYmE=");
        assert_eq!(base64("foobar"), "Zm9vYmFy");
    }

    #[test]
    fn base64_encodes_a_credential_pair() {
        assert_eq!(base64("id:secret"), "aWQ6c2VjcmV0");
    }

    #[test]
    fn a_track_reads_as_a_sentence() {
        let track = Track {
            id: "abc".to_owned(),
            name: "Blue Monday".to_owned(),
            artists: vec!["New Order".to_owned()],
        };

        assert_eq!(track.to_string(), "Blue Monday by New Order");
    }

    #[test]
    fn several_artists_are_listed() {
        let track = Track {
            id: "abc".to_owned(),
            name: "Under Pressure".to_owned(),
            artists: vec!["Queen".to_owned(), "David Bowie".to_owned()],
        };

        assert_eq!(track.to_string(), "Under Pressure by Queen, David Bowie");
    }
}
