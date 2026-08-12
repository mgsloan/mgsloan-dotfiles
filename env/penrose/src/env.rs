//! The shared environment: paths, capability checks, and secrets.
//!
//! The xmonad config carries an `Env` through `ReaderT` in both of its monads.
//! Here it is a plain struct behind an `Arc`, reachable from a `static`, because
//! handlers get `&mut State` but the threads they spawn cannot — so anything a
//! thread needs has to live somewhere it can own a handle to.
//!
//! Read-mostly by design. State the event loop mutates belongs in penrose's
//! extension state instead; this is what is loaded once and then only consulted.

use std::{
    collections::HashMap,
    process::{Command, Stdio},
    sync::{Arc, Mutex, OnceLock},
};

use tracing::{error, info};

static ENV: OnceLock<Arc<Env>> = OnceLock::new();

#[derive(Debug)]
pub struct Env {
    /// `$HOME`, the root of everything else this config reaches for.
    pub home: String,
    /// Whether `systemd-cat` accepts the flags process logging depends on.
    pub systemd_cat_works: bool,
    /// Bluetooth device IDs, absent on a machine that has never paired them.
    ///
    /// Read at startup rather than on use, so that a missing file is reported
    /// once in the log instead of silently turning a keypress into a no-op.
    #[allow(dead_code, reason = "read by the bluetooth actions, design.md §19")]
    pub headphones_uuid: Option<String>,
    #[allow(dead_code, reason = "read by the bluetooth actions, design.md §19")]
    pub receiver_uuid: Option<String>,
    /// Spotify Web API credentials, absent until they are put in place by hand.
    pub spotify_client_id: Option<String>,
    pub spotify_client_secret: Option<String>,
    pub spotify_refresh_token: Option<String>,
    /// Whether to drive Spotify over its Web API rather than dbus, for
    /// controlling playback on a device that is not this machine.
    pub spotify_no_dbus: bool,
    /// Environment variables added to everything spawned from here.
    ///
    /// The alternative is `std::env::set_var`, which is `unsafe` in Rust 2024
    /// for exactly the reason this config would hit: threads reading the
    /// environment while another sets it. Owning the overrides instead of
    /// mutating the process is both safe and easier to reason about — what a
    /// program inherits is a value, not a side effect.
    overrides: Mutex<HashMap<String, String>>,
}

/// Build the environment and publish it.
///
/// Called from `main` rather than the startup hook, deliberately: the
/// `systemd-cat` check below wants to wait for a child process, and
/// `WindowManager::run` sets `SIGCHLD` to `SIG_IGN`, after which nothing can
/// (see `process.rs`).
pub fn init() -> Arc<Env> {
    let home = std::env::var("HOME").unwrap_or_else(|_| "/root".to_owned());

    let env = Arc::new(Env {
        systemd_cat_works: check_systemd_cat(),
        headphones_uuid: read_untracked(&home, "headphones.uuid"),
        receiver_uuid: read_untracked(&home, "receiver.uuid"),
        spotify_client_id: read_untracked(&home, "spotify.client_id"),
        spotify_client_secret: read_untracked(&home, "spotify.client_secret"),
        spotify_refresh_token: read_untracked(&home, "spotify.refresh_token"),
        spotify_no_dbus: std::env::var("SPOTIFY_NO_DBUS").as_deref() == Ok("true"),
        overrides: Mutex::new(HashMap::new()),
        home,
    });

    let _ = ENV.set(Arc::clone(&env));

    env
}

/// The environment, for code that is not holding a handle already.
///
/// Panics if called before `init`, which would be a wiring mistake rather than
/// a runtime condition: `main` initializes before anything else can run.
pub fn get() -> &'static Arc<Env> {
    ENV.get().expect("env::init to have been called by main")
}

impl Env {
    /// Path to something under `$HOME`.
    pub fn home(&self, rel: &str) -> String {
        format!("{}/{rel}", self.home)
    }

    /// Path to a script in `~/env/scripts`, shared with the xmonad config.
    pub fn script(&self, name: &str) -> String {
        self.home(&format!("env/scripts/{name}"))
    }

    /// Path to a script in this project's own `scripts/`.
    ///
    /// Distinct from [Env::script] because the two directories are not the same
    /// and the difference is invisible at the call site — `M-q` pointed at the
    /// wrong one and reported "failed to run rebuild script" for every restart
    /// it was ever asked to do.
    pub fn penrose_script(&self, name: &str) -> String {
        self.home(&format!("env/penrose/scripts/{name}"))
    }

    /// Set an environment variable for everything spawned from now on.
    ///
    /// Programs already running keep what they were started with, which is the
    /// same limitation the xmonad config had and the reason these are worth
    /// having as menu entries rather than settings.
    pub fn set_override(&self, key: &str, value: &str) {
        match self.overrides.lock() {
            Ok(mut overrides) => {
                overrides.insert(key.to_owned(), value.to_owned());
                info!(key, value, "environment override");
            }
            Err(e) => error!(%e, "poisoned override lock"),
        }
    }

    /// The overrides, for `process.rs` to apply.
    pub fn overrides(&self) -> Vec<(String, String)> {
        match self.overrides.lock() {
            Ok(overrides) => overrides.clone().into_iter().collect(),
            Err(e) => {
                error!(%e, "poisoned override lock");
                Vec::new()
            }
        }
    }
}

/// First line of a file in `~/env/untracked`, if it is there.
///
/// Absence is normal — a fresh machine has none of these — so it is logged
/// rather than treated as an error. The alternative is a binding that does
/// nothing for no visible reason.
fn read_untracked(home: &str, name: &str) -> Option<String> {
    let path = format!("{home}/env/untracked/{name}");

    match std::fs::read_to_string(&path) {
        Ok(contents) => match contents.lines().next().map(str::trim) {
            Some(line) if !line.is_empty() => Some(line.to_owned()),
            _ => {
                error!(path, "file is empty");
                None
            }
        },
        Err(e) => {
            error!(%e, path, "unable to read, so anything needing it will not work");
            None
        }
    }
}

/// Does `systemd-cat` support the flags `process::spawn` wants to pass?
///
/// `--stderr-priority` comes from a personal systemd patch, so this cannot be
/// assumed. Failing it is not fatal: logging degrades to whatever the process
/// writes to the window manager's own stdout.
fn check_systemd_cat() -> bool {
    // Waiting works here and nowhere else: `init` runs from `main`, before
    // `WindowManager::run` sets SIGCHLD to SIG_IGN. Using process::status
    // instead would be circular, since it consults the flag this produces.
    #[allow(
        clippy::disallowed_methods,
        reason = "runs before the signal disposition changes"
    )]
    let status = Command::new("systemd-cat")
        .args(crate::process::SYSTEMD_CAT_ARGS)
        .args(["-t", "penrose-sanity-check", "true"])
        .stdin(Stdio::null())
        .status();

    match status {
        Ok(s) if s.success() => {
            info!("systemd-cat sanity check passed");
            true
        }
        Ok(s) => {
            error!(code = ?s.code(), "systemd-cat rejected its arguments, logging process output directly instead");
            false
        }
        Err(e) => {
            error!(%e, "unable to run systemd-cat, logging process output directly instead");
            false
        }
    }
}
