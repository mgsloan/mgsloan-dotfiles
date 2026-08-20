//! A penrose window manager, porting ~/env/src/xmonad.hs. See design.md.
//!
//! `clippy.toml` lists the APIs that compile here but are wrong in a window
//! manager — penrose's spawn helpers, anything that waits for a child, and
//! anything that sleeps on the event loop. This makes those a hard error, so
//! `cargo clippy` has to pass before `M-q` is worth pressing.
#![deny(clippy::disallowed_methods)]

// One backend per binary: `Conn` is one type, and every binding, hook and query
// here names it. Cargo features are additive, so this is the only way to say
// that these two are not.
#[cfg(all(feature = "x11", feature = "river"))]
compile_error!(
    "the x11 and river features are alternatives, not additions: build with \
     --no-default-features --features river for the river window manager"
);
#[cfg(not(any(feature = "x11", feature = "river")))]
compile_error!("one of the x11 or river features has to be enabled");

mod actions;
mod bindings;
mod conn;
mod env;
// EWMH is X11's, and river has no property store for any of it to live in: no
// _NET_ACTIVE_WINDOW for a browser to send, so nothing to refuse. See
// vendor/penrose/river-design.md §9.
#[cfg(feature = "x11")]
mod ewmh;
mod layout;
mod layouts;
mod manage;
mod menu;
mod notify;
mod process;
mod programs;
mod startup;
mod urgency;

use penrose::{
    Result,
    builtin::layout::Monocle,
    core::{Config, State, WindowManager, bindings::parse_keybindings},
    stack,
};
use tracing_subscriber::{EnvFilter, prelude::*};

use crate::layout::TallWheel;

pub use crate::conn::Conn;

/// Workspace tags: 1-9 then 0, matching `workspaceNames`.
pub const TAGS: [&str; 10] = ["1", "2", "3", "4", "5", "6", "7", "8", "9", "0"];

/// Is this a Wayland build?
///
/// The backend a build picks is a compile-time fact, but reading it as a `bool`
/// rather than as `#[cfg]` keeps both sides compiled and type-checked in either
/// build -- and keeps their tests running there too.
pub const WAYLAND: bool = cfg!(feature = "river");

pub const TERMINAL: &str = "ghostty";

/// The `app_id` of each terminal `startup.rs` opens for a job of its own, which
/// is what `manage.rs` places it by.
///
/// Reverse-DNS because ghostty's `--class` has to be a valid GTK application ID
/// -- at least one dot, no leading digit -- where alacritty's took any string
/// at all. An invalid one is not refused: it is dropped, the window arrives as
/// the default `com.mitchellh.ghostty`, and every rule in `manage.rs` misses
/// it. Under ghostty's own name rather than a namespace of this config's, to
/// sit alongside the `com.mitchellh.ghostty.ws-*` scheme in
/// `env/workspaces-design.md`.
///
/// The tmux session inside each of these is the last segment, so `bt` stays
/// `bt` to `tmux send-keys` and to anyone typing it.
pub const CLASS_SYSLOG: &str = "com.mitchellh.ghostty.syslog";
pub const CLASS_ERRLOG: &str = "com.mitchellh.ghostty.errlog";
pub const CLASS_BT: &str = "com.mitchellh.ghostty.bt";
pub const CLASS_WIFI: &str = "com.mitchellh.ghostty.wifi";

/// What a terminal opened by hand runs, matching the xmonad config's
/// `terminalArgs`: a terminal here is always a tmux session, so closing the
/// window does not lose the shell.
pub const TERMINAL_ARGS: [&str; 2] = ["-e", "tmux"];

/// Exit codes read by the supervisor script (scripts/run-penrose.sh).
///
/// gdm-x-session runs the window manager as the session script, so the loop
/// around it has to be told which exits mean "come back" and which mean "the
/// session is over".
pub const EXIT_RESTART: i32 = 0;
pub const EXIT_LOGOUT: i32 = 42;

fn main() -> Result<()> {
    // Logging goes to stdout; the supervisor script pipes it into the journal.
    tracing_subscriber::registry()
        .with(tracing_subscriber::fmt::layer())
        .with(EnvFilter::try_from_default_env().unwrap_or_else(|_| EnvFilter::new("info")))
        .init();

    // Before the connection, and before `run` sets SIGCHLD to SIG_IGN: the
    // systemd-cat check inside this waits for a child, which stops working
    // after that point (see process.rs).
    env::init();

    let conn = conn::connect()?;

    let config = Config {
        // No borders, as in the xmonad config, which makes the border colours
        // in Config::default() inert.
        border_width: 0,
        // Penrose warps the pointer to the focused window on a focus change
        // when this is set, which is what the config's `warpMid` did by hand.
        focus_follow_mouse: true,
        // Screen 0 is the rightmost monitor, as in the xmonad config.
        screen_order: conn::right_to_left,
        default_layouts: stack!(TallWheel::boxed_default(), Monocle::boxed()),
        tags: TAGS.iter().map(|t| (*t).to_owned()).collect(),
        manage_hook: Some(manage::hooks()),
        startup_hook: Some(startup::hook()),
        #[cfg(feature = "x11")]
        event_hook: Some(ewmh::hook()),
        // Two jobs, one hook: `StateHook::then_boxed` needs `Sized` and so
        // cannot chain two trait objects, and a closure that calls both is
        // less machinery than making them chainable would be.
        refresh_hook: Some(Box::new(|state: &mut State<Conn>, _: &mut Conn| {
            // Clears the urgency hint on whatever just took focus.
            urgency::on_refresh(state);
            // Notes the layout the next restart would otherwise lose.
            layouts::on_refresh(state);

            Ok(())
        })),
        ..Config::default()
    };

    // The hooks that write _NET_WM_DESKTOP and friends, which is also what makes a restart able
    // to read the tags back. River restarts through its own hot swap instead (§7).
    #[cfg(feature = "x11")]
    let config = penrose::extensions::hooks::add_ewmh_hooks(config);

    // Keysyms rather than keycodes, resolved against the server when they are grabbed: no
    // xmodmap subprocess, and a binding on a keysym which lives on two keys now grabs both.
    let key_bindings = parse_keybindings(bindings::raw_key_bindings()).into_result()?;

    WindowManager::new(config, key_bindings, bindings::mouse_bindings(), conn)?.run()
}
