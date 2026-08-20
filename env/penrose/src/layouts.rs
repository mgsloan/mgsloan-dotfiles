//! Per-workspace layout state, which a restart would otherwise drop.
//!
//! `M-q` exits the process and the supervisor relaunches it, so anything the
//! window manager held in memory is gone. Tags and focus survive because they
//! are written down somewhere outside it -- `_NET_WM_DESKTOP` on the X server,
//! the handover file under river (`conn.rs`). Each workspace's layout, and
//! `TallWheel`'s `ratio` and `max_main`, had nowhere to go and reset to the
//! configured defaults on every rebuild.
//!
//! The obstacle was never serialization. `Workspace` holds a `LayoutStack` of
//! `Box<dyn Layout>`, which cannot be serialized -- but nothing here needs to
//! be: a name and two numbers is the whole of the state, and `Workspace`
//! already exposes `layout_name` and `set_layout_by_name` for the name. The two
//! numbers travel by message (see `layout::ReportParams`). So this is the same
//! trade `toggles.rs` made: write it down rather than derive it from the world.
//!
//! Deriving it *was* the alternative, since a window's position on screen is
//! the old layout's output: count the windows sharing the leftmost `x` for
//! `max_main`, take the split point over the width for `ratio`. It reads the
//! busy workspaces exactly and the quiet ones not at all --
//! `TallWheel::single_pane` gives one window the whole screen, which is
//! pixel-identical to `Monocle`, and an empty workspace says nothing whatever.
//! Guessing wrong in the common case, silently, is worse than a file.
//!
//! == Restart only
//!
//! A new session starts from the configured defaults, as it did before this
//! existed and as xmonad does. The file is therefore cleared at login rather
//! than read: left in place, its first reader would be the first `M-q` of the
//! new session, putting back layouts from the session before last.

use std::{
    cell::RefCell,
    collections::HashMap,
    rc::Rc,
    sync::Arc,
    time::{Duration, Instant},
};

use penrose::{WinId, core::State, pure::Workspace};
use serde::{Deserialize, Serialize};
use tracing::{error, info, warn};

use crate::{
    Conn, env,
    layout::{ReportParams, SetParams, TallWheelParams},
};

/// The shortest gap between two writes of the file.
///
/// The recording hook runs on every refresh, which is every focus change, every
/// window mapped and every message sent to a layout -- and `ExpandMain` on a
/// held key is a stream of them. One write every couple of seconds keeps the
/// file within a couple of seconds of the truth for a cost that does not scale
/// with how hard the keyboard is being used.
///
/// It costs nothing at the moment that matters: [flush] writes unconditionally
/// on the way into a restart, so the rate limit can never be what loses the
/// last adjustment before `M-q`.
const WRITE_INTERVAL: Duration = Duration::from_secs(2);

/// One workspace's layout, in the form it goes to disk in.
#[derive(Debug, Clone, PartialEq, Serialize, Deserialize)]
struct WorkspaceLayout {
    /// `Layout::name()`, which is what `set_layout_by_name` matches on.
    name: String,
    /// Absent for a layout that carries no parameters, which here is `Monocle`
    /// (whose `name()` is "Mono").
    #[serde(default, skip_serializing_if = "Option::is_none")]
    params: Option<TallWheelParams>,
}

/// Tag to layout, as last seen, plus what the rate limit needs to know.
#[derive(Debug, Default)]
struct Layouts {
    seen: HashMap<String, WorkspaceLayout>,
    /// Set when `seen` has moved on from what is in the file.
    dirty: bool,
    /// When the file was last written. `None` until the first write, so the
    /// first change after a start is recorded immediately.
    written: Option<Instant>,
}

impl Layouts {
    /// Whether there is something to write and the rate limit allows it.
    ///
    /// Takes `now` rather than reading the clock so that the rule can be tested
    /// without a test having to wait out [WRITE_INTERVAL].
    fn may_write(&self, now: Instant) -> bool {
        self.dirty
            && self
                .written
                .is_none_or(|written| now.duration_since(written) >= WRITE_INTERVAL)
    }

    /// Note that the file now matches `seen`.
    fn wrote(&mut self, now: Instant) {
        self.dirty = false;
        self.written = Some(now);
    }
}

/// Restore the stored layouts, or clear them for a new session.
///
/// Mirrors `toggles::startup`, including the `restarted` argument, which here
/// decides between the two entirely.
pub fn startup(state: &mut State<Conn>, restarted: bool) {
    if !restarted {
        clear();
        return;
    }

    let stored = load();

    for (tag, layout) in &stored {
        let Some(ws) = state.client_set.workspace_mut(tag) else {
            // A tag that is no longer configured. Not worth a warning: editing
            // TAGS is how it happens, and the entry dies with the next write.
            continue;
        };

        ws.set_layout_by_name(&layout.name);

        if let Some(params) = layout.params {
            ws.handle_message(SetParams(params));
        }
    }

    info!(workspaces = stored.len(), "restored layouts");

    // Seeded so that the first refresh compares against what was restored and
    // finds nothing to do, rather than rewriting an identical file.
    get(state).borrow_mut().seen = stored;
}

/// Record the current workspace's layout, rate limited. Called on every refresh.
///
/// Only the current one: every binding that changes a layout acts on the
/// current workspace, so nothing else can have moved since the last refresh.
pub fn on_refresh(state: &mut State<Conn>) {
    let tag = state.client_set.current_tag().to_owned();
    let current = read(state.client_set.current_workspace_mut());

    let layouts = get(state);
    let mut layouts = layouts.borrow_mut();

    if layouts.seen.get(&tag) != Some(&current) {
        layouts.seen.insert(tag, current);
        layouts.dirty = true;
    }

    let now = Instant::now();

    if !layouts.may_write(now) {
        return;
    }

    save(&layouts.seen);
    layouts.wrote(now);
}

/// Write the file if anything is outstanding, whatever the rate limit says.
///
/// Called on the way into a restart, where the alternative is losing whichever
/// adjustments happened in the last couple of seconds -- which, since reaching
/// for `M-q` right after changing something is the ordinary way to use it, is
/// the case the whole feature is for.
pub fn flush(state: &mut State<Conn>) {
    let layouts = get(state);
    let mut layouts = layouts.borrow_mut();

    if layouts.dirty {
        save(&layouts.seen);
        layouts.wrote(Instant::now());
    }
}

/// What a workspace's layout is, as far as this module can see it.
fn read(ws: &mut Workspace<WinId>) -> WorkspaceLayout {
    let cell = Rc::new(RefCell::new(None));

    // Left empty by any layout that does not recognise the message, which is
    // the answer for `Monocle` rather than a failure.
    ws.handle_message(ReportParams(Rc::clone(&cell)));

    WorkspaceLayout {
        name: ws.layout_name(),
        params: *cell.borrow(),
    }
}

fn get(state: &mut State<Conn>) -> Arc<RefCell<Layouts>> {
    state.extension_or_default::<Layouts>()
}

fn path() -> String {
    match std::env::var("XDG_STATE_HOME") {
        Ok(dir) if !dir.is_empty() => format!("{dir}/penrose/layouts.json"),
        _ => env::get().home(".local/state/penrose/layouts.json"),
    }
}

fn clear() {
    let path = path();

    match std::fs::remove_file(&path) {
        Ok(()) => info!(path, "cleared stored layouts for a new session"),
        Err(e) if e.kind() == std::io::ErrorKind::NotFound => {}
        Err(e) => warn!(%e, path, "unable to clear stored layouts"),
    }
}

fn load() -> HashMap<String, WorkspaceLayout> {
    let path = path();

    let contents = match std::fs::read_to_string(&path) {
        Ok(contents) => contents,
        Err(e) if e.kind() == std::io::ErrorKind::NotFound => return HashMap::new(),
        Err(e) => {
            warn!(%e, path, "unable to read layouts, using defaults");
            return HashMap::new();
        }
    };

    match serde_json::from_str(&contents) {
        Ok(layouts) => layouts,
        Err(e) => {
            warn!(%e, path, "unable to parse layouts, using defaults");
            HashMap::new()
        }
    }
}

fn save(layouts: &HashMap<String, WorkspaceLayout>) {
    let path = path();
    let tmp = format!("{path}.tmp");

    let json = match serde_json::to_string_pretty(layouts) {
        Ok(json) => json,
        Err(e) => {
            error!(%e, "unable to serialize layouts");
            return;
        }
    };

    if let Some(dir) = std::path::Path::new(&path).parent()
        && let Err(e) = std::fs::create_dir_all(dir)
    {
        error!(%e, ?dir, "unable to create the state directory");
        return;
    }

    // Written and renamed rather than truncated in place, so a crash mid-write
    // cannot leave half a file for the next start to fail to parse.
    if let Err(e) = std::fs::write(&tmp, json) {
        error!(%e, tmp, "unable to write layouts");
        return;
    }

    if let Err(e) = std::fs::rename(&tmp, &path) {
        error!(%e, path, "unable to replace layouts");
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    fn tall(max_main: u32, ratio: f32) -> WorkspaceLayout {
        WorkspaceLayout {
            name: "TallWheel".to_owned(),
            params: Some(TallWheelParams { max_main, ratio }),
        }
    }

    /// The file is the whole point, so the shape of it is worth pinning: a
    /// layout with parameters keeps them, and one without writes no null.
    #[test]
    fn a_layout_round_trips_through_json() {
        let mut stored = HashMap::new();
        stored.insert("1".to_owned(), tall(2, 0.7));
        stored.insert(
            "9".to_owned(),
            WorkspaceLayout {
                name: "Mono".to_owned(),
                params: None,
            },
        );

        let json = serde_json::to_string(&stored).expect("to serialize");
        assert!(!json.contains("null"), "{json}");

        let back: HashMap<String, WorkspaceLayout> =
            serde_json::from_str(&json).expect("to deserialize");
        assert_eq!(back, stored);
    }

    /// A file written by a build that did not know about `params` still loads,
    /// which is what `serde(default)` on the field is for.
    #[test]
    fn a_layout_without_params_loads() {
        let back: WorkspaceLayout =
            serde_json::from_str(r#"{"name":"Mono"}"#).expect("to deserialize");

        assert_eq!(back.params, None);
    }

    /// Nothing outstanding, nothing written -- whatever the clock says.
    #[test]
    fn a_clean_state_is_not_written() {
        let layouts = Layouts::default();

        assert!(!layouts.may_write(Instant::now()));
    }

    /// The first change after a start does not wait out the interval: there is
    /// no previous write for it to be too close to.
    #[test]
    fn the_first_change_is_written_immediately() {
        let layouts = Layouts {
            dirty: true,
            ..Default::default()
        };

        assert!(layouts.may_write(Instant::now()));
    }

    /// The rate limit itself: a second change inside the window waits, and the
    /// same change is still outstanding once the window passes. This is what
    /// keeps a held `ExpandMain` from writing the file once per repeat.
    #[test]
    fn a_change_inside_the_interval_waits_for_it() {
        let now = Instant::now();
        let layouts = Layouts {
            dirty: true,
            written: Some(now),
            ..Default::default()
        };

        assert!(!layouts.may_write(now + WRITE_INTERVAL - Duration::from_millis(1)));
        assert!(layouts.may_write(now + WRITE_INTERVAL));
    }

    /// After a write there is nothing outstanding, so the next refresh does not
    /// write again just because the interval has passed.
    #[test]
    fn writing_clears_what_was_outstanding() {
        let now = Instant::now();
        let mut layouts = Layouts {
            dirty: true,
            ..Default::default()
        };

        layouts.wrote(now);

        assert!(!layouts.may_write(now + WRITE_INTERVAL * 10));
    }
}
