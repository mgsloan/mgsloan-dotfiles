//! The connection this window manager is built against, for whichever backend.
//!
//! There is nothing here but the choice of backend and the screen ordering,
//! which is the point: what used to be a `RustConn` newtype delegating thirty
//! methods to reorder screens is now a comparator penrose applies itself, and
//! the same one works on either backend.

use std::cmp::Ordering;

use penrose::{Result, core::left_to_right, pure::geometry::Rect};

/// The connection type the whole program is built against.
///
/// One type rather than a generic parameter: every binding, hook and query in
/// this config names it, and a window manager runs one backend at a time.
#[cfg(feature = "x11")]
pub type Conn = penrose::x11rb::RustConn;

/// See the non-river alias above.
///
/// The `not(x11)` is not a third configuration: enabling both is refused in
/// main.rs, and this keeps the two aliases from colliding so that the refusal is
/// the error reported rather than a pile of name resolution noise.
#[cfg(all(feature = "river", not(feature = "x11")))]
pub type Conn = penrose::river::RiverConn;

/// Connect to whichever the build is for.
#[cfg(feature = "x11")]
pub fn connect() -> Result<Conn> {
    penrose::x11rb::RustConn::new()
}

/// See the non-river constructor above.
#[cfg(all(feature = "river", not(feature = "x11")))]
pub fn connect() -> Result<Conn> {
    let saved = take_saved_state();

    penrose::river::RiverConn::new()?
        .restore_tags(saved.tags)
        .restore_focus(saved.focus)
        .allow_while_locked(crate::bindings::live_while_locked())
}

/// Right-to-left, top-to-bottom: `Config::screen_order`, so that `M-u`/`M-i`/
/// `M-o` land on the same monitors whichever order the backend enumerates them
/// in.
///
/// Mirrors the `screenOrder` comparator in the xmonad config, which compared
/// `(x2, y2)` against `(x1, y1)`: screen 0 is the rightmost monitor.
pub fn right_to_left(a: &Rect, b: &Rect) -> Ordering {
    left_to_right(a, b).reverse()
}

/// Where what a river restart has to carry across is written.
///
/// The runtime dir rather than a state dir: this is only meaningful within the
/// session that wrote it, and is cleaned up with it.
#[cfg(feature = "river")]
fn handover_file() -> std::path::PathBuf {
    let dir = std::env::var("XDG_RUNTIME_DIR").unwrap_or_else(|_| "/tmp".to_string());

    std::path::PathBuf::from(dir).join("penrose-river-tags")
}

/// What the previous generation left for this one.
#[cfg(feature = "river")]
#[derive(Default)]
pub struct Handover {
    /// Which tag each window was on, keyed by river's window identifier.
    pub tags: std::collections::HashMap<String, String>,
    /// The identifier of the window that had focus, which decides the workspace
    /// the session comes back up on.
    pub focus: Option<String>,
}

/// Write down what a restart would otherwise lose, for the next generation.
///
/// X11 needs no counterpart: `_NET_WM_DESKTOP` and `_NET_ACTIVE_WINDOW` live on
/// the X server, so a restart reads both back off it. River has no property
/// store, and its window identifiers are what stands in -- stable across a
/// window manager restart and never reused, because they belong to the window
/// rather than to a connection.
///
/// One line per window, `identifier<TAB>tag`, and a third field on the one that
/// had focus.
#[cfg(feature = "river")]
pub fn save_state(state: &penrose::core::State<Conn>, conn: &Conn) {
    use std::fmt::Write as _;

    let focused = state.client_set.current_client().copied();
    let mut out = String::new();
    let tagged: Vec<_> = state
        .client_set
        .clients()
        .filter_map(|&id| Some((id, state.client_set.tag_for_client(&id)?)))
        .collect();

    for (id, tag) in tagged {
        if let Some(identifier) = conn.window_identifier(id) {
            let focus = if Some(id) == focused { "\tfocus" } else { "" };
            let _ = writeln!(out, "{identifier}\t{tag}{focus}");
        }
    }

    if let Err(e) = std::fs::write(handover_file(), out) {
        tracing::error!(%e, "unable to write the restart handover file");
    }
}

/// Read back what the previous generation wrote, and consume it.
///
/// Removed as it is read: a stale file would put windows from a session ago on
/// tags they have since been moved off, and the identifiers in it name windows
/// that no longer exist.
#[cfg(feature = "river")]
fn take_saved_state() -> Handover {
    let path = handover_file();
    let contents = std::fs::read_to_string(&path).unwrap_or_default();
    let _ = std::fs::remove_file(&path);

    parse_handover(&contents)
}

/// Split out from [take_saved_state] so that the format has a test.
#[cfg(feature = "river")]
fn parse_handover(contents: &str) -> Handover {
    let mut saved = Handover::default();

    for line in contents.lines() {
        let mut fields = line.split('\t');
        let (Some(identifier), Some(tag)) = (fields.next(), fields.next()) else {
            continue;
        };

        if fields.next() == Some("focus") {
            saved.focus = Some(identifier.to_string());
        }

        saved.tags.insert(identifier.to_string(), tag.to_string());
    }

    saved
}

#[cfg(test)]
mod tests {
    use super::*;

    /// The same expectations the `PhysConn` this replaced was tested against.
    fn ordered(mut rects: Vec<Rect>) -> Vec<Rect> {
        rects.sort_by(right_to_left as penrose::core::ScreenComparator);
        rects
    }

    #[test]
    fn screens_are_ordered_right_to_left() {
        let laptop = Rect::new(0, 0, 1920, 1080);
        let left = Rect::new(-2560, 0, 2560, 1440);
        let right = Rect::new(1920, 0, 2560, 1440);

        // index 0 is the rightmost screen, matching `M-o` in the xmonad config
        assert_eq!(
            ordered(vec![laptop, left, right]),
            vec![right, laptop, left]
        );
    }

    #[test]
    fn stacked_screens_order_top_to_bottom() {
        let top = Rect::new(0, 0, 1920, 1080);
        let bottom = Rect::new(0, 1080, 1920, 1080);

        assert_eq!(ordered(vec![top, bottom]), vec![bottom, top]);
    }

    /// The window that had focus is what decides the workspace a restart comes
    /// back up on, and it is one optional field on one line: worth a test, since
    /// getting it wrong is invisible until the next restart lands somewhere odd.
    #[cfg(feature = "river")]
    #[test]
    fn the_handover_carries_tags_and_the_focused_window() {
        let saved = parse_handover("a\t1\nb\t8\tfocus\nc\t2\n");

        assert_eq!(saved.focus.as_deref(), Some("b"));
        assert_eq!(saved.tags.get("a").map(String::as_str), Some("1"));
        assert_eq!(saved.tags.get("b").map(String::as_str), Some("8"));
        assert_eq!(saved.tags.len(), 3);
    }

    /// Nothing focused is an ordinary state: every workspace can be empty.
    #[cfg(feature = "river")]
    #[test]
    fn a_handover_without_a_focused_window_parses() {
        let saved = parse_handover("a\t1\n");

        assert_eq!(saved.focus, None);
        assert_eq!(saved.tags.len(), 1);
    }
}
