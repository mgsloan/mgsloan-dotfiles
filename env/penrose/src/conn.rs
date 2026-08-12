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
    Ok(penrose::river::RiverConn::new()?.restore_tags(take_saved_tags()))
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

/// Where the tags of each window are written across a river restart.
///
/// The runtime dir rather than a state dir: this is only meaningful within the
/// session that wrote it, and is cleaned up with it.
#[cfg(feature = "river")]
fn tag_file() -> std::path::PathBuf {
    let dir = std::env::var("XDG_RUNTIME_DIR").unwrap_or_else(|_| "/tmp".to_string());

    std::path::PathBuf::from(dir).join("penrose-river-tags")
}

/// Write down which window is on which tag, for the next generation to read.
///
/// X11 needs no counterpart: `_NET_WM_DESKTOP` is on the windows themselves, so
/// a restart reads the tags back off the X server. River has no property store,
/// and its window identifiers are what stands in -- stable across a restart and
/// never reused, because they belong to the window rather than to a connection.
#[cfg(feature = "river")]
pub fn save_tags(state: &penrose::core::State<Conn>, conn: &Conn) {
    use std::fmt::Write as _;

    let mut out = String::new();
    let tagged: Vec<_> = state
        .client_set
        .clients()
        .filter_map(|&id| Some((id, state.client_set.tag_for_client(&id)?)))
        .collect();

    for (id, tag) in tagged {
        if let Some(identifier) = conn.window_identifier(id) {
            let _ = writeln!(out, "{identifier}\t{tag}");
        }
    }

    if let Err(e) = std::fs::write(tag_file(), out) {
        tracing::error!(%e, "unable to write the tag handover file");
    }
}

/// Read back what the previous generation wrote, and consume it.
///
/// Removed as it is read: a stale file would put windows from a session ago on
/// tags they have since been moved off, and the identifiers in it name windows
/// that no longer exist.
#[cfg(feature = "river")]
fn take_saved_tags() -> std::collections::HashMap<String, String> {
    let path = tag_file();
    let contents = std::fs::read_to_string(&path).unwrap_or_default();
    let _ = std::fs::remove_file(&path);

    contents
        .lines()
        .filter_map(|l| l.split_once('\t'))
        .map(|(id, tag)| (id.to_string(), tag.to_string()))
        .collect()
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
        assert_eq!(ordered(vec![laptop, left, right]), vec![right, laptop, left]);
    }

    #[test]
    fn stacked_screens_order_top_to_bottom() {
        let top = Rect::new(0, 0, 1920, 1080);
        let bottom = Rect::new(0, 1080, 1920, 1080);

        assert_eq!(ordered(vec![top, bottom]), vec![bottom, top]);
    }
}
