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
        .restore_order(saved.order)
        .restore_focus(saved.focus)
        .restore_screens(saved.screens, saved.focused_screen)
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

/// The first field of a line describing a screen, and of one describing a window.
///
/// A line used to start with a window identifier, which is why a line that starts with neither of
/// these is still read as a window: see [parse_handover].
#[cfg(feature = "river")]
const SCREEN: &str = "screen";

/// See [SCREEN].
#[cfg(feature = "river")]
const WINDOW: &str = "window";

/// What the previous generation left for this one.
#[cfg(feature = "river")]
#[derive(Default)]
pub struct Handover {
    /// Which tag each window was on, keyed by river's window identifier.
    pub tags: std::collections::HashMap<String, String>,
    /// Where in its workspace each window sat, keyed the same way.
    ///
    /// Tag membership on its own is not enough to come back looking the same.
    /// A `Stack`'s first element is the main one "regardless of focus", so
    /// whichever window is adopted first takes the master pane -- and adoption
    /// runs in river's window id order, which has nothing to do with the order
    /// the user arranged. Same layout, same ratio, different window in the big
    /// pane.
    pub order: std::collections::HashMap<String, usize>,
    /// The identifier of the window that had focus.
    pub focus: Option<String>,
    /// Which tag each screen was showing, by screen index, ascending.
    ///
    /// This is what decides the workspace the session comes back up on. The
    /// focused window was doing that job on its own for a while, and could not:
    /// a workspace with nothing on it has no window to name it, so `M-q` from
    /// an empty workspace came back on tag 1. Multiple screens have the same
    /// problem one screen at a time -- one focused window says nothing about
    /// what the others were showing.
    pub screens: Vec<(usize, String)>,
    /// Which screen was current.
    pub focused_screen: Option<usize>,
}

/// The handover as of the last refresh, waiting for the exit that will write it.
///
/// A global because of where the two halves run. The state it describes is only
/// reachable from the window manager thread, and the exit that has to write it
/// happens on the rebuild thread `M-q` spawns -- which has no `State` and can
/// never be given one, since `State` is not `Send`. Serialising on one side and
/// writing on the other is what lets the file be written at the moment it is
/// true rather than at the moment the state was last in reach.
#[cfg(feature = "river")]
static PENDING: std::sync::Mutex<String> = std::sync::Mutex::new(String::new());

/// Serialise what a restart would otherwise lose. Called on every refresh.
///
/// Not written here. A refresh is every focus change and every window mapped,
/// and this file has exactly one reader at exactly one moment -- the next
/// generation, once this one has gone -- so the write belongs at the exit and
/// nowhere else. See [write_handover].
#[cfg(feature = "river")]
pub fn on_refresh(state: &penrose::core::State<Conn>, conn: &Conn) {
    let pending = handover_text(state, conn);

    *PENDING
        .lock()
        .unwrap_or_else(std::sync::PoisonError::into_inner) = pending;
}

/// Write down what a restart would otherwise lose, for the next generation.
///
/// Called from the restart path with the process about to exit, which is the
/// only moment the file can describe the session the next generation will find.
/// Writing it when `M-q` was pressed -- which is what this used to do --
/// describes the session as it was before the rebuild, and a rebuild is long
/// enough to switch workspace, open a window or move one in. It also left the
/// file behind when a rebuild failed, for some later crash-relaunch to read as
/// though it were current.
#[cfg(feature = "river")]
pub fn write_handover() {
    let pending = PENDING
        .lock()
        .unwrap_or_else(std::sync::PoisonError::into_inner);

    // There is always a screen, so there is always a screen line: empty means
    // the refresh hook has never run, and writing that would hand the next
    // generation a session with every window on the current workspace.
    if pending.is_empty() {
        tracing::error!("nothing to hand over: the refresh hook has not run");
        return;
    }

    if let Err(e) = std::fs::write(handover_file(), pending.as_str()) {
        tracing::error!(%e, "unable to write the restart handover file");
    }
}

/// The contents of the handover file, as of now.
///
/// X11 needs no counterpart: `_NET_WM_DESKTOP` and `_NET_ACTIVE_WINDOW` live on
/// the X server, so a restart reads both back off it. River has no property
/// store, and its window identifiers are what stands in -- stable across a
/// window manager restart and never reused, because they belong to the window
/// rather than to a connection.
///
/// One line per screen and one per window, each named by its first field:
///
/// ```text
/// screen<TAB>index<TAB>tag[<TAB>focus]
/// window<TAB>identifier<TAB>tag<TAB>index[<TAB>focus]
/// ```
#[cfg(feature = "river")]
fn handover_text(state: &penrose::core::State<Conn>, conn: &Conn) -> String {
    use std::fmt::Write as _;

    let focused = state.client_set.current_client().copied();
    let current_screen = state.client_set.current_screen().index();
    let mut out = String::new();

    // Which tag each screen was showing. Written down rather than left to be
    // inferred from the focused window, which is what this used to do and which
    // has nothing to say about an empty workspace or about a screen other than
    // the current one.
    for screen in state.client_set.screens() {
        let focus = if screen.index() == current_screen {
            "\tfocus"
        } else {
            ""
        };

        let _ = writeln!(
            out,
            "{SCREEN}\t{}\t{}{focus}",
            screen.index(),
            screen.workspace.tag()
        );
    }

    // By workspace rather than by client, because the index within one is the
    // point: `Workspace::clients` yields them in stack order, which is the
    // order the next generation has to put them back in.
    for ws in state.client_set.workspaces() {
        for (index, &id) in ws.clients().enumerate() {
            if let Some(identifier) = conn.window_identifier(id) {
                let focus = if Some(id) == focused { "\tfocus" } else { "" };
                let _ = writeln!(out, "{WINDOW}\t{identifier}\t{}\t{index}{focus}", ws.tag());
            }
        }
    }

    out
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
///
/// A line that names neither kind is a window line from before the names
/// existed, which starts with the identifier instead. That is the file the
/// generation running at the moment of an upgrade wrote, so it has exactly one
/// restart to survive -- but that restart is the one where every window is on a
/// tag the user chose.
#[cfg(feature = "river")]
fn parse_handover(contents: &str) -> Handover {
    let mut saved = Handover::default();

    for line in contents.lines() {
        let mut fields = line.split('\t');

        match fields.next() {
            Some(SCREEN) => parse_screen(&mut saved, fields),
            Some(WINDOW) => parse_window(&mut saved, fields),
            Some(identifier) => parse_window(&mut saved, std::iter::once(identifier).chain(fields)),
            None => (),
        }
    }

    // Ascending, because `restore_screens` applies them in the order given and
    // pulling a tag to a screen can take it off another one.
    saved.screens.sort_by_key(|(index, _)| *index);

    saved
}

/// `index<TAB>tag[<TAB>focus]`, the fields after the kind on a screen line.
#[cfg(feature = "river")]
fn parse_screen<'a>(saved: &mut Handover, mut fields: impl Iterator<Item = &'a str>) {
    let (Some(index), Some(tag)) = (fields.next(), fields.next()) else {
        return;
    };

    let Ok(index) = index.parse::<usize>() else {
        return;
    };

    if fields.any(|field| field == "focus") {
        saved.focused_screen = Some(index);
    }

    saved.screens.push((index, tag.to_string()));
}

/// `identifier<TAB>tag<TAB>index[<TAB>focus]`, the fields after the kind on a
/// window line.
///
/// The trailing fields are read by what they are rather than by where they are.
/// The index was added after the focus marker, so a file from before it has a
/// `focus` where this one has a number -- and reading by position would take
/// that as an order.
#[cfg(feature = "river")]
fn parse_window<'a>(saved: &mut Handover, mut fields: impl Iterator<Item = &'a str>) {
    let (Some(identifier), Some(tag)) = (fields.next(), fields.next()) else {
        return;
    };

    for field in fields {
        if field == "focus" {
            saved.focus = Some(identifier.to_string());
        } else if let Ok(index) = field.parse::<usize>() {
            saved.order.insert(identifier.to_string(), index);
        }
    }

    saved.tags.insert(identifier.to_string(), tag.to_string());
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

    /// The window that had focus, which is one optional field on one line:
    /// worth a test, since getting it wrong is invisible until the next restart
    /// lands somewhere odd.
    #[cfg(feature = "river")]
    #[test]
    fn the_handover_carries_tags_and_the_focused_window() {
        let saved = parse_handover("window\ta\t1\nwindow\tb\t8\tfocus\nwindow\tc\t2\n");

        assert_eq!(saved.focus.as_deref(), Some("b"));
        assert_eq!(saved.tags.get("a").map(String::as_str), Some("1"));
        assert_eq!(saved.tags.get("b").map(String::as_str), Some("8"));
        assert_eq!(saved.tags.len(), 3);
    }

    /// The order within a workspace, which is what keeps the master pane on the
    /// same window across a restart.
    #[cfg(feature = "river")]
    #[test]
    fn the_handover_carries_the_order_within_a_workspace() {
        let saved =
            parse_handover("window\ta\t1\t0\nwindow\tb\t1\t1\tfocus\nwindow\tc\t1\t2\n");

        assert_eq!(saved.order.get("a"), Some(&0));
        assert_eq!(saved.order.get("b"), Some(&1));
        assert_eq!(saved.order.get("c"), Some(&2));
        assert_eq!(saved.focus.as_deref(), Some("b"));
    }

    /// What each screen was showing, and which of them was current. This is the
    /// answer for an empty workspace, which no window line can name.
    #[cfg(feature = "river")]
    #[test]
    fn the_handover_carries_the_screens_and_the_current_one() {
        let saved = parse_handover("screen\t0\t2\nscreen\t1\t4\tfocus\nwindow\ta\t2\t0\n");

        assert_eq!(
            saved.screens,
            vec![(0, "2".to_owned()), (1, "4".to_owned())]
        );
        assert_eq!(saved.focused_screen, Some(1));
        // The empty workspace on screen 1 is the case: nothing was focused, and
        // the session still has to come back up on tag 4.
        assert_eq!(saved.focus, None);
    }

    /// Applied in the order given, so they have to come out ascending whatever
    /// order `StackSet::screens` yielded them in.
    #[cfg(feature = "river")]
    #[test]
    fn the_screens_come_back_in_index_order() {
        let saved = parse_handover("screen\t2\t3\nscreen\t0\t1\nscreen\t1\t2\n");

        assert_eq!(
            saved.screens,
            vec![(0, "1".to_owned()), (1, "2".to_owned()), (2, "3".to_owned())]
        );
    }

    /// The file the generation before the kind field wrote: every line is a
    /// window and starts with its identifier. Rejecting those would put every
    /// window on the current workspace on the first restart after an upgrade.
    #[cfg(feature = "river")]
    #[test]
    fn a_handover_from_before_the_line_kinds_existed_parses() {
        let saved = parse_handover("a\t1\t0\nb\t8\t1\tfocus\n");

        assert!(saved.screens.is_empty());
        assert_eq!(saved.focused_screen, None);
        assert_eq!(saved.focus.as_deref(), Some("b"));
        assert_eq!(saved.order.get("b"), Some(&1));
        assert_eq!(saved.tags.get("b").map(String::as_str), Some("8"));
    }

    /// The file the generation before the order existed wrote: a `focus` where
    /// the index now goes. Read by position that would be an order, and the
    /// first restart after an upgrade would shuffle every window rather than
    /// none.
    #[cfg(feature = "river")]
    #[test]
    fn a_handover_from_before_the_order_existed_parses() {
        let saved = parse_handover("a\t1\nb\t8\tfocus\n");

        assert!(saved.order.is_empty());
        assert_eq!(saved.focus.as_deref(), Some("b"));
        assert_eq!(saved.tags.get("b").map(String::as_str), Some("8"));
    }

    /// Nothing focused is an ordinary state: every workspace can be empty.
    #[cfg(feature = "river")]
    #[test]
    fn a_handover_without_a_focused_window_parses() {
        let saved = parse_handover("window\ta\t1\n");

        assert_eq!(saved.focus, None);
        assert_eq!(saved.tags.len(), 1);
    }
}
