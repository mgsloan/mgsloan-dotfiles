//! `PhysConn`: a `RustConn` that reports screens in physical order.
//!
//! Penrose indexes screens in whatever order the X server reports them. The
//! xmonad config used `XMonad.Actions.PhysicalScreens` with a comparator
//! ordering screens right-to-left, so that `M-u`/`M-i`/`M-o` land on the same
//! monitors regardless of how randr enumerates them.
//!
//! Sorting here rather than at each call site means every screen index in the
//! program is physical, including the ones penrose computes internally.
//!
//! `impl<X: XConn> Conn for X` in penrose means implementing `XConn` by
//! delegation is enough to get a usable `Conn`.

use std::cmp::Reverse;

use penrose::{
    Result,
    WinId,
    core::bindings::{KeyCode, MouseState},
    pure::geometry::{Point, Rect},
    x::{
        ClientAttr, ClientConfig, XConn, XEvent,
        event::ClientMessage,
        property::{Prop, WindowAttributes, WmState},
    },
    x11rb::RustConn,
};

/// A [RustConn] wrapper that orders screens right-to-left.
#[derive(Debug)]
pub struct PhysConn(RustConn);

impl PhysConn {
    pub fn new() -> Result<Self> {
        Ok(Self(RustConn::new()?))
    }
}

/// Right-to-left, top-to-bottom: descending `(x, y)`.
///
/// Mirrors the `screenOrder` comparator in the xmonad config, which compared
/// `(x2, y2)` against `(x1, y1)`. Screen 0 is therefore the rightmost monitor.
fn physical_order(mut rects: Vec<Rect>) -> Vec<Rect> {
    rects.sort_by_key(|r| (Reverse(r.x), Reverse(r.y)));
    rects
}

impl XConn for PhysConn {
    fn screen_details(&mut self) -> Result<Vec<Rect>> {
        Ok(physical_order(self.0.screen_details()?))
    }

    // Everything below is plain delegation.

    fn root(&mut self) -> WinId {
        self.0.root()
    }

    fn cursor_position(&mut self) -> Result<Point> {
        self.0.cursor_position()
    }

    fn grab(&mut self, key_codes: &[KeyCode], mouse_states: &[MouseState]) -> Result<()> {
        self.0.grab(key_codes, mouse_states)
    }

    fn capture_next_key(&mut self) -> Result<()> {
        self.0.capture_next_key()
    }

    fn cancel_capture_next_key(&mut self) -> Result<()> {
        self.0.cancel_capture_next_key()
    }

    fn next_event(&mut self) -> Result<XEvent> {
        self.0.next_event()
    }

    fn flush(&mut self) {
        self.0.flush()
    }

    fn intern_atom(&mut self, atom: &str) -> Result<WinId> {
        self.0.intern_atom(atom)
    }

    fn atom_name(&mut self, xid: WinId) -> Result<String> {
        self.0.atom_name(xid)
    }

    fn client_geometry(&mut self, client: WinId) -> Result<Rect> {
        self.0.client_geometry(client)
    }

    fn existing_clients(&mut self) -> Result<Vec<WinId>> {
        self.0.existing_clients()
    }

    fn map(&mut self, client: WinId) -> Result<()> {
        self.0.map(client)
    }

    fn unmap(&mut self, client: WinId) -> Result<()> {
        self.0.unmap(client)
    }

    fn kill(&mut self, client: WinId) -> Result<()> {
        self.0.kill(client)
    }

    fn focus(&mut self, client: WinId) -> Result<()> {
        self.0.focus(client)
    }

    fn get_prop(&mut self, client: WinId, prop_name: &str) -> Result<Option<Prop>> {
        self.0.get_prop(client, prop_name)
    }

    fn list_props(&mut self, client: WinId) -> Result<Vec<String>> {
        self.0.list_props(client)
    }

    fn get_wm_state(&mut self, client: WinId) -> Result<Option<WmState>> {
        self.0.get_wm_state(client)
    }

    fn get_window_attributes(&mut self, client: WinId) -> Result<WindowAttributes> {
        self.0.get_window_attributes(client)
    }

    fn set_wm_state(&mut self, client: WinId, wm_state: WmState) -> Result<()> {
        self.0.set_wm_state(client, wm_state)
    }

    fn set_prop(&mut self, client: WinId, name: &str, val: Prop) -> Result<()> {
        self.0.set_prop(client, name, val)
    }

    fn delete_prop(&mut self, client: WinId, prop_name: &str) -> Result<()> {
        self.0.delete_prop(client, prop_name)
    }

    fn set_client_attributes(&mut self, client: WinId, attrs: &[ClientAttr]) -> Result<()> {
        self.0.set_client_attributes(client, attrs)
    }

    fn set_client_config(&mut self, client: WinId, data: &[ClientConfig]) -> Result<()> {
        self.0.set_client_config(client, data)
    }

    fn send_client_message(&mut self, msg: ClientMessage) -> Result<()> {
        self.0.send_client_message(msg)
    }

    fn warp_pointer(&mut self, id: WinId, x: i16, y: i16) -> Result<()> {
        self.0.warp_pointer(id, x, y)
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn screens_are_ordered_right_to_left() {
        let laptop = Rect::new(0, 0, 1920, 1080);
        let left = Rect::new(-2560, 0, 2560, 1440);
        let right = Rect::new(1920, 0, 2560, 1440);

        let ordered = physical_order(vec![laptop, left, right]);

        // index 0 is the rightmost screen, matching `M-o` in the xmonad config
        assert_eq!(ordered, vec![right, laptop, left]);
    }

    #[test]
    fn stacked_screens_order_top_to_bottom() {
        let top = Rect::new(0, 0, 1920, 1080);
        let bottom = Rect::new(0, 1080, 1920, 1080);

        assert_eq!(physical_order(vec![top, bottom]), vec![bottom, top]);
    }
}
