//! `TallWheel`: the layout from the xmonad config.
//!
//! Identical to xmonad's builtin `Tall` except that the order of the master
//! pane is reversed, so that windows rotate through a wheel rather than being
//! pushed along two stacks.
//!
//! With `max_main == 1` this is indistinguishable from `Tall`; the difference
//! shows up once `IncMain` grows the master pane.

use penrose::{
    builtin::layout::messages::{ExpandMain, IncMain, ShrinkMain},
    WinId,
    core::layout::{Layout, Message},
    pure::{Stack, geometry::Rect},
};

/// Proportion of the screen given to the master pane by default.
pub const PHI: f32 = 0.618_03;

#[derive(Debug, Clone, Copy)]
pub struct TallWheel {
    max_main: u32,
    ratio: f32,
    ratio_step: f32,
}

impl TallWheel {
    pub fn new(max_main: u32, ratio: f32, ratio_step: f32) -> Self {
        Self {
            max_main,
            ratio,
            ratio_step,
        }
    }

    /// The parameters used by the xmonad config: `TallWheel 1 (phi / 8) phi`.
    pub fn boxed_default() -> Box<dyn Layout> {
        Box::new(Self::new(1, PHI, PHI / 8.0))
    }

    /// True when there is no meaningful split into two panes.
    fn single_pane(&self, n: u32) -> bool {
        n <= self.max_main || self.max_main == 0 || self.ratio <= 0.0 || self.ratio >= 1.0
    }
}

impl Layout for TallWheel {
    fn name(&self) -> String {
        "TallWheel".to_owned()
    }

    fn boxed_clone(&self) -> Box<dyn Layout> {
        Box::new(*self)
    }

    fn layout(
        &mut self,
        s: &Stack<WinId>,
        r: Rect,
    ) -> (Option<Box<dyn Layout>>, Vec<(WinId, Rect)>) {
        let n = s.len() as u32;

        let positions: Vec<(WinId, Rect)> = if self.single_pane(n) {
            r.as_rows(n).into_iter().zip(s).map(|(r, c)| (*c, r)).collect()
        } else {
            let (main, stack) = r
                .split_at_width_perc(self.ratio)
                .expect("split point to be valid");

            // The wheel: the master pane runs bottom-to-top, the stack pane
            // top-to-bottom, so that focus travels round rather than jumping
            // back to the top of each pane.
            let mut main_rows = main.as_rows(self.max_main);
            main_rows.reverse();

            main_rows
                .into_iter()
                .chain(stack.as_rows(n.saturating_sub(self.max_main)))
                .zip(s)
                .map(|(r, c)| (*c, r))
                .collect()
        };

        (None, positions)
    }

    fn handle_message(&mut self, m: &Message) -> Option<Box<dyn Layout>> {
        if let Some(&ExpandMain) = m.downcast_ref() {
            self.ratio = (self.ratio + self.ratio_step).min(1.0);
        } else if let Some(&ShrinkMain) = m.downcast_ref() {
            self.ratio = (self.ratio - self.ratio_step).max(0.0);
        } else if let Some(&IncMain(n)) = m.downcast_ref() {
            if n < 0 {
                self.max_main = self.max_main.saturating_sub(n.unsigned_abs() as u32);
            } else {
                self.max_main += n as u32;
            }
        }

        None
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use penrose::core::layout::IntoMessage;

    const SCREEN: Rect = Rect {
        x: 0,
        y: 0,
        w: 1000,
        h: 900,
    };

    fn stack(n: u8) -> Stack<WinId> {
        let ids: Vec<WinId> = (1..=n).map(|i| WinId::from(i as u32)).collect();
        Stack::try_from_iter(ids).expect("non-empty")
    }

    fn positions(l: &mut TallWheel, n: u8) -> Vec<Rect> {
        l.layout(&stack(n), SCREEN).1.into_iter().map(|(_, r)| r).collect()
    }

    #[test]
    fn single_client_fills_the_screen() {
        assert_eq!(positions(&mut TallWheel::new(1, 0.5, 0.1), 1), vec![SCREEN]);
    }

    #[test]
    fn one_master_splits_left_right() {
        let rs = positions(&mut TallWheel::new(1, 0.5, 0.1), 2);
        assert_eq!(rs[0], Rect::new(0, 0, 500, 900));
        assert_eq!(rs[1], Rect::new(500, 0, 500, 900));
    }

    #[test]
    fn master_pane_is_reversed() {
        // Two in master, two in the stack. The master column is bottom-to-top,
        // which is what distinguishes this from Tall.
        let rs = positions(&mut TallWheel::new(2, 0.5, 0.1), 4);

        assert_eq!(rs[0].y, 450, "first master client is the lower one");
        assert_eq!(rs[1].y, 0, "second master client is the upper one");
        assert_eq!(rs[2].y, 0, "stack pane runs top-to-bottom");
        assert_eq!(rs[3].y, 450);
    }

    #[test]
    fn all_clients_stack_when_max_main_covers_them() {
        let rs = positions(&mut TallWheel::new(3, 0.5, 0.1), 2);
        assert_eq!(rs[0], Rect::new(0, 0, 1000, 450));
        assert_eq!(rs[1], Rect::new(0, 450, 1000, 450));
    }

    #[test]
    fn messages_adjust_ratio_and_main_count() {
        let mut l = TallWheel::new(1, 0.5, 0.1);

        l.handle_message(&ExpandMain.into_message());
        assert!((l.ratio - 0.6).abs() < 1e-6);

        l.handle_message(&ShrinkMain.into_message());
        assert!((l.ratio - 0.5).abs() < 1e-6);

        l.handle_message(&IncMain(2).into_message());
        assert_eq!(l.max_main, 3);

        l.handle_message(&IncMain(-5).into_message());
        assert_eq!(l.max_main, 0, "saturates rather than underflowing");
    }

    #[test]
    fn ratio_is_clamped() {
        let mut l = TallWheel::new(1, 0.95, 0.1);
        l.handle_message(&ExpandMain.into_message());
        assert!((l.ratio - 1.0).abs() < 1e-6);

        let mut l = TallWheel::new(1, 0.05, 0.1);
        l.handle_message(&ShrinkMain.into_message());
        assert!((l.ratio - 0.0).abs() < 1e-6);
    }
}
