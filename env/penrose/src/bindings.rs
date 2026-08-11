//! The keymap, mirroring the `keymap` list in the xmonad config.
//!
//! Bindings are written as strings and resolved against `xmodmap -pke`, so the
//! `XF86*` media keys work by name. The unit test at the bottom parses the whole
//! map, which catches a mistyped key name without starting a window manager.

use std::collections::HashMap;

use penrose::{
    builtin::{
        actions::{
            floating::{MouseDragHandler, MouseResizeHandler, sink_all, sink_focused},
            modify_with, send_layout_message, spawn,
        },
        layout::messages::{ExpandMain, IncMain, ShrinkMain},
    },
    core::bindings::{
        KeyEventHandler, ModifierKey::Meta, MouseButton::{Left, Right}, MouseEventHandler,
        MouseState,
    },
    map,
};

use crate::{Conn, TAGS, TERMINAL, actions};

pub fn raw_key_bindings() -> HashMap<String, Box<dyn KeyEventHandler<Conn>>> {
    let mut raw = map! {
        map_keys: |k: &str| k.to_owned();

        // Recompile and restart
        "M-q" => actions::restart(),

        // Layout
        "M-space" => modify_with(|cs| cs.next_layout()),
        "M-comma" => send_layout_message(|| IncMain(1)),
        "M-period" => send_layout_message(|| IncMain(-1)),
        "M-l" => send_layout_message(|| ShrinkMain),
        "M-semicolon" => send_layout_message(|| ExpandMain),

        // Screens. Physical indices, right to left: see conn::PhysConn.
        "M-u" => modify_with(|cs| cs.focus_screen(2)),
        "M-i" => modify_with(|cs| cs.focus_screen(1)),
        "M-o" => modify_with(|cs| cs.focus_screen(0)),
        "M-S-u" => modify_with(|cs| cs.move_focused_to_screen(2)),
        "M-S-i" => modify_with(|cs| cs.move_focused_to_screen(1)),
        "M-S-o" => modify_with(|cs| cs.move_focused_to_screen(0)),

        // Windows. Note j/k are inverted relative to the penrose default,
        // matching the xmonad config.
        "M-k" => modify_with(|cs| cs.focus_down()),
        "M-j" => modify_with(|cs| cs.focus_up()),
        "M-S-k" => modify_with(|cs| cs.swap_down()),
        "M-S-j" => modify_with(|cs| cs.swap_up()),
        "M-S-c" => modify_with(|cs| cs.kill_focused()),

        // Master
        "M-h" => modify_with(|cs| cs.focus_head()),
        "M-S-h" => modify_with(|cs| cs.swap_focus_and_head()),

        // Floating
        "M-t" => sink_focused(),
        "M-S-t" => sink_all(),

        // Launchers
        "M-p" => actions::run_prompt(),
        "M-x" => actions::action_menu(),
        "M-S-Return" => spawn(TERMINAL),
        "M-e" => spawn("emacs"),
        "M-s" => spawn("slock"),
        "M-r" => spawn("flameshot gui --accept-on-select --clipboard"),
    };

    // M-<tag>    focus that tag on this screen (xmonad's greedyView)
    // M-S-<tag>  move the focused window to that tag
    // M-C-<tag>  focus that tag on the other screen
    for tag in TAGS {
        raw.extend([
            (
                format!("M-{tag}"),
                modify_with(move |cs| cs.pull_tag_to_screen(tag)),
            ),
            (
                format!("M-S-{tag}"),
                modify_with(move |cs| cs.move_focused_to_tag(tag)),
            ),
            (
                format!("M-C-{tag}"),
                modify_with(move |cs| {
                    cs.next_screen();
                    cs.pull_tag_to_screen(tag);
                }),
            ),
        ]);
    }

    raw
}

/// Move and resize floating windows with the windows key held.
///
/// The xmonad config used `FlexibleManipulate`, where one drag did both
/// depending on where in the window it started. These are penrose's built-ins,
/// so move and resize are separate buttons.
pub fn mouse_bindings() -> HashMap<MouseState, Box<dyn MouseEventHandler<Conn>>> {
    map! {
        map_keys: |(button, modifiers)| MouseState { button, modifiers };

        (Left, vec![Meta]) => MouseDragHandler::boxed_default(),
        (Right, vec![Meta]) => MouseResizeHandler::boxed_default(),
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use penrose::core::bindings::parse_keybindings_with_xmodmap;

    #[test]
    fn bindings_parse_correctly_with_xmodmap() {
        if let Err(e) = parse_keybindings_with_xmodmap(raw_key_bindings()) {
            panic!("{e}");
        }
    }
}
