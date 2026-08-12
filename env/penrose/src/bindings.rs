//! The keymap, mirroring the `keymap` list in the xmonad config.
//!
//! Bindings are written as strings and parsed to keysyms, which each backend
//! resolves for itself: x11rb looks them up against the server when it grabs
//! them, river registers them with the compositor. So the `XF86*` media keys
//! work by name, and the same map drives both sessions. The unit test at the
//! bottom parses the whole map, which catches a mistyped key name without
//! starting a window manager.
//!
//! Keys deliberately left alone: dunst owns `M-n`, `M-S-n` and `` M-` ``, and
//! keynav owns `M-v` and `C-semicolon`.

use std::collections::HashMap;

use penrose::{
    builtin::{
        actions::{
            floating::{MouseDragHandler, MouseResizeHandler, sink_all, sink_focused},
            modify_with, send_layout_message,
        },
        layout::messages::{ExpandMain, IncMain, ShrinkMain},
    },
    core::bindings::{
        KeyEventHandler,
        ModifierKey::Meta,
        MouseButton::{Left, Right},
        MouseEventHandler, MouseState,
    },
    map,
};

use crate::{
    Conn, TAGS, TERMINAL, TERMINAL_ARGS, actions,
    actions::{audio, background, capture, notes, spotify},
};

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
        "M-S-Return" => actions::program(TERMINAL, &TERMINAL_ARGS),
        "M-e" => actions::program("emacs", &[]),
        "M-s" => actions::lock_screen(),

        // Capture
        "M-r" => capture::screenshot(),
        "M-S-r" => capture::record(),

        // Notes, and the context they are written with
        "M-a" => notes::add_note(),
        "M-S-a" => notes::add_note_with_clipboard(),
        "M-y" => notes::context_to_clipboard(),

        // Backgrounds
        "M-b M-g" => background::random_binding(),

        // Volume. Up and down unmute first, since the usual reason for
        // reaching for them is that something is inaudible.
        "M-f" => audio::volume_max(),
        "M-S-f" => audio::volume_up(),
        "M-S-d" => audio::volume_down(),
        "M-d" => audio::mute_toggle(),

        // The same, on the keys with the pictures on them
        "XF86AudioRaiseVolume" => audio::volume_up(),
        "XF86AudioLowerVolume" => audio::volume_down(),
        "XF86AudioMute" => audio::mute_toggle(),
        "XF86AudioMicMute" => audio::microphone_toggle(),

        // Play/pause goes to the video if there is one, else to Spotify
        "XF86AudioPlay" => audio::play_pause(),

        // Brightness. Shift is the fine adjustment, plain is the extremes.
        "M-S-equal" => audio::brightness("brightness-increase.sh", "2"),
        "M-S-minus" => audio::brightness("brightness-decrease.sh", "2"),
        "M-equal" => audio::brightness("brightness-set.sh", "100"),
        "M-minus" => audio::brightness("brightness-set.sh", "1"),

        // Spotify
        "M-m M-m" => spotify::toggle_play_binding(),
        "M-m M-l" => spotify::like(),
        "M-m M-d" => spotify::debug_player_info(),
        "M-Left" => spotify::previous(),
        "M-Right" => spotify::next(),
        "M-Up" => spotify::add_volume(5),
        "M-Down" => spotify::add_volume(-5),
        "M-S-Up" => spotify::set_volume(100),
        "M-S-Down" => spotify::set_volume(0),
        "M-S-slash" => spotify::notify_track(),
    };

    // Drive the mouse from the keyboard. Under X11 keynav grabs these keys for
    // itself, so nothing here mentions them; waynav grabs nothing until it is
    // run, so the entry points have to be bindings.
    //
    // Both of keynav's launch keys: C-semicolon zooms to the focused window and
    // C-S-semicolon covers the whole screen. The window zoom is the window
    // manager's work rather than waynav's -- see actions::waynav_window -- and
    // the whole-screen one is plain `waynav`, which needs no config of its own.
    #[cfg(feature = "river")]
    raw.extend([
        // Pressing it again dismisses waynav, as keynav's toggle-start did.
        // That cannot be left to waynav at either end: river matches xkb
        // bindings before it consults keyboard focus, so this key never reaches
        // waynav's own grab while the overlay is up, and a second waynav finds
        // the lock in XDG_RUNTIME_DIR held and exits silently. So the toggle is
        // out here, on whether killing one succeeded.
        ("C-semicolon".to_owned(), actions::waynav_window()),
        ("C-S-semicolon".to_owned(), actions::waynav_screen()),
        // Middle click paste at the pointer, which was keynav's M-v. Which
        // commands run on startup is a property of the config file rather than
        // of the launch key, so this entry point needs a config of its own.
        ("M-v".to_owned(), actions::waynav_paste()),
    ]);

    // M-<tag>    focus that tag on this screen (xmonad's greedyView)
    // M-S-<tag>  move the focused window to that tag
    // M-C-<tag>  focus that tag on the other screen
    for tag in TAGS {
        raw.extend([
            (format!("M-{tag}"), actions::focus_tag(tag)),
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
    use penrose::core::bindings::parse_keybindings;

    #[test]
    fn bindings_parse_correctly() {
        if let Err(e) = parse_keybindings(raw_key_bindings()).into_result() {
            panic!("{e}");
        }
    }
}
