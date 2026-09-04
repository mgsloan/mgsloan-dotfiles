//! The keymap, mirroring the `keymap` list in the xmonad config.
//!
//! Bindings are written as strings and parsed to keysyms, which each backend
//! resolves for itself: x11rb looks them up against the server when it grabs
//! them, river registers them with the compositor. So the `XF86*` media keys
//! work by name, and the same map drives both sessions. The unit test at the
//! bottom parses the whole map, which catches a mistyped key name without
//! starting a window manager.
//!
//! Keys deliberately left alone under X11: keynav grabs `M-v` and `C-semicolon`
//! for itself, which it can only do there. Under river those are bindings here,
//! driving waynav.

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
    Conn, TAGS, TERMINAL, TERMINAL_ARGS, WAYLAND, actions,
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

        // Notifications. dunst grabbed these keys itself until 1.7, which was
        // X11 only and left them dead under river; dunstctl talks to it over
        // dbus, so one set of bindings covers both.
        "M-n" => actions::dunst("close"),
        "M-S-n" => actions::dunst("close-all"),
        "M-grave" => actions::dunst("history-pop"),

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
    // itself, so they must stay unbound there; waynav grabs nothing until it is
    // run, so under river the entry points have to be bindings.
    //
    // Both of keynav's launch keys: C-semicolon zooms to the focused window and
    // C-S-semicolon covers the whole screen. The window zoom is the window
    // manager's work rather than waynav's -- see actions::waynav_window -- and
    // the whole-screen one is plain `waynav`, which needs no config of its own.
    //
    // The condition is a constant, so this is a `#[cfg]` in everything but
    // effect -- with the difference that both sides are compiled and tested in
    // either build, rather than one of them going unchecked until somebody
    // builds the other backend.
    if WAYLAND {
        raw.extend([
            // Pressing it again dismisses waynav, as keynav's toggle-start did.
            // That cannot be left to waynav at either end: river matches xkb
            // bindings before it consults keyboard focus, so this key never
            // reaches waynav's own grab while the overlay is up, and a second
            // waynav finds the lock in XDG_RUNTIME_DIR held and exits silently.
            // So the toggle is out here, on whether killing one succeeded.
            ("C-semicolon".to_owned(), actions::waynav_window()),
            ("C-S-semicolon".to_owned(), actions::waynav_screen()),
            // Middle click paste at the pointer, which was keynav's M-v. Which
            // commands run on startup is a property of the config file rather
            // than of the launch key, so this entry point needs a config of its
            // own.
            ("M-v".to_owned(), actions::waynav_paste()),
        ]);
    }

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

/// The bindings that go on working while the screen is locked.
///
/// River matches a key against the window manager's bindings before the lock
/// screen sees it, so without a list like this every binding above is available
/// to whoever sits down at a locked laptop -- and one of them is a terminal.
/// This is the whole of the exception, and it is meant to be read as a whole:
/// the question it answers is "what can a stranger at my locked machine do".
///
/// Everything on it changes the volume, the brightness, or what is playing.
/// None of it says anything about what is on the screen or in the session, none
/// of it leaves anything behind, and all of it is what a laptop's own media keys
/// would do without anybody's permission. Nothing that spawns, switches
/// workspace or moves a window belongs here -- and neither does a sequence
/// leader, since the keys that would continue it stay disabled and pressing it
/// would only eat the key.
///
/// X11 needs no counterpart. slock takes an active keyboard grab, which
/// overrides the passive grabs that bindings are made of, so none of them fire
/// while it is up whatever this says.
#[cfg(feature = "river")]
pub fn live_while_locked() -> &'static [&'static str] {
    &[
        // Volume, on the keys with the pictures on them and on the home row.
        "XF86AudioRaiseVolume",
        "XF86AudioLowerVolume",
        "XF86AudioMute",
        "XF86AudioMicMute",
        "M-S-f",
        "M-S-d",
        "M-d",
        // Brightness. The screen goes dark by itself a few seconds into a lock,
        // so being able to bring it back without unlocking is worth more here
        // than anywhere else.
        "M-S-equal",
        "M-S-minus",
        "M-equal",
        "M-minus",
        // Spotify: skipping a track and setting its volume, and the play/pause
        // key, which reaches whichever of Spotify and a focused video is the one
        // playing. Reading a window title to decide is as far into the session
        // as any of this sees.
        //
        // The sequences are allowed a whole key at a time, so `M-m` opens the
        // way to these two and to nothing else it leads to -- `M-m M-d` dumps
        // what the player is doing, and stays behind the lock.
        "XF86AudioPlay",
        "M-m M-m",
        "M-m M-l",
        "M-Left",
        "M-Right",
        "M-Up",
        "M-Down",
        "M-S-Up",
        "M-S-Down",
    ]
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

    /// The allowlist names bindings a second time, so the two can drift apart. An
    /// entry bound to nothing is harmless but means somebody thinks it works
    /// while locked, and it does not.
    #[cfg(feature = "river")]
    #[test]
    fn every_binding_live_while_locked_is_bound() {
        let bound = raw_key_bindings();

        for pattern in live_while_locked() {
            // A sequence is a key at a time, which is how penrose reads it.
            for key in pattern.split_whitespace() {
                penrose::core::bindings::KeySym::parse(key)
                    .unwrap_or_else(|e| panic!("{pattern}: {key} is not a key: {e}"));
            }

            assert!(
                bound.contains_key(*pattern),
                "{pattern} is allowed while locked but nothing is bound to it"
            );
        }
    }

    /// Allowing a sequence must not quietly allow its siblings: `M-m M-l` is on
    /// the list and `M-m M-d`, which says what the player is doing, is not.
    #[cfg(feature = "river")]
    #[test]
    fn allowing_a_sequence_does_not_allow_its_siblings() {
        let allowed: Vec<&str> = live_while_locked().to_vec();

        assert!(allowed.contains(&"M-m M-l"));
        assert!(!allowed.contains(&"M-m M-d"));
        // ...and no bare leader, which would be the way to allow them all.
        assert!(!allowed.contains(&"M-m"));
    }
}
