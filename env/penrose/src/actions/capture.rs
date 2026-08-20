//! Screenshots, screen recordings, OCR, and pasting to a gist.
//!
//! Everything that takes something off the screen and puts it somewhere else.
//! The directories these write into are created by `startup.rs`, since none of
//! the capture programs will make one.
//!
//! Which program does the capturing is `programs.rs`'s business, because it is
//! the one thing here that the backend decides. What is left — the prompt, the
//! naming, and what happens to the result — is the same either way.

use penrose::{builtin::actions::key_handler, core::bindings::KeyEventHandler};
use tracing::warn;

use crate::{Conn, env, menu, notify::notify, process, programs};

/// Default recording length, in seconds, when the prompt is left empty.
const DEFAULT_RECORDING: &str = "10";

/// `M-r`: select a region, copy it to the clipboard, and keep a copy.
///
/// On a thread because the Wayland path blocks on the selection, and a handler
/// that blocks blocks the window manager — including the compositor events the
/// selection overlay needs to draw itself, which would deadlock the two.
pub fn screenshot() -> Box<dyn KeyEventHandler<Conn>> {
    key_handler(|_, _| {
        std::thread::spawn(|| {
            let dir = env::get().home("pics/screenshots");

            if let Err(e) = programs::screenshot_region(&dir) {
                warn!(%e, "unable to take a screenshot");
                notify("Unable to take a screenshot");
            }
        });

        Ok(())
    })
}

/// `M-S-r`: record a region as a gif.
///
/// The prompt takes a duration in seconds, which under X11 is passed to byzanz
/// as an argument and so can be anything else byzanz takes.
pub fn record() -> Box<dyn KeyEventHandler<Conn>> {
    key_handler(|_, _| {
        std::thread::spawn(|| {
            let Some(args) = menu::prompt("Recording seconds: ") else {
                return;
            };

            let args = if args.trim().is_empty() {
                DEFAULT_RECORDING.to_owned()
            } else {
                args
            };

            let env = env::get();
            let name = jiff::Zoned::now()
                .strftime("%Y-%m-%d_%H:%M:%S.gif")
                .to_string();
            let output = env.home(&format!("pics/screencaps/{name}"));

            // Both scripts refuse an output that is already there rather than
            // overwriting it, and byzanz appends, so a name collision would
            // produce a broken file.
            let _ = std::fs::remove_file(&output);

            let name = programs::record_region_script();
            let script = env.script(name);

            // Sequential: the recording has to finish before there is anything
            // to open, and this is already off the event loop.
            match process::status(&script, &[&args, &output]) {
                Ok(0) => {
                    if let Err(e) = process::spawn("google-chrome", &[&output]) {
                        warn!(%e, "unable to open the recording");
                    }
                }
                // A cancelled selection is one of these, not a failure, so it
                // says what happened rather than calling it an error.
                Ok(code) => notify(&format!("{name} exited with {code}")),
                Err(e) => {
                    warn!(%e, name, "unable to run the recording script");
                    notify(&format!("Unable to run {name}"));
                }
            }
        });

        Ok(())
    })
}

/// Read text off the screen and put it on the clipboard.
///
/// The backend split is inside the script rather than here, unlike the two
/// bindings above: it is one pipeline whose first and last stages change, the
/// script is shared with the xmonad config, and a second copy of it would be
/// two places to fix the next time the OCR arguments want tuning.
pub fn screenshot_ocr() {
    run_script("screenshot-ocr.sh");
}

/// Reset the USB bus, which is how the bluetooth adapter comes back when it
/// stops responding — hence the `bluetooth-reset` synonym in the menu.
pub fn usb_reset() {
    let path = env::get().home(".local/bin/usb-reset.sh");

    if let Err(e) = process::spawn(&path, &[]) {
        warn!(%e, "unable to run usb-reset.sh");
    }
}

/// Paste the clipboard to a private gist and open it.
///
/// The extension is the point: it decides how the gist is highlighted, which is
/// why there is an entry per file type rather than one that guesses.
pub fn gist(filename: &'static str) {
    std::thread::spawn(move || {
        // -P pastes from the clipboard, -p makes it private.
        match process::read_output("gist", &["-P", "-p", "-f", filename]) {
            Ok(output) => {
                let url = output.trim();

                if url.is_empty() {
                    notify("gist produced no url");
                    return;
                }

                if let Err(e) = process::spawn("xdg-open", &[url]) {
                    warn!(%e, url, "unable to open the gist");
                }

                notify(&format!("Gist: {url}"));
            }
            Err(e) => {
                warn!(%e, "unable to run gist");
                notify("Unable to run gist");
            }
        }
    });
}

fn run_script(name: &str) {
    let path = env::get().script(name);

    if let Err(e) = process::spawn(&path, &[]) {
        warn!(%e, name, "unable to run script");
    }
}
