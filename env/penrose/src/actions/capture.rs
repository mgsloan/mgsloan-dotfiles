//! Screenshots, screen recordings, OCR, and pasting to a gist.
//!
//! Everything that takes something off the screen and puts it somewhere else.
//! The directories these write into are created by `startup.rs`, since neither
//! flameshot nor byzanz will make one.

use penrose::{builtin::actions::key_handler, core::bindings::KeyEventHandler};
use tracing::warn;

use crate::{Conn, env, menu, notify::notify, process};

/// Default byzanz recording length, in seconds, when the prompt is left empty.
const DEFAULT_RECORDING: &str = "10";

/// `M-r`: select a region, copy it to the clipboard, and keep a copy.
///
/// `--path` is what puts the copy in `~/pics/screenshots` rather than wherever
/// flameshot was last pointed.
pub fn screenshot() -> Box<dyn KeyEventHandler<Conn>> {
    key_handler(|_, _| {
        let path = env::get().home("pics/screenshots/");

        process::spawn(
            "flameshot",
            &["gui", "--accept-on-select", "--clipboard", "--path", &path],
        )?;

        Ok(())
    })
}

/// `M-S-r`: record a region as a gif.
///
/// The prompt takes byzanz's arguments — a bare number is a duration in
/// seconds, which is all it is usually given.
pub fn record() -> Box<dyn KeyEventHandler<Conn>> {
    key_handler(|_, _| {
        std::thread::spawn(|| {
            let Some(args) = menu::prompt("Byzanz arguments: ") else {
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

            // byzanz-record-region.sh appends rather than replaces, so a name
            // collision would produce a broken file.
            let _ = std::fs::remove_file(&output);

            let script = env.script("byzanz-record-region.sh");

            // Sequential: the recording has to finish before there is anything
            // to open, and this is already off the event loop.
            match process::status(&script, &[&args, &output]) {
                Ok(0) => {
                    if let Err(e) = process::spawn("google-chrome", &[&output]) {
                        warn!(%e, "unable to open the recording");
                    }
                }
                Ok(code) => notify(&format!("byzanz-record-region.sh exited with {code}")),
                Err(e) => {
                    warn!(%e, "unable to run byzanz-record-region.sh");
                    notify("Unable to run byzanz-record-region.sh");
                }
            }
        });

        Ok(())
    })
}

/// Read text off the screen and put it on the clipboard.
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
