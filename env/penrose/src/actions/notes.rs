//! Notes, the clipboard, and describing what was on screen.
//!
//! All three bindings are the same idea: capture *when* and *what I was looking
//! at*, because that is the part that is expensive to reconstruct later and
//! free to record now. The window title is the only thing a window manager
//! knows about what you were doing, which is why this lives here rather than in
//! an editor.

use std::sync::LazyLock;

use penrose::{
    builtin::actions::key_handler,
    core::{bindings::KeyEventHandler, conn::Conn as _},
};
use regex::Regex;
use tracing::warn;

use crate::{Conn, env, menu, notify, programs};

/// Where the notes go, relative to `$HOME`.
const NOTES: &str = "docs/obsidian/notes.md";

/// `xclip` blocks forever when no other client owns the selection, so every
/// read of it is bounded by `timeout`.
const CLIPBOARD_TIMEOUT: &str = "0.2";

/// `M-a`: type a note, and record when and where it was written.
pub fn add_note() -> Box<dyn KeyEventHandler<Conn>> {
    key_handler(|state, conn: &mut Conn| {
        // The context has to be read before the prompt opens, since the prompt
        // is itself a window and would otherwise be the thing described.
        let context = context(state, conn);

        std::thread::spawn(move || {
            let Some(content) = menu::prompt(&format!("Add to {NOTES}: ")) else {
                return;
            };

            append(&format!("\n{context}:\n  {content}\n"));
        });

        Ok(())
    })
}

/// `M-S-a`: the same, with the clipboard quoted underneath.
pub fn add_note_with_clipboard() -> Box<dyn KeyEventHandler<Conn>> {
    key_handler(|state, conn: &mut Conn| {
        let context = context(state, conn);

        std::thread::spawn(move || {
            let Some(content) = menu::prompt(&format!("Add to {NOTES} (with clipboard): ")) else {
                return;
            };

            let mut body = format!("\n* {context}:\n");

            if !content.trim().is_empty() {
                body.push_str(&format!("  {content}\n"));
            }

            for line in clipboard().lines() {
                body.push_str(&format!("  > {line}\n"));
            }

            append(&body);
        });

        Ok(())
    })
}

/// `M-y`: put the context and the clipboard back on the clipboard, quoted.
///
/// For pasting somewhere that is not the notes file — a message, an issue —
/// with the provenance already attached.
pub fn context_to_clipboard() -> Box<dyn KeyEventHandler<Conn>> {
    key_handler(|state, conn: &mut Conn| {
        let context = context(state, conn);

        std::thread::spawn(move || {
            let quoted: String = clipboard().lines().map(|l| format!("> {l}\n")).collect();
            let content = format!("{context}:\n\n{quoted}");

            if let Err(e) = programs::clipboard_copy(&content) {
                warn!(%e, "unable to write to the clipboard");
                return;
            }

            notify::notify_truncated(&format!("Copied: {content}"));
        });

        Ok(())
    })
}

/// A timestamp, plus a phrase describing the focused window.
fn context(state: &mut penrose::core::State<Conn>, conn: &mut Conn) -> String {
    let title = state
        .client_set
        .current_client()
        .copied()
        .and_then(|id| conn.client_title(id).ok());

    let now = jiff::Zoned::now().strftime("[[%Y-%m-%d]] %H:%M:%S");

    match title.as_deref() {
        Some(title) => format!("{now} {}", describe(title)),
        None => now.to_string(),
    }
}

/// Turn a window title into something worth reading in six months.
///
/// The patterns are the ones that carry a location: a page, a file, a note.
/// Anything else keeps the title verbatim, which is still better than nothing.
fn describe(title: &str) -> String {
    struct Patterns {
        chrome_page: Regex,
        chrome_url: Regex,
        emacs: Regex,
        obsidian: Regex,
    }

    static PATTERNS: LazyLock<Patterns> = LazyLock::new(|| Patterns {
        chrome_page: Regex::new(r"^(.+)\s+-\s+(\S+)\s+-\s+Google Chrome$").expect("valid"),
        chrome_url: Regex::new(r"^(\S+)\s+-\s+Google Chrome$").expect("valid"),
        emacs: Regex::new(r"^(\S+)\s+-\s+Emacs$").expect("valid"),
        obsidian: Regex::new(r"^(.+)\s+-\s+obsidian\s+-\s+Obsidian\s+v[0-9.]+").expect("valid"),
    });

    let p = &*PATTERNS;

    if let Some(c) = p.chrome_page.captures(title) {
        format!("While browsing [{}]({})", &c[1], &c[2])
    } else if let Some(c) = p.chrome_url.captures(title) {
        format!("While browsing {}", &c[1])
    } else if let Some(c) = p.emacs.captures(title) {
        format!("While editing file://{}", &c[1])
    } else if let Some(c) = p.obsidian.captures(title) {
        format!("With focus on [[{}]]", &c[1])
    } else {
        format!("With focus on '{title}'")
    }
}

/// The clipboard, or an empty string if it could not be read.
fn clipboard() -> String {
    match programs::clipboard_paste(CLIPBOARD_TIMEOUT) {
        Ok(contents) => contents.trim().to_owned(),
        Err(e) => {
            warn!(%e, "unable to read the clipboard");
            notify::notify("Unable to read the clipboard");
            String::new()
        }
    }
}

/// Add to the notes file, and say what was added.
fn append(content: &str) {
    use std::io::Write;

    let path = env::get().home(NOTES);

    let appended = std::fs::OpenOptions::new()
        .create(true)
        .append(true)
        .open(&path)
        .and_then(|mut file| file.write_all(content.as_bytes()));

    match appended {
        Ok(()) => notify::notify_truncated(&format!("Appended the following: {content}")),
        Err(e) => {
            warn!(%e, path, "unable to append to the notes file");
            notify::notify(&format!("Unable to write to {NOTES}"));
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn a_chrome_page_becomes_a_markdown_link() {
        assert_eq!(
            describe("Rust Programming Language - https://rust-lang.org - Google Chrome"),
            "While browsing [Rust Programming Language](https://rust-lang.org)"
        );
    }

    #[test]
    fn a_bare_chrome_url_keeps_the_url() {
        assert_eq!(
            describe("https://example.com - Google Chrome"),
            "While browsing https://example.com"
        );
    }

    #[test]
    fn emacs_becomes_a_file_url() {
        assert_eq!(
            describe("/home/mgsloan/env/penrose/design.md - Emacs"),
            "While editing file:///home/mgsloan/env/penrose/design.md"
        );
    }

    #[test]
    fn obsidian_becomes_a_wiki_link() {
        assert_eq!(
            describe("Some Note - obsidian - Obsidian v1.5.3"),
            "With focus on [[Some Note]]"
        );
    }

    #[test]
    fn anything_else_keeps_the_title() {
        assert_eq!(describe("htop"), "With focus on 'htop'");
        assert_eq!(describe(""), "With focus on ''");
    }
}
