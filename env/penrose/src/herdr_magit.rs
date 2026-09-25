//! Publish workspace visibility for the separate Herdr/Magit bridge.

use std::{
    fs,
    path::PathBuf,
    process::{Child, Command, Stdio},
    sync::OnceLock,
};

use penrose::core::{State, conn::Conn as _, hooks::EventHook};
use serde_json::json;
use tracing::warn;

use crate::Conn;

pub const CLASS: &str = "com.mitchellh.ghostty.herdr";
static SNAPSHOT: OnceLock<PathBuf> = OnceLock::new();

pub struct Bridge {
    child: Child,
    path: PathBuf,
}

impl Drop for Bridge {
    fn drop(&mut self) {
        let _ = fs::remove_file(&self.path);
        let _ = self.child.kill();
    }
}

/// Start before `WindowManager::run`, while SIGCHLD still has its default disposition.
pub fn start() -> Option<Bridge> {
    if std::env::var_os("PENROSE_NO_STARTUP_HOOK").is_some() {
        return None;
    }

    let start = || -> std::io::Result<Bridge> {
        let runtime = std::env::var_os("XDG_RUNTIME_DIR")
            .ok_or_else(|| std::io::Error::other("XDG_RUNTIME_DIR is unset"))?;
        let path =
            PathBuf::from(runtime).join(format!("penrose-magit-{}.json", std::process::id()));
        let executable = std::env::current_exe()?.with_file_name("herdr-magit-sync");
        let child = Command::new(executable)
            .arg(&path)
            .arg(std::process::id().to_string())
            .envs(crate::env::get().overrides())
            .stdin(Stdio::null())
            .stdout(Stdio::null())
            .spawn()?;
        let _ = SNAPSHOT.set(path.clone());
        Ok(Bridge { child, path })
    };

    match start() {
        Ok(bridge) => Some(bridge),
        Err(error) => {
            warn!(%error, "unable to start Herdr/Magit bridge");
            None
        }
    }
}

#[derive(Default)]
struct Published(String);

pub fn publish(state: &mut State<Conn>, conn: &mut Conn) {
    let Some(path) = SNAPSHOT.get() else { return };
    let mut herdr = false;
    let mut frames = Vec::new();

    // Workspace membership is intentional: monocle and fullscreen must keep syncing.
    for id in state.client_set.on_screen_workspace_clients().copied() {
        #[cfg(feature = "x11")]
        let is_herdr = {
            use penrose::{core::conn::ConnExt as _, x::query::ClassName};
            conn.query_or(false, &ClassName(CLASS), id)
        };
        #[cfg(feature = "river")]
        let is_herdr = conn.window_app_id(id).as_deref() == Some(CLASS);
        herdr |= is_herdr;

        if let Ok(title) = conn.client_title(id)
            && title.ends_with(" - Emacs")
        {
            frames.push(json!({ "title": title, "pid": conn.client_pid(id) }));
        }
    }
    frames.sort_by_cached_key(|frame| frame.to_string());
    let snapshot =
        json!({ "owner": std::process::id(), "herdr": herdr, "frames": frames }).to_string();
    let published = state.extension_or_default::<Published>();
    let mut published = published.borrow_mut();
    if published.0 == snapshot {
        return;
    }

    // The runtime directory is local; rename keeps readers from seeing half a snapshot.
    let temporary = path.with_extension("tmp");
    match fs::write(&temporary, &snapshot).and_then(|()| fs::rename(&temporary, path)) {
        Ok(()) => published.0 = snapshot,
        Err(error) => warn!(%error, "unable to publish workspace visibility"),
    }
}

pub fn hook() -> Box<dyn EventHook<Conn>> {
    Box::new(
        |event: &<Conn as penrose::core::conn::Conn>::Event,
         state: &mut State<Conn>,
         conn: &mut Conn| {
            #[cfg(feature = "x11")]
            let changed = matches!(event, penrose::x::XEvent::PropertyNotify(property)
            if !property.is_root && matches!(property.atom.as_str(), "WM_NAME" | "_NET_WM_NAME" | "WM_CLASS" | "_NET_WM_PID"));
            #[cfg(feature = "river")]
            let changed = matches!(
                event,
                penrose::river::RiverEvent::Title(_) | penrose::river::RiverEvent::AppId(_)
            );
            if changed {
                publish(state, conn);
            }
            Ok(true)
        },
    )
}
