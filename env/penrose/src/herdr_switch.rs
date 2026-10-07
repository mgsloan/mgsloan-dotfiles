//! Publish workspace visibility for the separate Herdr editor bridge.

use std::{
    collections::BTreeSet,
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
const ZED_CLASSES: [(&str, &str); 4] = [
    ("dev.zed.Zed", "stable"),
    ("dev.zed.Zed-Preview", "preview"),
    ("dev.zed.Zed-Nightly", "nightly"),
    ("dev.zed.Zed-Dev", "dev"),
];
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
            PathBuf::from(runtime).join(format!("penrose-herdr-{}.json", std::process::id()));
        let executable = std::env::current_exe()?.with_file_name("herdr-switch-sync");
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
            warn!(%error, "unable to start Herdr editor bridge");
            None
        }
    }
}

#[derive(Default)]
struct Published(String);

pub fn publish(state: &mut State<Conn>, conn: &mut Conn) {
    let Some(path) = SNAPSHOT.get() else { return };
    let mut herdr = false;
    let mut zed = BTreeSet::new();
    let mut frames = Vec::new();

    // Workspace membership is intentional: monocle and fullscreen must keep syncing.
    for id in state.client_set.on_screen_workspace_clients().copied() {
        #[cfg(feature = "x11")]
        let class = {
            use penrose::x::{XConn as _, property::Prop};
            match conn.get_prop(id, "WM_CLASS") {
                Ok(Some(Prop::UTF8String(classes))) => classes.into_iter().nth(1),
                _ => None,
            }
        };
        #[cfg(feature = "river")]
        let class = conn.window_app_id(id);
        herdr |= class.as_deref() == Some(CLASS);

        if let Some((_, channel)) = ZED_CLASSES
            .iter()
            .find(|(name, _)| class.as_deref() == Some(name))
        {
            zed.insert(channel);
        }

        if let Ok(title) = conn.client_title(id)
            && title.ends_with(" - Emacs")
        {
            frames.push(json!({ "title": title, "pid": conn.client_pid(id) }));
        }
    }
    frames.sort_by_cached_key(|frame| frame.to_string());
    let snapshot =
        json!({ "owner": std::process::id(), "herdr": herdr, "frames": frames, "zed": zed })
            .to_string();
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
