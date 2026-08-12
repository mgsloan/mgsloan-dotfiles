//! Connecting and disconnecting bluetooth devices.
//!
//! Driven by typing into the `bluetoothctl` session that `startup.rs` leaves
//! running, rather than by talking to bluez: `bluetoothctl` is interactive, and
//! this is the approach that works without reimplementing it. The cost is the
//! dependency — no `bt` tmux session, no bluetooth bindings — which is why the
//! failure below says so rather than doing nothing.

use tracing::warn;

use crate::{env, notify::notify, process};

/// The tmux session `startup::wireless_terminals` creates.
const SESSION: &str = "bt";

#[derive(Clone, Copy)]
pub enum Device {
    Headphones,
    Receiver,
}

impl Device {
    fn name(self) -> &'static str {
        match self {
            Self::Headphones => "headphones",
            Self::Receiver => "receiver",
        }
    }

    /// The UUID read from `~/env/untracked` at startup.
    fn uuid(self) -> Option<&'static String> {
        let env = env::get();

        match self {
            Self::Headphones => env.headphones_uuid.as_ref(),
            Self::Receiver => env.receiver_uuid.as_ref(),
        }
    }
}

pub fn connect(device: Device) {
    send("connect", device);
}

pub fn disconnect(device: Device) {
    send("disconnect", device);
}

/// Type a bluetoothctl command into the session, and press return.
fn send(command: &str, device: Device) {
    let Some(uuid) = device.uuid() else {
        let name = device.name();

        warn!(name, "no uuid, so this device cannot be addressed");
        notify(&format!("No {name}.uuid in ~/env/untracked"));
        return;
    };

    // Three keystrokes: the command, the address, and Enter. tmux takes them as
    // separate arguments and types them in order.
    let result = process::spawn(
        "tmux",
        &[
            "send-keys",
            "-t",
            SESSION,
            &format!("{command} {uuid}"),
            "Enter",
        ],
    );

    if let Err(e) = result {
        warn!(%e, command, "unable to drive bluetoothctl");
        notify(&format!("Unable to {command}: is the {SESSION} terminal running?"));
    }
}
