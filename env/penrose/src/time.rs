//! Wall-clock waits for inhibition deadlines, including time spent suspended.

use rustix::{
    io::Errno,
    thread::{ClockId, Timespec, clock_nanosleep_absolute},
};

/// Blocks the calling thread; only call from a worker thread.
pub fn sleep_until(deadline: u64) -> rustix::io::Result<()> {
    let deadline = Timespec {
        tv_sec: deadline.try_into().map_err(|_| Errno::INVAL)?,
        tv_nsec: 0,
    };
    loop {
        // Absolute realtime waits expire during suspend and follow clock changes.
        match clock_nanosleep_absolute(ClockId::Realtime, &deadline) {
            Err(Errno::INTR) => continue,
            result => return result,
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use std::{sync::mpsc, thread, time::Duration};

    #[test]
    fn expired_deadline_returns_immediately() {
        let (sender, receiver) = mpsc::channel();
        thread::spawn(move || sender.send(sleep_until(1)).unwrap());
        receiver
            .recv_timeout(Duration::from_secs(1))
            .unwrap()
            .unwrap();
    }

    #[test]
    fn waits_until_wall_clock_deadline() {
        let deadline = jiff::Timestamp::now().as_second() as u64 + 1;
        let (sender, receiver) = mpsc::channel();
        thread::spawn(move || {
            sleep_until(deadline).unwrap();
            sender
                .send(jiff::Timestamp::now().as_second() as u64)
                .unwrap();
        });
        assert!(receiver.recv_timeout(Duration::from_secs(2)).unwrap() >= deadline);
    }
}
