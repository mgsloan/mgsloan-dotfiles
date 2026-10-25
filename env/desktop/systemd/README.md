# Desktop session units

- `river-session.target` groups river session services. Setup links it into
  `~/.config/systemd/user/`; see
  [the river session installer](../../setup/046-create-penrose-river-session.sh).
- `slock@.service` locks X11 sessions before sleep. It skips Wayland sessions,
  where swayidle handles locking. This is a system unit, installed under
  `/etc/systemd/system/`, despite belonging to the desktop configuration.

After changing an installed unit, update its copy and reload the appropriate
systemd manager (`systemctl --user daemon-reload` for the river target,
`sudo systemctl daemon-reload` for slock).

## Same-spot reminders

`same-spot.service` reads the existing JPEGs under `~/pics/webcam`; it never
opens the camera. YuNet detects faces at 640 pixels wide, retrying with local
contrast enhancement when needed. Presence requires a score of 0.85; lower
thresholds produced false faces on weight plates in the saved captures.
Masked 48×27 background thumbnails are
compared across small shifts and lighting changes against up to eight views.
The model is downloaded with a pinned hash by the public Nix configuration.

Reminders start at 45 minutes and repeat every 15 minutes, replacing the
previous notification. Only a fresh image with a detected face can trigger a
reminder. Locking and webcam inhibition suppress notifications. State survives
service restarts in `$XDG_STATE_HOME/same-spot/state.json` (by default,
`~/.local/state/same-spot/state.json`).

Five minutes of consecutive, clear images without a detected face count as a
probable break. Dark, blurred, or marginal detections are uncertain. Five
minutes without reliable presence starts a fresh estimate on return, including
capture gaps caused by locking, inhibition, or suspend. Such gaps are not
classified as physical breaks.

Presence requires a confident face at least 20% of the image height. Smaller
background faces do not count as presence or prevent a contrast-enhanced search
for a nearby face. Set `--min-face-fraction` to change this proportion in the
live service or historical review.

Shorter confirmed absences do not reset the timer. Reminder details report
their count, total estimated duration, longest duration, and latest duration,
alongside the five-minute minimum. Separate short breaks are not combined into
a qualifying break. Uncertain images and capture gaps do not add inferred away
time. Durations have the precision of the capture interval (about 30 seconds).
These measurements survive service restarts and clear with the session timer.

Scene changes during continuous presence are accepted as camera adjustments.
After a short interruption, a different background must persist for one minute
before resetting the timer. A background with insufficient texture or overlap
cannot establish a move; after one minute of renewed presence, tracking resumes
with the previous estimate. Moving while remaining continuously in view and
breaks between captures can go undetected. Turning away for a long time can
look like a break.

Both same-spot unit files are versioned directly in `~/.config/systemd/user/`.
After installing the Nix environment, `~/env/setup/041-systemd-apply.sh` reloads
systemd and enables the reminder service. Change defaults with `systemctl --user edit same-spot`
and an `ExecStart` override; see `same-spot --help` for timing and threshold
options. Restart the service after changing its command.

Replay never sends notifications or changes live state:

```sh
same-spot --replay ~/pics/webcam/2026-10-15 --since 2026-10-15T18:00 > /tmp/same-spot.jsonl
journalctl --user -u same-spot.service -f
systemctl --user stop same-spot.service
```

Replay emits one JSON record per image with detector scores, face height,
quality measures, short-break measurements, scene similarity, reset reasons,
estimated duration, proposed reminders, and processing time. Live logs use the
same fields without image filenames.
For a live test without notifications, use `--watch ~/pics/webcam --dry-run
--state /tmp/same-spot-test/state.json`; `--once` processes pending images and
exits. Normal startup starts from the newest image rather than replaying old
history. A saved state resumes from the last processed capture.

## Historical review

Start `same-spot-review.service`, then open <http://127.0.0.1:8765>. It is a
local viewer, started on demand rather than enabled at login:

```sh
systemctl --user start same-spot-review.service
systemctl --user stop same-spot-review.service
```

The service analyzes archived `YYYY-MM-DD.hevc` files, newest first, using each
companion `YYYY-MM-DD-timestamps` file for the original capture times. Every
decoded frame must have exactly one timestamp. Analysis uses the same detector
and timer policy as the live service, without sending notifications or touching
live state. Each day starts a fresh estimate.

Clips split when the tracked streak changes, tracking stops, or a long capture
gap occurs. All footage is kept, including untracked intervals. Break boundaries
reflect when the detector made its decision; they are not backdated to when
someone first left the frame. These intervals are not location clusters.

H.264 MP4 review copies play at the original 48 captures per second, with exact frame counts
checked for every clip. The viewer plays clips consecutively, supports selecting
a day or all ready days, and shows original timestamps, presence scores, estimated
streak duration, and proposed reminders. The colored strip shows detections and
supports clicking to seek. Original HEVC files and timestamps are read only.

Analysis and clips are cached under `$XDG_CACHE_HOME/same-spot-review` (normally
`~/.cache/same-spot-review`). Changes to the source, timestamps, detector, model,
or analysis settings produce a new cache entry. Completed days are reused after
a restart; an interrupted day is recomputed. The service uses four workers at
low scheduling priority; the command defaults to two. Completed days remain
available in the viewer. Restart to discover new archives.

The command also accepts a single video, a date filter, a cache location, and a
worker count. For example:

```sh
same-spot-review ~/pics/webcam --date 2026-10-14 --analyze-only
same-spot-review ~/pics/webcam --port 8766
journalctl --user -u same-spot-review.service -f
```

Follow-up: revisit a cheap comparison with the previous frame to skip face
detection and other expensive analysis on near-duplicates. Include periodic full
checks so a mistaken observation cannot persist indefinitely. Keep this separate
from the baseline evaluation. Location clustering and human labels are also
future work.
