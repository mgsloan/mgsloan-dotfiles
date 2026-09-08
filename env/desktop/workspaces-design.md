# Path-scoped workspaces

A design for giving workspaces a working directory, persisting what is in them,
restoring them on restart, and closing them without losing them.

Written 2026-08-06, against this config as it stands (`src/`, 10 fixed
workspace tags) and against the river port in [`xmonad-river/`](xmonad-river/README.md).
Nothing here is implemented yet.

---

## 0. Summary

A workspace gains an optional **path**. Everything spawned there starts with
that path as its cwd. The workspace's contents — which terminals, which browser,
which editor, and enough state to bring each back — are written to disk as they
change, so:

- **Restart** (`M-q`) and **reboot** both restore the same way.
- **Close** quits every window in a workspace but keeps its record, so a
  workspace is a thing you return to rather than a thing you rebuild.

The hard part is not persistence. It is that Chrome, Zed and (by default)
Ghostty are *single-instance*: one process serves every window, so the
pid→workspace association that `Process.spawnOn` relies on has nothing to bite
on. §4 is about that, and the answer generalises: **stop trying to identify
windows of a shared process — partition the process instead, and name the
partition so it shows up in `app_id`.**

Terminals: keep tmux, demote it to a persistence layer, and let Ghostty be the
renderer. tmux is the only piece in this stack that lets the window manager ask
a terminal for its scrollback from the outside, which is exactly what "restore
with history" needs (§6).

---

## 1. The model

Today `workspaceNames` is `["1".."9", "0"]` and a workspace is nothing but a
tag. The change is additive:

```haskell
-- | Persistent record of a workspace. Absent from the store => an ordinary
-- unscoped workspace, behaving exactly as today.
data WorkspaceRecord = WorkspaceRecord
  { wrTag       :: WorkspaceId      -- ^ "1".."9", "0". The slot.
  , wrName      :: String           -- ^ Short identity, e.g. "env". Used in
                                    --   app_ids, tmux session names, paths.
  , wrPath      :: FilePath         -- ^ cwd for everything spawned here.
  , wrTerminals :: [TerminalRecord]
  , wrBrowser   :: Maybe BrowserRecord
  , wrEditors   :: [EditorRecord]
  , wrAutoOpen  :: Bool             -- ^ Reopen on cold start?
  }
```

Three states:

| State | Meaning |
| --- | --- |
| **Unscoped** | No record. Tags 9 and 0 (logs, wireless) stay this way. |
| **Open** | Record exists, windows are up. |
| **Closed** | Record exists, no windows, no processes. Reopening replays it. |

The tag stays a *slot*, not an identity. `M-1`..`M-0` and `M-S-1`..`M-S-0` keep
meaning exactly what they mean now — the muscle memory is the most valuable
thing in this config and none of it should move. `wrName` is the identity; the
tag is where that identity is currently parked.

**Deferred:** more scoped workspaces than slots (an archive of closed
workspaces, bind-on-open). The record is already keyed by name rather than
position, so this is a lookup change, not a redesign. Not worth building until
10 slots actually chafe.

### Opening is code, not a replayed command list

The tempting shape is to persist a list of shell commands and re-exec them.
Don't. A stored `exec` list rots invisibly: a flag Chrome drops, a Ghostty
option that changes meaning, and the restore silently produces something
subtly different from what the interactive path produces. Instead persist the
*typed* record above, and let `openWorkspace` be ordinary Haskell that calls
the same functions `M-S-<Return>` and the action prompt call. One code path,
type-checked on every `M-q`.

---

## 2. Where the state lives

```
~/.local/state/xmonad/workspaces/
  <name>/
    meta.json            -- the WorkspaceRecord
    term-<session>.ansi  -- captured scrollback (§6)
    chrome/              -- Chrome --user-data-dir (§7)
```

JSON via `aeson`, which is already a dependency. Writes go to `meta.json.tmp`
followed by `rename`, so a crash mid-write cannot corrupt a workspace.

**Not extensible state.** `XMonad.Util.ExtensibleState` is the obvious home and
is the wrong one twice over:

1. It does not survive a reboot at all, and reboot restore is half the point.
2. It does not survive a *restart* under the river port. From
   [`xmonad-river/README.md`](xmonad-river/README.md): "Extensible state does
   not survive a restart. xmonad serialises it into the resume state handed to
   the new process; river's stop-then-exec path has no equivalent channel yet,
   so `PersistentExtension` behaves like `StateExtension`."

Point 2 has a second victim worth flagging now: `XMonad.Util.SessionStart`,
which `ScreenLock.handleStartup` uses to tell a cold start from an `M-q`, is a
`PersistentExtension`. Under river every restart will look like a session
start, which means `initialStartupAction` re-runs and re-spawns Emacs, Chrome
and Spotify on every recompile. That has to be fixed regardless of this design;
the fix is the same mechanism — put the marker on disk, keyed by boot id
(`/proc/sys/kernel/random/boot_id`) so it self-invalidates on reboot without
needing cleanup.

State is written through on every mutation, not on exit. There is no exit hook
worth trusting: `M-q` execs, and a crash skips it entirely.

---

## 3. Propagating the path

`Process.syncSpawnImpl` builds its `ProcessConfig` through `loggedProc`, so the
cwd goes in one place:

```haskell
loggedProc catArgs cmd args f = do
  ...
  proc "systemd-cat" (catArgs ++ (cmdPath : args)) (f . maybe id setWorkingDir mcwd)
```

`systemd-cat` inherits the cwd and execs the child with it, so wrapping is not
in the way.

`spawnOn ws cmd args` looks up `ws`'s record and uses `wrPath` when there is
one. That single change covers `M-S-<Return>`, `M-e`, the `M-p` shell prompt,
and every action-prompt entry, because they all funnel through `Process`.

Also export to children:

- `WORKSPACE_NAME`, `WORKSPACE_PATH` — so shell functions, scripts and prompts
  can see where they are without re-deriving it.
- Nothing else. Resist adding a general env-var mechanism until something needs
  it.

Deleted paths: if `wrPath` no longer exists at open time, notify and fall back
to `$HOME` rather than failing the spawn. A workspace whose directory moved
should be annoying, not fatal.

---

## 4. Identifying windows (the actual problem)

`Process.manageSpawn` maps a window to a workspace by taking the window's pid,
walking `/proc/<pid>/stat` up the parent chain, and looking for a pid we
registered in `envPidHooks`. This works for Emacs and for the terminals the
config opens today — `~/.config/ghostty/config` pins `gtk-single-instance =
false`, so each `ghostty` invocation is a process of its own with a pid to
walk to — and fails for everything this design cares about:

- **Chrome** — the singleton browser process serves every window. A second
  `google-chrome` invocation hands off over the profile lock and exits.
- **Zed** — the `zed` CLI hands the path to a running instance over a socket.
- **Ghostty** — left at `gtk-single-instance = detect`, an invocation with no
  CLI arguments is a D-Bus call to `com.mitchellh.ghostty`, not a new process.
  (`ghostty +new-window` is that D-Bus call made explicit; with no instance
  running it fails with `NameHasNoOwner`.) Pinning it to `false`, as the config
  now does, buys the pid back — but at one process per window, which is the
  cost S1 avoids by making the `app_id` carry the workspace instead.

Under X11 you'd reach for `_NET_WM_PID`; under river you get
`river_window_v1.unreliable_pid`. Neither helps: the pid is *correct*, it's
just the same pid for every window of the app.

Four strategies, in preference order.

### S1 — Partition the instance, and name it

Give each scoped workspace its own instance of the app, keyed by a string
derived from `wrName`, and arrange for that string to be the Wayland `app_id`.
Placement then reduces to a manage hook with no timing, no pid, and no state:

```haskell
className =? ("com.mitchellh.ghostty.ws-" ++ wrName) --> doShift wrTag
```

Ghostty supports this exactly, and the docs are explicit that it is both the
identity and the partition key:

> This controls the class field of the `WM_CLASS` X11 property (when running
> under X11), the Wayland application ID (when running under Wayland), and the
> bus name that Ghostty uses to connect to DBus.
>
> Note that changing this value between invocations will create new, separate
> instances, of Ghostty when running with `gtk-single-instance=true`.

One flag buys the partition, the identity and a per-workspace D-Bus address
(so `ghostty +new-window` can target one workspace's instance) simultaneously.
The class must be a valid GTK application ID.

Why this is worth preferring even when it costs a process: see §5.

### S2 — pid

The existing `manageSpawn`, unchanged. Correct whenever S1 has already
partitioned the app into its own process, and for anything genuinely
one-process-per-window. Note that S1 usually *implies* S2 works too — the two
reinforce rather than compete.

### S3 — title token

Match on a substring of the title. Only safe where we control the title
(terminals, via `--title` or OSC 2) or where the app's title is a faithful
function of something we chose (Zed titles its window from the project
folder). Fails on ambiguity — two workspaces whose paths share a basename —
so it is a supplement to S4, not a replacement.

### S4 — claim the next window

Last resort, for apps that refuse to partition. Register a *claim*: an
`app_id` predicate, a target workspace, and a deadline. The next unclaimed
window matching the predicate is shifted and the claim retires.

This is racy in exactly one way — another window of the same app appearing
spontaneously inside the window — and that is containable:

- Serialise claimed spawns through an `MVar`, so at most one claim per app is
  ever outstanding.
- Expire after 10s, matching the existing `spawnOn` pid-hook expiry.
- Log expiry rather than failing silently. A claim that never fires is a bug
  and should look like one, in keeping with the port's `warnUnimplemented`
  policy.

The principled version of S4 is `xdg-activation-v1`: the launcher mints a
token, passes it via `XDG_ACTIVATION_TOKEN`, and the client presents it when
mapping — which survives the hand-off through a single-instance app, precisely
because the env var travels with the launch request rather than with the
process. GTK and Chromium both honour it. Whether river's window-management
protocol surfaces the token to the WM is unknown; the port's README notes that
river "does not surface activation to the window manager" for the activation
*request*, which is discouraging but not the same question. Worth checking
before building S4 out, because if the token is visible then S4 becomes exact
and S1's cost stops being necessary.

### Per-app assignment

| App | Strategy | Identity |
| --- | --- | --- |
| Ghostty | S1 (+ S2 free) | `class=com.mitchellh.ghostty.ws-<name>` |
| Chrome | S1 via `--user-data-dir` (+ S2) | own process; `--class` if it works, else pid |
| Zed | S4, serialised | `dev.zed.Zed`, claim-next |
| Emacs, everything else | S2 | unchanged |

---

## 5. Why restart drives all of this

Under X11, `M-q` preserves placement for free: xmonad serialises the
`StackSet`, X11 `Window` ids are stable across the exec, and the new process
adopts the old layout wholesale.

Under river it does not. The new WM process makes a fresh Wayland connection
and gets fresh object ids, and the port has no channel for resume state, so
**every window is re-announced and re-runs the manage hook after every
`M-q`.** (Marked to verify — nothing in the port has run against a live river
yet. But the README's statement about extensible state implies it, and the
`StackSet` travels the same road.)

That is the whole argument for S1. If placement is a pure function of `app_id`,
this is not a problem to solve — the manage hook recomputes the same answer it
computed the first time, and `M-q` goes back to being free. If placement
depends on a pid registered ten minutes ago, or a claim that fired at spawn
time, there is nothing left after the exec and every window lands wherever the
default hook puts it.

So the ranking in §4 is not aesthetic. S1 is the only strategy that is
*stateless across restarts*, and that is worth spending a process on.

For the S2/S4 apps that remain, the fallback is a persisted
`(app_id, title-prefix, pid) → workspace` table, consulted by the manage hook
when no stronger rule matches, and pruned when a pid dies. It will be wrong
occasionally. Keep the set of apps that depend on it small.

---

## 6. Terminals: Ghostty as renderer, tmux as persistence

The requirement is "restore with history", and it is the requirement that
decides the whole terminal question.

### What Ghostty gives, and what it doesn't

Ghostty gives per-workspace instances and identity (§4, S1), `--working-directory`,
`--title`, and `-e`. It does not give session persistence on Linux. Its
`window-save-state` option ends with:

> This is currently only supported on macOS. This has no effect on Linux.

There is a `write_scrollback_file` action, but it is a keybinding action
dispatched inside a surface, with no IPC to trigger it from outside and no way
to choose the destination path. Not usable as a WM-driven capture.

So with Ghostty alone, closing a workspace destroys the pty and the scrollback,
and restore means an empty prompt.

### What tmux gives

- The pty and its processes outlive the terminal window.
- Scrollback lives in the server, so reattaching after `M-q` or after a close
  is lossless with no work at all.
- `tmux capture-pane -p -e -S - -t <session>` prints the entire history with
  escape sequences **from outside the terminal**, on demand. Nothing else here
  offers that.

That last property is decisive. It is the only mechanism in the stack by which
the window manager can snapshot history, which is what makes reboot-survival
possible rather than just restart-survival.

### The shape

Keep tmux; stop using it as a UI.

- One tmux session per terminal, named `ws-<name>-<slot>`. No tmux windows, no
  tmux panes.
- One Ghostty window per session. No Ghostty splits or tabs either.
- Multiple terminals in a workspace are multiple Ghostty windows, tiled by
  `TallWheel`. This is a tiling WM; a second multiplexer inside it was always
  redundant.

Flattening tmux this way costs almost nothing (its multiplexing UI is what
nesting breaks — true colour, undercurl, graphics passthrough, the terminal's
own scrollback and search) while keeping the one thing it is uniquely good at.

Launch:

```
ghostty --class=com.mitchellh.ghostty.ws-env \
        --working-directory=/home/mgsloan/env \
        --title=env:main \
        -e tmux new-session -A -s ws-env-main <shell-command>
```

`new-session -A` attaches if the session exists and creates it otherwise, so
the same command is both "restore" and "start fresh" — no branch, and no way
for the two paths to drift.

`Tmux.interactiveShellCommand` already seeds bash history with the launching
command so that up-arrow retrieves it; that stays, and extends naturally to
seeding the cwd.

### Reboot survival

The tmux server dies with the session. So:

- On close, and on a timer for open workspaces, run `capture-pane -p -e -S -`
  per session into `term-<session>.ansi`, truncated to the last N lines
  (start at 10k).
- On a cold start with no live server, the session's first command is
  `cat term-<session>.ansi` followed by the interactive shell.

Replaying raw captured output is an approximation, not a restoration. Colour
survives (`-e`); alternate-screen content was never in the history buffer so
there is nothing to corrupt; a stray unterminated sequence is possible, so
follow the `cat` with an SGR reset. Good-enough scrollback is the goal, not
fidelity.

### Existing system terminals

`syslog`, `errlog`, `bt`, `wifi` on tags 9 and 0 are singletons, not
workspace-scoped, and should keep working exactly as they do. They can move to
Ghostty with the default class without participating in any of this.

---

## 7. Chrome

Goal: a workspace has its browser window, closing the workspace takes it away,
reopening brings back the same tabs.

A shared instance cannot do this. Chrome's session restore is per
`--user-data-dir` and restores *all* windows of the instance at once; there is
no "restore this one window" and no supported way to read one window's tabs.
The unsupported way — CDP over `--remote-debugging-port` — is closed too:
Chrome (136 and later; 151 here) refuses remote debugging against the default
user data directory. *Verify this against 151 before relying on it either way.*

Both roads end in the same place, so take it directly: **one
`--user-data-dir` per scoped workspace**, under `~/.local/state/xmonad/workspaces/<name>/chrome/`.

What that buys:

- A separate process, so pid identification (S2) works with no new machinery.
- Chrome's own session restore does the tab persistence — closing the workspace
  quits the instance, reopening launches it with `--restore-last-session` and
  the window comes back with its tabs, their back/forward history, scroll
  positions and form state. No custom tab-list code, and better fidelity than
  custom code could reach.
- Per-workspace cookie jars, which for some workspaces is the point.

What it costs, honestly:

- A full browser process tree per open workspace. This is the most expensive
  line item in the design. It is bounded by the number of *open* workspaces,
  which is the argument for making close cheap and habitual.
- Separate logins, history and extensions per profile. Chrome sign-in/sync
  papers over bookmarks and passwords; it does not paper over "I am already
  logged into this site in the other window". Mitigate by only giving a browser
  to workspaces that want one, and leaving the main browser unscoped on tag 1.

If Chrome honours `--class` under Ozone/Wayland for the Wayland `app_id`, add
it for S1-grade restart resilience. *Unverified — Chromium's `--class` is
documented as an X11 flag and its Ozone/Wayland behaviour needs testing.*
Without it, Chrome relies on pid, which is lost across `M-q` (§5); with it,
restart placement is free.

`isAutomatedBrowser` in `Misc.hs` (which shifts test-automation browsers to tag
7 by reading `--enable-automation` out of `/proc/<pid>/cmdline`) keeps working
unchanged, and is a nice precedent: it is already S2-with-a-cmdline-predicate.

---

## 8. Zed

Zed is the easy one, because it already does most of the work.

`zed <path>` opens a project, and Zed persists per-project workspace state —
open buffers, pane layout, tabs — keyed by the project path. *Verify the extent
of what it restores.* So the record needs to hold only "this workspace had a
Zed open at path P"; Zed restores the inside of the window itself.

Instance handling: the `zed` CLI hands off to a running instance, so all
windows share a process. Options:

- **`--user-data-dir` per workspace** — partitions cleanly, and gives S2. Costs
  a full Zed process, plus duplicated extension state and settings, per
  workspace. Language servers are already per-project, so that part is not
  extra.
- **Single instance + S4 claim** — `zed -n <path>` produces exactly one new
  window, and we are the ones asking for it, so the claim is about as safe as
  a claim gets. Costs nothing.

Start with the claim. `zed -n <path>` immediately after registering the claim,
serialised so only one is outstanding. If it proves flaky in practice, fall
back to `--user-data-dir`. Title matching (S3, on the path's basename) is
available as a tie-breaker when two claims somehow overlap.

Emacs stays as it is. It is one process per window already and S2 covers it;
`M-e` just gains the workspace cwd.

---

## 9. Lifecycle

### Open

```
openWorkspace record:
  for each TerminalRecord: ghostty --class=ws-<name> -e tmux new-session -A -s ...
  for the BrowserRecord:   google-chrome --user-data-dir=... --restore-last-session
  for each EditorRecord:   claim, then zed -n <path>
```

Idempotent by construction: `new-session -A` attaches rather than duplicating,
Chrome's profile lock makes a second launch a no-op hand-off, and a claim for a
window that already exists simply expires. So `openWorkspace` on an
already-open workspace is safe, which matters because it is also the recovery
path when something dies.

### Close

Two commands, because the interesting question is what happens to the running
processes:

- **`ws-close`** — quit the Ghostty windows and let the tmux sessions detach;
  quit Chrome and Zed. The build running in a terminal survives, the memory
  spent on rendering does not. This is the default.
- **`ws-close-hard`** — as above, plus `tmux kill-session`. Use when the
  workspace is done, not paused.

Both capture scrollback first (§6), then write the record, then kill windows.
In that order: if killing goes wrong, the record on disk is already correct.

### Restart (`M-q`)

Nothing to do, if §4/§5 hold. Windows re-announce, the manage hook re-derives
placement from `app_id`, and the records on disk are already current because
they are written through. The only work is re-reading `meta.json` for each
workspace into memory.

### Cold start

Detect via the on-disk boot-id marker (§2), not `SessionStart`. Then open every
record with `wrAutoOpen`, replacing the hardcoded
`startupInitialApplications`. Keep the existing screen-configuration branch —
which workspace gets the browser on a single screen versus a big screen — as a
property of the records rather than a `case` in `xmonad.hs`.

---

## 10. Commands and bindings

New entries in the `M-x` action prompt, which is where anything without daily
muscle memory belongs:

| Action | Effect |
| --- | --- |
| `ws-scope` | Prompt for a directory; bind it to the current workspace |
| `ws-unscope` | Drop the record; the tag goes back to being an ordinary tag |
| `ws-close` | Quit windows, keep tmux sessions and the record |
| `ws-close-hard` | Quit windows and tmux sessions, keep the record |
| `ws-open` | Prompt for a name; open it into the current tag |
| `ws-rename` | Rename; renames the tmux sessions and state directory with it |
| `ws-add-browser` / `ws-add-editor` | Add a component to the current workspace |

`M-p` (shell prompt) and `M-S-<Return>` need no new binding — they inherit the
cwd through `spawnOn` (§3). That is the point of putting the path in the spawn
path rather than in each call site.

---

## 11. What is persisted

| Persisted | Not persisted |
| --- | --- |
| Path, name, tag binding, auto-open | Window geometry |
| Terminal list: session name, title, last cwd | Focus order within a workspace |
| Scrollback capture per terminal | Layout choice and master ratio |
| Browser presence and profile dir | Which screen a workspace was on |
| Editor project paths | Floating window state |

The right-hand column is deliberate. Every one of those is either cheap to
re-establish or actively better re-derived — a workspace restored onto a
different screen configuration should lay out for the screens that exist, not
for the ones that existed last week. Layout and master ratio are the only
plausible additions later, and only if their absence is actually irritating.

---

## 12. Costs

Stated plainly, because they are real:

- **Memory.** A Ghostty instance and a Chrome instance per open scoped
  workspace. This is the design's central trade: it buys stateless restart
  placement and free tab restore, and it is bounded by open workspaces rather
  than total workspaces. Close being cheap is what makes it tolerable.
- **A second copy of a lot of browser state.** Separate cookies per workspace
  is a feature for some workspaces and a nuisance for the rest. Don't scope the
  browser on workspaces that don't want it.
- **tmux stays.** Not the outcome a switch to Ghostty was hoping for, but the
  alternative is losing history on close, and history on close is the
  requirement.
- **The manage hook gets more rules.** Bounded by the number of scoped
  workspaces, generated rather than written by hand.

---

## 13. To verify before building

Ordered by how much of the design falls over if the answer is bad.

1. **Does river re-announce every window to a freshly exec'd WM, and does the
   manage hook run for each?** §5 rests entirely on this. If placement *does*
   survive `M-q` some other way, S1's cost stops being justified and S4 becomes
   attractive across the board.
2. **Does `xdg-activation-v1`'s token reach the window manager under river?**
   If yes, S4 becomes exact, and §7's per-workspace Chrome profile is no longer
   forced by identification (though still attractive for session restore).
3. **Does Chrome honour `--class` for the Wayland `app_id` under Ozone?**
   Decides whether Chrome gets S1 or is stuck on pid.
4. **Does Chrome 151 still refuse `--remote-debugging-port` on the default
   user-data-dir?** Only matters if the per-profile approach is rejected.
5. **How much does Zed actually restore per project?** If it restores less than
   assumed, `EditorRecord` needs to carry open file paths itself.
6. **`capture-pane` replay fidelity** against a realistically messy scrollback
   — progress bars, `ccze` output, half-drawn TUIs.
7. **Cost of N Ghostty instances** — RSS and GPU contexts for 4–5 simultaneous
   instances, measured rather than guessed.

---

## 14. Implementation order

Each step is independently useful, which matters because the verification list
above may redirect the later ones.

1. **The path, and nothing else.** `WorkspaceRecord` with only `wrName`/`wrPath`,
   JSON store, `spawnOn` propagating cwd, `ws-scope`. No persistence of
   contents, no close. This alone is most of the daily value.
2. **Fix the session-start marker** (§2). Independently necessary under river,
   and step 3 depends on telling cold start from restart.
3. **Terminals.** Ghostty with per-workspace `class`, flattened tmux,
   `new-session -A`, generated manage-hook rules. Proves S1 end to end.
4. **Close and reopen**, terminals only. Add `capture-pane` on close and
   replay on cold start.
5. **Chrome**, per-workspace profile.
6. **Zed**, claim-based.
7. **Cold-start restore** replacing `startupInitialApplications`.
