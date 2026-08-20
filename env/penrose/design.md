# Penrose on X11: design notes

Porting `~/env/src/xmonad.hs` and its modules to
[penrose](https://github.com/sminez/penrose), vendored at
`vendor/penrose-pinned` (§1), X11 backend only. Penrose source references below
are relative to that; xmonad ones to `~/env/src`.

**Scope.** The whole configuration, less the three modules listed at the end.
§1–§10 are the window-management half — layouts, bindings, workspaces, screens,
focus, manage rules, restart, prompts, placement — and are built. §11–§20 are
the other ~1600 lines: the environment and process layer (§11–§14) and the
custom actions on top of it (§15–§20). Both are built. What is deliberately not
ported is listed at the end.

The split matters because the two halves fail differently. The first is about
penrose's model and where it differs from xmonad's; the second is a
Haskell→Rust rewrite of code penrose has no opinion about, whose only real
coupling to the window manager is §11 (where the shared environment lives), §12
(child processes, which penrose actively breaks) and §14 (state that has to
survive `M-q`).

**X11 is the target**, not a waypoint on the way to Wayland, so the X11-only
pieces are implemented rather than skipped: keynav, xidlehook, the root cursor
(`xsetroot -cursor_name left_ptr`, standing in for `setDefaultCursor`, which
penrose has no API for), and the lock the session starts behind (§21). Each has
a Wayland replacement noted in river-design.md for whenever that happens; none
of them is a reason to be worse here in the meantime.

Two stay out. `gnomeRegister` is XSMP, which penrose has no support for and
which buys little. The xrandr screen configuration matches output names —
`eDP-1-1`, `DP-0.8`, `DP-0` — that no longer exist on this machine, which now
reports `eDP-1` and `DP-1`..`DP-3`, so porting it verbatim would detect nothing
and do nothing. It needs the topology redone rather than translated.

## What comes for free

`StackSet`/`Stack` is the same data structure with the same operations
(`focus_up/down`, `swap_up/down`, `focus_tag`, `move_focused_to_tag`,
`swap_focus_and_head` = `dwmpromote`, `next_screen`, `float`/`sink`). Hooks are
the same five (startup, event, manage, refresh, layout). `Layout` is a trait
with typed messages (`IncMain`, `ExpandMain`, `ShrinkMain`). Extension state is
`State::extension::<T>()` over an anymap. Errors from handlers are logged by
the run loop, so the config's `printErrors`/`printHandlerErrors` wrappers
disappear.

`warpMid` also comes free, which is not obvious: `handle_pointer_change`
(`src/core/conn.rs:515`) warps the pointer to the focused client whenever a
refresh changed focus or moved the focused window, gated on
`config.focus_follow_mouse` and skipped for `Enter` events (so no feedback
loop). Setting `focus_follow_mouse: true` — xmonad's default, which this config
does not override — reproduces the `warpMid` wrapper without wrapping anything.

---

## 1. Crate layout and launch

A standalone cargo project in `~/env/penrose/`, with penrose itself vendored
beneath it:

```
~/env/penrose/
  Cargo.toml              -- penrose = { path = "vendor/penrose-pinned" }
  src/
  scripts/
  vendor/penrose-pinned/  -- submodule, github.com/mgsloan/penrose: what the build uses
  vendor/penrose/         -- plain clone of the same fork, gitignored: where the library is hacked on
```

Pinning by submodule follows the existing pattern for dependencies that get
patched: `env/xmonad`, `env/xmonad-river`, `oss/waynav` and friends are all
submodules pointing at personal forks, with the home repo pinning the SHA
rather than a rev in `Cargo.toml`.

The build depends only on `vendor/penrose-pinned`, so hacking on penrose in
`vendor/penrose` never changes what this window manager is built against — and
that checkout is deliberately *not* a submodule, since its state is scratch
work rather than something the home repo should pin. Moving to a newer penrose
is a deliberate act: check the pinned submodule out at the new commit, rebuild,
and commit the SHA that moved.

Track `develop`, not a release. The `Conn`/`XConn` split and the
`KeyBindingKey` associated type both landed after 0.4.0, and §5 depends on the
first of them. The pinned copy sits on `key-binding-sequences`, which is
`develop` plus the key sequences in §3 — a PR rather than a private patch.
Nothing else here needs a fork-local change, and keeping it that way is what
makes following `develop` a fast-forward rather than a rebase.

Cargo needs nothing special for the nesting: the pinned copy declares its own
`[workspace]` (members `penrose_ui`, `penrose_keysyms`), which keeps it
independent of the outer package, and a path dependency into it resolves
normally. `target/` is gitignored.

`setup-scripts/045-build-penrose.sh` covers fresh machines — pinned submodule
init and build — and `044-create-penrose-xsession.sh` installs
`xsessions/penrose.desktop.template`, paralleling the xmonad pair. The session
`Exec` is `scripts/run-penrose.sh` (§2), not the binary, and
`scripts/rebuild-penrose.sh` builds and installs to `~/.local/bin/penrose-wm`.
Both WMs stay installed and selectable from GDM during the transition.

## 2. Restart (`M-q`)

`M-q` rebuilds on a thread and, on success, exits the process outright
(`std::process::exit` from the thread — there is no state to flush). The
xsession script runs the binary in a `while` loop and relaunches it. No
serialization, no exec-self, no channel back into the event loop.

This works because penrose uses **the X server as the state store**.
`manage_existing_clients` (`src/x/mod.rs:475`) re-adopts every existing client
on startup, reading `_NET_WM_DESKTOP` to put it back on its tag and
`_NET_ACTIVE_WINDOW` to restore focus. Tags and focus survive a restart, so
long as `add_ewmh_hooks` is running to have written those properties — which
is why EWMH is mandatory here (§7).

What does not survive, worth knowing before the first `M-q` is surprising:

- **Layout state** — each workspace's layout choice and TallWheel's
  `ratio`/`max_main` reset to defaults. This one is accepted rather than worked
  around: penrose's `Workspace` holds a `LayoutStack` of `Box<dyn Layout>`,
  which Rust cannot serialize for free (`StackSet` and `Workspace` are
  accordingly not `Serialize`, only `Stack<T>` and the geometry types), so
  there is no cheap version of what xmonad gets from `LayoutClass`'s
  `Read + Show` superclasses.
- **Stacking order**, per the `run()` doc.
- **Clients on hidden workspaces**, which `manage_existing_clients` skips
  deliberately, since extensions may own them.
- **Extension state** — no `PersistentExtension` equivalent, so anything kept
  there is rebuilt from defaults.
- The workspace holding the previously active client lands on the first
  available screen rather than the one it was on.

### Exit codes

The loop makes exiting ambiguous, and one case needs rescuing: gdm-x-session
runs the window manager *as the session script*, so under xmonad `logout` is
just `exitSuccess` — the session ends because the script did. Wrapped in a
loop, that same exit relaunches the WM instead.

So the exit code is the signal:

| code | meaning | loop does |
|---|---|---|
| `0` | restart (`M-q`) | relaunch |
| `42` | logout | `break`, ending the X session |
| other | crash | preserve the log, relaunch |

The `logout` action calls `exit(42)`; `M-q` calls `exit(0)`. Crash handling
mirrors `my-penrose-config`'s `bin/run-penrose`, which already branches on `$?`
to save the previous log.

The script exports `RESTARTED=true` on each iteration, which is what lets the
startup hook keep the config's `everyRunAction` / `initialStartupAction` split,
since penrose has no equivalent of xmonad's `handleStartup`. The every-run half
is thin here: the root cursor, and the hourly background thread (§19).

## 3. Key bindings

Penrose parses `"M-S-semicolon"`-style strings against `xmodmap -pke`
(`parse_keybindings_with_xmodmap`) into keycodes. `XF86Audio*` names resolve
through xmodmap, so the media keys port literally. The `map!` macro plus a loop
over tags gives roughly the shape of the current `keymap` list.

Parsing to keysyms instead, and letting the backend resolve them when grabbing,
would be better and is deferred rather than rejected: the `HashMap<String, u8>`
built from `xmodmap -pke` binds only one key when a keysym lives on two, and
parsing needs the `xmodmap` binary and a display. Names are the same text under
either scheme, since `xmodmap` prints keysym names, so switching later does not
touch this keymap.

Sequences (`M-m M-l`, `M-m M-m`, `M-m M-d`, `M-b M-g`) are binding strings too,
in the same `map!` block and the same style as `XMonad.Util.EZConfig`. Penrose
had no submap concept, so this was written as a library feature on the
`key-binding-sequences` branch (§1) rather than as a config-local hook, and
offered upstream as the PR for penrose#260. A click does not cancel a
half-typed sequence, as it does under xmonad: only the keyboard is grabbed,
because on Wayland a compositor cannot swallow the click and the two backends
would behave differently under one name (river-design.md §11).

The cost to the config is two methods — `capture_next_key` and
`cancel_capture_next_key` are required on `Conn`/`XConn`, so `PhysConn` (§5)
forwards them like everything else.

Nothing binds a sequence yet: three are Spotify (§16) and one is backgrounds
(§19), so they arrive with those. `unusedAlphaLeaders` prints the unbound
`M-<letter>` leaders at startup, which is how the config keeps room for new
ones; port it as a startup-hook log line over the parsed keymap.

## 4. Layouts

`TallWheel 1 (phi/8) phi ||| Full`, i.e. `stack!(TallWheel, Monocle)` with
`ratio = 0.61803`, `ratio_step = phi/8`, `max_main = 1`.

**TallWheel** (`src/TallWheel.hs`, 52 lines) is `Tall` with the master column's
order reversed, so windows rotate through a wheel; `src/layout.rs` is the port,
a `Layout` impl handling `IncMain`/`ExpandMain`/`ShrinkMain`. **Full** is
penrose's `Monocle`; `NextLayout` is `cs.next_layout()`.

No `focusTracking` equivalent. It exists in xmonad so that a layout which
reorders windows does not lose focus (and so floats are not buried); penrose
keeps focus in the `Stack` itself rather than anywhere a layout could drop it,
so the workaround should be unnecessary. If focus does misbehave,
`LayoutTransformer` is where a replacement goes.

## 5. Screens

Screen indices must be **physical**, ordered right-to-left by `(x, y)`
descending — `XMonad.Actions.PhysicalScreens` semantics, matching the current
`screenOrder` comparator. Penrose indexes screens in whatever order
`conn.screen_details()` returns.

Fixed once, at the connection: `impl<X: XConn> Conn for X` means
`PhysConn(RustConn)` need only implement `XConn` by forwarding everything and
sorting `screen_details()`. Every screen index in the config is then physical,
including inside penrose's own code, and the newtype stays available as a seam
for anything else needing interception.

`focusScreen`/`moveToScreen` become `cs.focus_screen(n)` /
`cs.move_focused_to_screen(n)`.

Tag switching wants **`pull_tag_to_screen`, not `focus_tag`**. `focus_tag` is
xmonad's `W.view` — if the tag is already visible on another screen, focus
moves to that screen. `pull_tag_to_screen` is `W.greedyView`, which swaps the
tag onto the active screen, and `greedyView` is what `focusWorkspace` uses
today. So `M-<tag>` is `pull_tag_to_screen`, and `M-C-<tag>` ("that tag, on the
other screen") is `modify_with(|cs| { cs.next_screen(); cs.pull_tag_to_screen(tag) })`.

`detectScreens`/`configureScreens` shell out to `xrandr` and stay as they are;
penrose picks up the resulting change via randr events.

## 6. Manage rules and placement

`manage_hooks!` plus `Query` (`Title`, `AppName`, `ClassName`,
`StringProperty`) and `FloatingCentered`/`SetWorkspace` covers `title =?
"Desktop" --> doShift "0"` and the float rules directly. Two are custom `Query`
impls in `src/manage.rs`:

- `IsDialog` — `_NET_WM_WINDOW_TYPE`, falling back to `WM_TRANSIENT_FOR` for
  older clients that set no window type at all.
- `AutomatedBrowser` — an automated Chrome is indistinguishable from a normal
  one by its window properties, so this reads `/proc/$pid/cmdline` for
  `--enable-automation`, with `Conn::client_pid` supplying the pid.

`ClassName` matches the *second* string in `WM_CLASS`, the class; `AppName`
matches the first, the instance. `alacritty --class NAME` sets both, so the
terminals match either way, but a program that names itself matches only on its
own spelling — Spotify's instance is `spotify` and its class is `Spotify`, and
`ClassName("spotify")` therefore silently placed nothing.

**Placement is class-based, not pid-based.** `spawn_on` has no equivalent
anywhere in penrose or the configs built on it; the community answer is
`ClassName("discord") => SetWorkspace("9")`, and that covers this startup once
each terminal gets a class of its own: `alacritty --class syslog` (and
`errlog`, `bt`, `wifi`) makes the log terminals on 9 and the wireless terminals
on 0 ordinary `manage_hooks!` rules. Emacs, Chrome and Spotify are single
instances with distinct classes already — though only Spotify has a rule, since
a class rule applies to *every* window of that program, and yanking each new
Chrome window to one tag is not what `spawnOn` did. No pid tracking, no
`_envPidHooks`-equivalent table, and the rules are declarative in one place
rather than spread across the spawn sites.

The case this cannot express is placing a *specific instance* of a program that
shares its class with other instances — which, with `--class` available, means
programs whose class cannot be set from the command line. Nothing in the
current startup falls into that category. If something later does, the fallback
is a `Mutex<HashMap<u32, String>>` in extension state plus a manage hook
matching `client_pid` and its parent chain (~60 lines; the chain walk matters,
since `alacritty -e tmux` and Chrome's zygote do not surface the spawned pid).

## 7. EWMH and activation

`add_ewmh_hooks` runs: its refresh hook writes the `_NET_WM_DESKTOP` and
`_NET_ACTIVE_WINDOW` properties that restart depends on (§2), and it brings
`_NET_WM_STATE_FULLSCREEN` handling.

Its event hook, though, focuses the client and switches tags on an incoming
`_NET_ACTIVE_WINDOW` message (`src/extensions/hooks/ewmh.rs:121`) — exactly
what `setEwmhActivateHook doAskUrgent` exists to prevent, since Chrome sends
one on startup. `src/ewmh.rs` composes an event hook *before* it that returns
`false` for that message — independent of the outgoing property writes, so the
two compose cleanly.

Chrome then stops stealing focus, and something has to answer the question it
was asking. **Penrose has no urgency concept at all** (`grep -rn urgen src`
finds nothing), so `urgency.rs` is it: the refused request is recorded in
extension state and announced with a notification, which is `doAskUrgent`
without a status bar to render it in. `M-x goto-urgent` focuses the most recent
one, and a refresh hook clears whatever is focused, since focus is the only
definition of "seen" available here.

A window may also ask by setting `_NET_WM_STATE_DEMANDS_ATTENTION` itself,
which penrose does not act on at all; that is the same request and gets the same
answer. What is *not* covered is the ICCCM route, `WM_HINTS`'s urgency flag:
penrose parses it into `WmHints` but the field is `pub(crate)` with no accessor,
so a config cannot read it. Five lines upstream would fix that.

## 8. Floating and mouse

Penrose's built-in handlers (`src/builtin/actions/floating.rs`), on the windows
key: `M-button1` → `MouseDragHandler`, `M-button3` → `MouseResizeHandler`.

This drops `FlexibleManipulate`, where a single `M-button1` drag moved or
resized depending on which ninth of the window it started in. Move and resize
are now separate buttons. `M-t`/`M-S-t` map to `sink_focused`/`sink_all`
directly.

## 9. Prompts

**rofi**, shelled out to. Penrose has no real equivalent of `XMonad.Prompt` —
`crates/penrose_menu` is not in the workspace `members` list in `Cargo.toml`,
i.e. unbuilt and unmaintained — and its `DMenu` helper is no help either, since
it execs `dmenu`/`dmenu_run` by name and only speaks dmenu's flags. So
`menu.rs` wraps rofi directly: options on stdin to `rofi -dmenu -i -p`,
selection back on stdout, `None` when cancelled. `M-p` is `rofi -show run`,
which brings its own history; `M-x` builds a menu from a list of named actions.

Three of the custom actions want a prompt with no completions at all — note
text (§17), byzanz arguments (§18) — which is the same wrapper called with an
empty option list, since `rofi -dmenu` returns whatever was typed.

rofi over dmenu because it also has a Wayland fork, so the choice survives a
move off X11 — settled, rather than provisional: what is given up is the emacs
keymap, the `gsettings`-driven light/dark switch, and per-prompt history for
anything other than `-show run`, and none of those is worth writing a prompt
implementation for.
The light/dark switch has a rofi-shaped replacement if it is missed: darkman
already runs at startup and can swap a rofi theme, rather than the prompt
asking `gsettings` on each invocation as `getColorScheme` does.

## 10. Asynchronous work

Handlers are synchronous `FnMut(&mut State<C>, &mut C)`, so anything slow (the
whole `Xio`/`forkXio` half of the config) runs on a thread, and penrose offers
no channel back into the loop.

That shapes every custom action into the same two steps: **read what you need
from `State` on the handler thread, then move an owned copy onto a thread.**
The focused window's title, its pid, the current tag — all cheap, all available
synchronously. Everything after that (HTTP, `amixer`, reading `/proc`, waiting
on a script) happens where it cannot stall the event loop. This is not a
restriction the Haskell config escapes either: `Xio` exists precisely because
`X` is a `StateT` and cannot be run concurrently.

Nothing in the config needs the reverse direction. `M-q` exits the process, and
no action mutates the client set from a thread. If one ever does, the X11
answer is xmonad's: the thread sends a client message to the root window
(`XConn::send_client_message`) and an event hook picks it up — the
`sendRestart` trick from `xmonad.hs:242`, about twenty lines.

---

# The rest of the configuration

Dependency order: §11 and §12 are what the rest is built on.

## 11. Environment, secrets and threads

`Monad.hs` carries an `Env` through `ReaderT` in both `XX` (with the window
manager's state) and `Xio` (without it). Rust needs the same split for the same
reason, but gets it from ownership rather than from two monads: a handler has
`&mut State<Conn>`, a thread cannot have it, so anything a thread needs must be
in a value it can own a handle to.

So `Env` becomes a plain struct behind an `Arc`, built once in `main` and
reachable from anywhere:

```rust
static ENV: OnceLock<Arc<Env>> = OnceLock::new();
```

A `static` rather than penrose's extension state, deliberately.
`State::extension::<T>()` is the natural home for config state, but it is
reachable only through `&mut State`, which is exactly what a spawned thread
does not have. Extension state is still right for anything the *event loop*
owns and mutates (§14); `Env` is the read-mostly rest: home directory,
`systemd-cat` availability (§12), the bluetooth UUIDs, the Spotify credentials
and cached access token (§16), the backgrounds list (§19).

Secrets and device IDs are read once at startup from `~/env/untracked/` —
`headphones.uuid`, `receiver.uuid`, `spotify.client_id`, `spotify.client_secret`,
`spotify.refresh_token` — each optional, each logging an error naming the
missing file when absent, so a fresh machine degrades to "that binding does
nothing and says why" rather than failing to start. That is what `readUuid` and
`readToken` do today, and it is worth keeping verbatim: the failure is
otherwise invisible until a keypress does nothing.

Two things from `Monad.hs` have no successor. The `debug` trace helper exists
to log from pure code without `IO`; `tracing::debug!` needs no such excuse.
`printErrors`/`printHandlerErrors` wrap every binding to log what it threw —
penrose's run loop already logs handler errors, as noted at the top.

## 12. Spawning, logging and tmux

Every process the config starts goes through `systemd-cat`, so its output lands
in the journal tagged and at the right priority — which is what makes the
`syslog`/`errlog` terminals (§6) worth having. `systemdCatArgs` is
`--level-prefix=false --stderr-priority=err`, the second of which comes from a
personal systemd patch, so `checkSystemdCatWorks` runs one test invocation at
startup and falls back to spawning directly if it fails. Port as-is: a `spawn`
helper in `src/process.rs` that consults a flag in `Env` (§11).

**Nothing here may wait on a child process.** `WindowManager::run` sets
`SIGCHLD` to `SIG_IGN` unconditionally and panics if it cannot
(`src/core/mod.rs:422`), so the kernel auto-reaps children and `waitpid`
returns `ECHILD`. Every form of waiting fails:

```
status():      Err(Os { code: 10, message: "No child processes" })
output():      Err(Os { code: 10, message: "No child processes" })
spawn+wait():  Err(Os { code: 10, message: "No child processes" })
```

That is measured, not inferred, and it is why `M-q` currently reports a failed
rebuild even when the build succeeds. The xmonad config met the same wall and
patched it out locally (`XMonad/Core.hs:882`, *"mgsloan modification to allow
for waiting for processes"*).

**The port keeps the signal ignored and reads stdout to EOF instead.** A pipe
reaches EOF when the child exits, whether or not anything can wait for it, so
output is available without the process table — which is penrose's own
sanctioned workaround, documented on `util::spawn_for_output`
(`src/util.rs:60`). That is `process::read_output`, and it covers
`syncSpawnAndRead` wholesale: amixer, xclip, gsettings, xrandr, gist.

**That splits the helper in two, and they cannot be merged.** `systemd-cat`
works by *being* the process that owns the child's stdout, which is exactly the
pipe the capture path needs to read. So logged fire-and-forget spawns run under
`systemd-cat` and their output goes to the journal, while spawns whose output
the config reads run bare and their output goes to the caller. The Haskell
config already draws the line in the same place — `syncSpawnAndRead` uses
`proc` directly while everything else goes through `loggedProc` — which is
worth knowing before trying to unify them.

Exit codes are the only casualty, and where one is genuinely needed — `M-q`'s
rebuild, `spawnAndNotifyFail` — the child reports it itself. `process::status`
runs `sh -c 'systemd-cat … "$0" "$@"; echo "__penrose_rc=$?"'`, so the
command's own output still reaches the journal and the only thing on the pipe
is the marker line. Passing the command through as `"$0" "$@"` rather than
interpolating it into the script text means nothing needs escaping.

The wrapping shell earns its place twice over, because an ignored disposition
is inherited across `exec` where a handler is not, so anything launched
directly from the window manager starts life unable to wait for *its* children;
a shell installs its own `SIGCHLD` handling and launders that away.

`status_survives_sigchld_being_ignored` checks both halves of this — that
`Command::status()` fails under the disposition, and that these helpers do not.
It is `#[ignore]`d because the disposition is process-wide and would otherwise
break penrose's own `xmodmap` call in the binding test; run it with
`cargo test -- --ignored sigchld`.

The alternative — a `Config` field to turn the signal off, plus the config
reaping its own children — was considered and rejected. It is not the 15 lines
it looks like: a blanket `waitpid(-1)` reaper races with any targeted wait and
would randomly steal exit statuses, so it means routing every spawn through one
owner that always waits, while any spawn that escapes it (penrose's own
`util::spawn`, any extension) leaks a zombie permanently. The Haskell config
demonstrates the cost of that discipline: 61 defunct children after eleven
hours of uptime, recorded in `~/env/todo.md`.

The rest of the layer ports directly:

- `spawnOn`/`spawnAndDo`/`manageSpawn` — gone, replaced by class-based
  placement (§6). This is the largest single deletion: the pid table, the
  100 ms MVar handshake to learn the child's pid, the 10-second expiry, and the
  parent-chain walk in the manage hook all disappear.
- **tmux terminals** (`Tmux.hs`) — `kill-session` then
  `alacritty --class NAME -e tmux new-session -s NAME -n NAME <cmd>`, where
  `<cmd>` is `bash --init-file <(echo "history -s <cmd>; <cmd>")`: the shell
  outlives the command, and the command is in the history, so ↑ re-runs it.
  That trick needs `Escape.hs`'s bash quoting, which is ~15 lines by hand or
  the `shell-escape` crate. The workspace argument these took is now the
  `--class`.
- `showLogsOfFocusedWindow` (`Logs.hs`) — `Conn::client_pid` for the focused
  window, then walk `/proc/$pid/stat` field 4 upwards to the window manager's
  own pid, and open a terminal running `journalctl --boot --follow` with a
  `_PID=` filter per pid. The parent walk is the same code `AutomatedBrowser`
  (§6) needs a single step of.

## 13. Notifications

`notify-send -i ~/env/xmonad.png <title> <msg>`, already in `src/actions/mod.rs`, moving to `src/notify.rs`.
Two variants beyond it: `notifyTruncated` cuts the body at 300 characters (the
clipboard actions in §17 echo what they captured, and a clipboard can be a
megabyte), and `dunstToggle` is `notify-send DUNST_COMMAND_TOGGLE`, a control
message rather than a notification.

Dunst owns `M-n`, `M-S-n` and `` M-` `` for clearing and history. Nothing in
the keymap may use them.

## 14. State that must survive a restart

Three toggles are `PersistentExtension` in xmonad, i.e. serialized into the
state file and read back after `M-q`: redshift on/off (`RedShift.hs`), touchpad
on/off (`Touchpad.hs`), and the workspace-switch lock (`Focus.hs`). Penrose has
no equivalent (§2), and `M-q` is a frequent operation here, so without
something they reset on every rebuild: redshift restarts, the touchpad
re-disables itself, and a focus lock silently unlocks.

Decided: one small JSON file, `~/.local/state/penrose/toggles.json` (or
`$XDG_STATE_HOME`), read in the startup hook into extension state and rewritten
on each toggle — via a temporary file and a rename, since the one moment a
truncated file would hurt is the startup that reads it. The
alternative — deriving each from the world (`pgrep redshift`, `synclient -l`) —
is more honest for two of the three and impossible for the lock, which exists
only in the window manager's head, so a file for all three keeps one mechanism
rather than two and a half.

The lock is the one with teeth: `focusWorkspace` checks it and, when locked,
notifies `FOCUS!` instead of switching. That check lives in the `M-<tag>`
binding (§5), the only route to a tag switch, which is why that one binding is
`actions::focus_tag` rather than a plain `modify_with`.

Applying them at startup follows the split the xmonad config already had:
redshift is started from its stored value once per session, and the touchpad is
forced off at the start of a session but left alone across a rebuild — so a
restart does not re-disable a touchpad that was deliberately turned on.

## 15. Audio, brightness and media keys

`amixer set Master <arg>` for volume and mute, `amixer set Capture toggle` for
the microphone, each followed by a notification showing the last line of
amixer's own output — which needs to read a child's stdout, so it depends on
§12. Bindings are `M-f`/`M-S-f`/`M-S-d`/`M-d` (max, up, down, toggle) and the
`XF86Audio{RaiseVolume,LowerVolume,Mute,MicMute}` keys, which resolve by name
(§3). Volume up and down unmute first, deliberately.

Brightness is four bindings onto `env/scripts/brightness-{increase,decrease,set}.sh`,
unchanged.

`XF86AudioPlay` is the interesting one: it reads the focused window's title and,
if it looks like a video in Chrome (Netflix, a YouTube/Prime/Coursera suffix),
sends `space` to that window with `xdotool` and pauses Spotify; otherwise it
toggles Spotify. Reading the title is a `State` read, so it happens on the
handler thread and the rest goes to a thread (§10). The original routes xdotool
through a terminal because running it directly did not work; worth retrying
directly in the port, since `Conn::client_pid`'s sibling
`xdotool getwindowfocus` is the part being worked around and a window id from
`State` can be passed with `--window` instead.

## 16. Spotify

Two transports, chosen by `SPOTIFY_NO_DBUS`. The default is `dbus-send` to
`org.mpris.MediaPlayer2.spotify` — fire-and-forget, no reply parsing, works
only against the local desktop client. With `SPOTIFY_NO_DBUS=true` the Web API
takes over, which is what makes volume, liking a track and "what is playing"
possible at all, since MPRIS exposes none of them the way this config wants.

The Web API path needs three things Rust does not have for free:

- **An HTTP client.** `ureq` over `reqwest`: blocking, no tokio, and every call
  here already runs on its own thread (§10). Pulling an async runtime into a
  window manager for six endpoints would be the wrong trade.
- **JSON.** `serde_json` with pointer lookups (`/item/id`, `/item/name`,
  `/item/artists/*/name`, `/device/volume_percent`, `/is_playing`) rather than
  typed structs — the config only ever reaches for five fields out of large
  responses, and `lens-aeson` is doing exactly that today.
- **Token refresh.** Client id, secret and refresh token from `~/env/untracked/`
  (§11) are exchanged for an access token at
  `accounts.spotify.com/api/token`, cached with its expiry minus five seconds
  in a `Mutex<Option<(Instant, String)>>` in `Env`, and refreshed on demand.

Endpoints in use: `GET me/player`, `PUT me/player/play|pause|volume`,
`POST me/player/next|previous`, `PUT me/tracks?ids=`.

Bindings: `M-m M-l` (like current track), `M-m M-m` (toggle), `M-m M-d` (log
player info), `M-<Left>`/`M-<Right>` (previous/next), `M-<Up>`/`M-<Down>`
(volume ±5), `M-S-<Up>`/`M-S-<Down>` (100/0), `M-S-/` (notify current track).
The three `M-m` sequences are the reason §3 exists. `spotify-clear-cache` is a
menu entry (§20) that removes `~/.cache/spotify` and the snap equivalent, a
workaround for share links failing.

## 17. Notes, clipboard and window context

`M-a` appends a line to `docs/obsidian/notes.md`, `M-S-a` appends the clipboard
under it, `M-y` copies the same context back to the clipboard. All three are
built on one function: a timestamp plus a phrase describing what was focused.

The phrase comes from four regexes over the window title — Chrome with a page
title and URL, Chrome with a URL alone, Emacs with a path, Obsidian with a note
name — producing `While browsing [title](url)`, `While editing file://path`,
`With focus on [[note]]`, or a bare `With focus on 'title'`. The `regex` crate
covers all four; the Haskell uses `rex` quasiquotes, so the patterns transfer
character for character. A date library is needed for
`[[%Y-%m-%d]] %H:%M:%S` — `jiff` or `chrono`, either is fine and neither is in
the tree yet.

The clipboard is `xclip -o` with a 100 ms timeout, and appending notifies with
the text truncated (§13). The prompt is a rofi prompt with no completions,
which §9's wrapper does not do yet: `rofi -dmenu` with an empty option list
returns whatever was typed, so it is one extra entry point rather than a new
mechanism.

`Todoist.hs` is not ported. Its binding is already commented out in the keymap
with a note to bring it back "once it's reliable", and nothing else reaches it.

## 18. Capture and sharing

- **`M-r`** — `flameshot gui --accept-on-select --clipboard --path
  ~/pics/screenshots/`.
- **`M-S-r`** — prompt for a duration (default `10` seconds; under X11 it is
  passed through as byzanz arguments), then a timestamped `.gif` into
  `~/pics/screencaps` via `env/scripts/byzanz-record-region.sh`, opened in the
  browser when it finishes. Another no-completion prompt (§17).
- **Menu entries** — `screenshot-ocr` and `usb-reset`/`bluetooth-reset` are
  script spawns; `gist-hs`/`gist-md`/`gist-txt` run `gist -P -p -f <name>` over
  the clipboard and `xdg-open` the URL it prints, so they need output capture
  (§12).

Startup creates `~/pics/{screenshots,screenshots-large,screencaps}`, which is
two lines and prevents each of the above failing on a fresh machine.

**On Wayland**, all three of these change program, and the first changes shape
with it — the one place in §18 where the backend is visible above `programs.rs`.
There is no Wayland flameshot to swap in: flameshot 14 captures a Wayland
session through `org.freedesktop.portal.Screenshot`, river's session implements
no such interface, and `M-r` hung on a dbus call nothing was going to answer.
Putting it on Xwayland instead captures the Xwayland root, which under a
rootless server holds none of the session.

So `M-r` becomes `slurp` for the selection, `grim -g` for the capture, and
`wl-copy --type image/png` for the clipboard — three programs in sequence where
flameshot was one, two of which have to be waited for. That is why
`capture::screenshot` runs on a thread on both backends now: a handler that
blocks would block the compositor events `slurp`'s overlay needs to draw
itself, and the two would wait on each other. A cancelled selection is empty
output from `slurp` rather than a status, for the reason everything else here
observes children through their pipes (§12). What is lost is flameshot's
annotation UI, which `satty` or `swappy` would put back.

`M-S-r` picks a script per backend rather than one script with a branch:
`wf-record-region.sh` takes the same duration and output path and produces the
same gif, but by way of `wf-recorder` into an mp4 and `ffmpeg` into the gif,
where byzanz wrote one directly. `screenshot-ocr.sh` does branch internally,
since it is one pipeline whose ends change — `slurp`/`grim` and `wl-copy` for
`maim` and `xsel` — and it is shared with the xmonad config, so a second copy
would be two places to fix.

## 19. Backgrounds and bluetooth

**Backgrounds.** A random `.jpg` from `env/untracked/backgrounds`, applied with
`feh --bg-scale`: once at startup, then hourly from a thread that sleeps an
hour and repeats, plus `M-b M-g` on demand. The file list is cached in `Env`
behind a `Mutex<Option<Vec<PathBuf>>>` and rebuilt by the
`update-backgrounds-list` menu entry, since walking the directory on every
change is pointless when it changes monthly. `bg-white` sets
`env/solid_white.png` instead, for screen sharing.

**Bluetooth.** `connect <uuid>` / `disconnect <uuid>` typed into the
`bluetoothctl` session by `tmux send-keys -t bt`, with the UUIDs read at
startup (§11). It works only because startup keeps that tmux session alive
(§6), which the original notes as a TODO — the port inherits the same
fragility, and the same one-line failure mode when the session is not there.

## 20. The `M-x` menu

The action menu is the config's escape valve: anything too rare for a binding
goes here. Ported entries, and where each lives:

| entry | §  |
|---|---|
| `touchpad-toggle`, `redshift-toggle`, `lock`, `unlock` | §14 |
| `connect-headphones`, `disconnect-headphones`, `connect-receiver`, `disconnect-receiver` | §19 |
| `update-backgrounds-list`, `bg-white` | §19 |
| `dunst-toggle` | §13 |
| `spotify-clear-cache` | §16 |
| `gist-hs`, `gist-md`, `gist-txt`, `screenshot-ocr` | §18 |
| `usb-reset`, `bluetooth-reset` | §18 |
| `show-logs`, `tops` | §12 |
| `startup-log-terminals`, `startup-wireless-terminals`, `startup-initial-applications`, `startup-misc` | §12 |
| `invert-screen`, `normal-screen` | `xrandr --rotate`, unchanged |
| `logout` | §2 |

Dropped with the omitted modules: `weekly-review`, `daily-review`, and
`xrandrize`, whose screen configuration stays in the xmonad config.

The `startup-*` entries matter more than they look: they are how startup gets
iterated on without a restart, which is why `startup.rs` is written as named
functions the menu can call rather than as one block.

`normal-dpi`/`medium-dpi`/`high-dpi` are rethought rather than ported. They call
`setEnv` inside the window manager so that *later* spawned programs inherit
`GDK_SCALE`/`GDK_DPI_SCALE`; in Rust 2024 `std::env::set_var` is `unsafe`,
precisely because this config is full of threads reading the environment while
another sets it. So they write to an overrides map in `Env` (§11) that the
spawn helpers apply (§12) — safe, and less surprising: what a program inherits
is a value the config owns rather than a mutation of its own process.

## 21. The startup lock

The machine autologins, so the lock *is* the login: `slock` comes up first and
the rest of startup runs behind it, meaning emacs, Chrome and Spotify are
loading while the password is being typed. That is why it belongs here rather
than in the display manager — a display manager login would serialize the two.

`PENROSE_NO_STARTUP_LOCK=true` skips it, replacing `XMONAD_NO_STARTUP_LOCK`.

One deliberate difference from xmonad, which exits the X session if `slock`
cannot be started, on the reasoning that an unlocked desktop is worse than no
desktop. Under the supervisor loop (§2) that exit is a *relaunch*, which would
try and fail the same way until the loop gives up after three attempts and
drops to a shell — itself unlocked, and now also without a window manager. So
the failure notifies loudly and carries on instead. If the machine ever stops
autologging in, this whole section can go.

---

## Module map

Two layers, split by directory. The top level is the window manager: penrose's
model, this config's answers to it, and the plumbing every binding needs.
`src/actions/` is what the bindings and the `M-x` menu actually *do* — one
module per thing the config drives, none of which knows anything about penrose
beyond the handler signature.

That boundary is worth keeping sharp. It is the line between code that would
have to be rethought for a Wayland compositor and code that would merely be
recompiled, and it is roughly the line between §1–§10 and §11–§20.

| file | what it holds |
|---|---|
| `src/main.rs` | `Config`, the `Conn` type alias, tags, exit codes |
| `src/bindings.rs` | the keymap and mouse bindings (§3, §8) |
| `src/layout.rs` | `TallWheel` (§4) |
| `src/conn.rs` | `PhysConn`, screen ordering (§5) |
| `src/manage.rs` | placement rules, `IsDialog`, `AutomatedBrowser` (§6) |
| `src/ewmh.rs` | activation suppression (§7) |
| `src/urgency.rs` | what asked for attention while it was refused (§7) |
| `src/menu.rs` | the rofi wrapper (§9) |
| `src/startup.rs` | startup hook, Wayland-capable programs with `--class` |
| `src/env.rs` | `Env`, secrets and UUIDs from `env/untracked` (§11) |
| `src/process.rs` | the `systemd-cat` spawn helpers, output capture, tmux (§12) |
| `src/notify.rs` | `notify`, `notify_truncated`, dunst control (§13) |
| `src/actions/mod.rs` | restart, logout, the `M-x` menu (§2, §20) |
| `src/actions/toggles.rs` | the persisted redshift/touchpad/lock state (§14) |
| `src/actions/audio.rs` | amixer, brightness, the media keys (§15) |
| `src/actions/spotify.rs` | dbus and Web API transports, token refresh (§16) |
| `src/actions/notes.rs` | window-title context, clipboard, appending (§17) |
| `src/actions/capture.rs` | screenshots, recording, ocr, gist, usb-reset (§18) |
| `src/actions/logs.rs` | the journal for the focused window and its children (§12) |
| `src/actions/background.rs` | the backgrounds list and the hourly thread (§19) |
| `src/actions/bluetooth.rs` | `tmux send-keys` into `bluetoothctl` (§19) |
| `scripts/run-penrose.sh` | the session script and supervisor loop (§2) |
| `scripts/rebuild-penrose.sh` | what `M-q` runs |

Everything in the table exists.

`env.rs`, `process.rs`, `notify.rs` and `menu.rs` stay at the top level rather
than under `actions/`, because both layers use them: the startup hook spawns
through `process.rs`, `M-q` notifies, and the manage rules do not care that
`spotify.rs` exists. The dependency runs one way — `actions/*` reaches up, the
window-management modules never reach down — which is what keeps the split from
being cosmetic.

Tags are `1..9` then `0`, matching `workspaceNames`. With `border_width: 0` the
border colours in `Config` are dead settings.

New third-party dependencies, all in the second half: `serde`/`serde_json`
(§14, §16), `ureq` (§16), `regex` (§17), and a date library (§17). Nothing in
§1–§10 needed anything beyond penrose and `tracing`.

## Remaining

- **§15–§20**, in any order: the actions are independent of each other now
  that the environment, spawn helpers, notifications and persisted state
  exist. §11 and §12 are load-bearing; after them the
  actions are independent of each other and can land one at a time.
- **Sequence bindings** (§3). The mechanism exists in the pinned penrose; the
  four sequences are Spotify (§16) and backgrounds (§19), so they arrive with
  those.
- **Verification on real hardware.** `PhysConn` has only been exercised against
  a single Xephyr screen, and the startup hook has only run with `RESTARTED`
  set, so its first-run path is untested.

## Testing

- **Keymap parse test.** `bindings_parse_correctly` runs `parse_keybindings`
  over the whole map, catching every typo in a ~100-entry keymap without
  starting a window manager. Parsing resolves names to keysyms in process (§3),
  so this needs neither a display nor the `xmodmap` binary.
- **Unit tests** for TallWheel's rects (§4) and `PhysConn`'s screen order (§5).
- **Xephyr** for everything else, rather than betting a login session on each
  iteration. Run the binary nowhere else: with no window manager on the display
  it will happily take over whatever it finds, and with `RESTARTED` unset it
  will spawn the entire startup set into it.
- **`cargo clippy`**, which `rebuild-penrose.sh` runs before building.
  `clippy.toml` forbids the APIs that compile here but are wrong in a window
  manager — penrose's spawn helpers, anything that waits for a child (§12),
  `env::set_var` (§20), and sleeping on the event loop (§10) — and `main.rs`
  makes them a hard error. Every one of them had already caused a bug. Where a
  call is genuinely correct it carries an `#[allow]` saying why, of which there
  are three.

The second half adds one more cheap category: the parts with no X in them at
all — the window-title regexes and the context strings they produce (§17), the
toggles file round-tripping (§14), the JSON field extraction (§16) — are plain
functions over strings and test as such.

## Open questions

- Does focus behave acceptably without `focusTracking`? To be answered by
  running it. (§4)

## Original XMonad configuration modules to omit

- **`Screens.hs`** — detects laptop/big-screen/side-screen by grepping
  `xrandr --query` for specific output names, then runs the matching `xrandr`
  invocations. Out with the rest of the X11 screen configuration. It takes
  `xrandrize` and the screen-dependent placement of emacs and chrome with it —
  both land on their tags by class instead (§6).
- **`WeeklyReview.hs`** — the `weekly-review` and `daily-review` menu entries.

`Power.hs` is not on the list but is dead code either way: `checkAcConnected`
has no callers left.

Also gone, without being a module: `FlexibleManipulate` (§8) and `focusTracking`
(§4).
