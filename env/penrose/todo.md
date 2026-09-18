# Penrose todo: gaps found against xmonad

Findings from reading penrose's core (`src/pure/`, `src/core/`, `src/x/`,
`src/x11rb/`, `src/builtin/layout/`) against xmonad's core (`StackSet.hs`,
`Core.hs`, `Operations.hs`, `Main.hs`, `Layout.hs`). Not reviewed:
`penrose_ui`, `penrose_menu`, xmonad-contrib.

Penrose source references are relative to `vendor/penrose-pinned`, as in
[`design.md`](design.md). xmonad references are relative to `~/env/xmonad`.

Items 1-5 are small and independent. 1 and 3 are correctness bugs that show up
in normal use; 2 is close to a one-character fix; 5 and 6 are the ones most
likely to produce "why did my keybinding stop working" confusion; 8 is specific
to this config.

## Bugs

### 1. `xrandr` changes never trigger a refresh

`src/x/handle.rs:196` — `detect_screens` calls `client_set.update_screens(rects)`
and returns. Nothing repositions clients afterwards. Both callers
(`src/x/mod.rs:235` for root `ConfigureNotify`, `:249` for `RandrNotify`) drop
straight back into the event loop, and `WindowManager::run` does not refresh
either. Result: after plugging in a monitor, internal state is correct but the
screen is stale until some unrelated event forces a refresh.

xmonad's `rescreen` (`Operations.hs:361`) is `windows $ \ws -> ...` — the screen
update *is* a refresh, by construction.

Fix: wrap the update in `modify_and_refresh` rather than mutating `client_set`
directly.

### 2. `same_screen` is decoded wrong

`src/x11rb/conversions.rs:93` and `:100` — `same_screen: event.same_screen_focus == 0`.

That field is a packed byte: bit 0 is `focus`, bit 1 is `same-screen`. Should be
`(event.same_screen_focus & 2) != 0`. As written the common case (same screen,
focused = `0b11`) yields `same_screen: false`, inverting the guard in
`handle::leave` (`src/x/handle.rs:188`) that mirrors xmonad's
`not (ev_same_screen e)` at `Main.hs:386`.

### 3. `ConfigureRequest` on a managed tiled window is silently dropped

`src/x/handle.rs:104` — returns `Ok(())` with no reply to the client. ICCCM
requires a synthetic `ConfigureNotify` telling the client its real geometry;
xmonad does this at `Main.hs:404-409`. Clients that block waiting for it
(Java/AWT is the classic case, some GTK dialogs) hang or mis-size.

### 4. `ConfigureRequest` on a floating window moves it, then refresh snaps it back

Same handler (`src/x/handle.rs:104`) falls through to
`x.position_client(*id, *r)` for floating clients but never updates
`client_set.floating`, so the next `visible_client_positions` re-applies the
stored `RelativeRect`.

xmonad calls `float w` after configuring (`Main.hs:403`), which re-derives the
stored rect from the window's actual geometry via `floatLocation`.

### 5. Modifier hygiene is hardcoded and incomplete

`src/x11rb/mod.rs:294` grabs with `[0, ModMask::M2]`; `src/x11rb/conversions.rs:79`
strips `M2` from incoming key events. Two problems:

- NumLock is assumed to be mod2 rather than looked up in the modifier map.
- CapsLock (`LockMask`) is not handled at all, so every binding breaks with
  CapsLock on.

xmonad derives the mask from the modifier map (`cacheNumlockMask`,
`Operations.hs:444`) and grabs the full cross product via `extraModifiers`
(`:623`) = `[0, numlock, lock, numlock .|. lock]`, cleaning incoming events with
`cleanMask` (`:629`).

### 6. No `nubScreens`, and screen indices aren't ordered

`src/x11rb/mod.rs:246` — `screen_details` returns CRTCs in randr enumeration
order with no dedup.

- **No dedup.** xmonad's `getCleanedScreenInfo` (`Operations.hs:356`) drops
  duplicate and fully-contained rects, so mirrored outputs collapse to one
  screen. Penrose creates two screens on the same pixels, one permanently
  invisible. `PhysConn` in this repo does not dedup either.
- **No ordering.** `Screen::index`'s doc comment (`src/pure/screen.rs:29`)
  claims indices are "assigned from left to right based on the absolute
  position of their top left corner" — nothing does that. `PhysConn` already
  works around this half; the upstream doc comment is still wrong.

### 7. `focus_in` is a focus-stealing vector

`src/x/handle.rs:127` — honours any `FocusIn` by calling `x.focus_client(id)`,
setting `_NET_ACTIVE_WINDOW`, then `set_active_client`, which pulls that
client's *workspace* onto the screen.

xmonad's core deliberately has no `FocusIn` handler. A window that grabs focus
does not get to change the current workspace.

### 8. `SIGCHLD` is set to `SIG_IGN` unconditionally

`src/core/mod.rs:420` — set inside `WindowManager::run`, and it `panic!`s if the
call fails, so a config cannot override it.

Relevant here specifically: `~/env/xmonad/src/XMonad/Core.hs:882` carries a local
modification commenting out exactly this, annotated *"mgsloan modification to
allow for waiting for processes."* With `SIG_IGN` set, `waitpid` cannot reap
children. The process-supervision layer is out of scope per `design.md` §scope,
but if any of it moves over, this is what will break it.

### 9. `clearEvents enterWindowMask` is unimplemented

`src/core/conn.rs:201` is a literal TODO where xmonad has `Operations.hs:229`.
Windows that move under a stationary pointer generate `EnterNotify` and steal
focus.

Matters for this config specifically, which sets `focus_follow_mouse: true` and
relies on the pointer warp for `warpMid`. The warp itself is safe —
`requires_pointer_warp` returns false for `Enter` (`src/x/mod.rs:154`), which
breaks the feedback loop — but layout changes that shuffle windows under a
stationary cursor are not covered.

### 10. Border insets use a config constant, not the window's real border

`src/core/conn.rs:234` — `position_clients` shrinks by `config.border_width`
unless the rect happens to equal a screen rect. xmonad's `tileWindow`
(`Operations.hs:329`) reads `wa_border_width` from the window itself.

The `!screen_positions.contains(&r)` heuristic also means a floating window
sized exactly to the screen silently skips the inset.

Low priority for this config, which runs `border_width: 0`.

### 11. `util::notify` can never work

`src/util.rs:112` — `Command::new("notify-send").arg(msg).output()`, which is
precisely what the doc comment on `spawn_for_output` two functions above says
cannot be done:

> `std::process::Command::output` will not work within penrose due to the way
> that signal handling is set up. Use this function if you need to access the
> output of a process that you spawn.

`SIG_IGN` on `SIGCHLD` (#8) makes `waitpid` return `ECHILD`, so `output()`
returns `Err(No child processes)` even though `notify-send` ran fine. Callers
that check the result see every notification as failed; the notification itself
is unaffected, since the process is already spawned by then.

One-line fix — `.spawn().map(|_| ())`, since nothing wants notify-send's
output. Worth a PR: it is self-contained and does not depend on how #8 is
resolved.

### 12. A window manager already running is not detected

`src/x11rb/mod.rs:735` — `set_client_attributes` sends
`change_window_attributes` with `SUBSTRUCTURE_REDIRECT` on the root and never
checks the result. Only one client may hold that mask, so a second window
manager gets `BadAccess` — but the request is unchecked, so the error arrives
asynchronously and is dropped. Penrose carries on, grabbing keys and managing
clients alongside whatever is already running.

Should be the checked variant (`.check()`), failing startup with a clear
message. That is the standard behaviour: xmonad, dwm and i3 all refuse to start
with "another window manager is already running". Tracked for the library, with
the wider question of which requests deserve checking, in the fork's own
`todo.md`.

Wanted here for the obvious reason: running the binary by hand should exit
immediately rather than fight the session. Note it is not a complete guard —
an X display with *no* window manager (Xwayland under a Wayland compositor)
still lets it take over, which is how a stray `./penrose-wm` relaunched the
whole startup set on 2026-08-11.

## Structural gaps

Larger than bug fixes; each is a design decision to make rather than a patch.

### River does not clip windows to their allocations

`river_window_v1.propose_dimensions` is only a proposal: a client may choose
different dimensions or respond later. The backend positions the resulting
window without calling `set_clip_box`, so an oversized or unresponsive client
can draw outside the tile Penrose allocated. Track allocation-sized clip boxes
in `RenderPlan` and restate them with the other render state. Verify the desired
behavior for floating windows before enabling this.

### No restart-with-state

xmonad has `writeStateToFile` / `readStateFile` / `restart`
(`Operations.hs:654-711`): `M-q` re-execs and restores the `WindowSet` and
extensible state. Penrose has no equivalent — `manage_existing_clients`
(`src/x/mod.rs:475`) reconstructs a best-effort approximation from
`_NET_WM_DESKTOP`, losing stack order, floating rects, and layout state.

`design.md` puts restart in scope, so this is the largest thing to build.

### Layouts have no access to WM state

`Layout::layout` takes only a `Stack<WinId>` and a `Rect`; there is a TODO
acknowledging this at `src/core/layout/mod.rs:18`. xmonad's `doLayout` runs in
the `X` monad, which is what makes contrib layouts that query window class or
properties possible. `LayoutHook` is a partial substitute but cannot introspect
the layout or receive messages.

### Unhandled events aren't broadcast to layouts

xmonad's catch-all `handle e = broadcastMessage e` (`Main.hs:426`) means layouts
see `DestroyWindowEvent`, `PropertyEvent` and friends, and can drop per-window
resources. Penrose only ever broadcasts `Hide` to newly-hidden workspaces
(`src/core/conn.rs:504`), and has no `ReleaseResources` equivalent.

### No layout error containment

xmonad wraps `runLayout` in `catchX` and falls back to `Full`
(`Operations.hs:193`). Penrose's `Layout::layout` is infallible, so a bad layout
panics the WM — and there are `expect`s on the hot path, e.g.
`split_at_width_perc(...).expect("split point to be valid")` at
`src/builtin/layout/mod.rs:171`.

### Test depth

xmonad: 123 QuickCheck properties over `StackSet` and `Tall`. Penrose: 29
quickcheck properties plus ~85 unit tests. The pure layer is well covered on
both sides. Penrose's `diff.rs` properties cover ground xmonad structurally
cannot (no X server needed), which partly offsets this.

## Not to change

Recorded so these don't get re-litigated.

- The `Conn` / `XConn` split (`src/core/conn.rs:59`, `src/x/mod.rs:90`, with the
  blanket `impl<X: XConn> Conn for X`) is better factored than xmonad's direct
  Xlib dependency throughout `Operations.hs`, and is what makes the river
  backend in [`river-design.md`](river-design.md) viable at all.
- `MockXConn` / `StubXConn` let refresh logic be tested without a server;
  xmonad has no equivalent.
- Errors as values rather than exceptions: the config's
  `printErrors` / `printHandlerErrors` wrappers genuinely do disappear.
- `Diff` / `Snapshot` (`src/pure/diff.rs`) make the render pass legible in a way
  `windows` (`Operations.hs:158`) is not.
