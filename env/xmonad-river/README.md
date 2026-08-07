# xmonad-river

xmonad's window manager API, implemented over the river Wayland protocols
instead of X11, so that `../src/` can keep being the config.

**Status: the config compiles and links against this backend.** `custom-xmonad`
builds from `../src/` unmodified. What that does *not* mean is that it works:
nothing here has been run against a live river, because river is not installed
on this machine. See [Status](#status) for what is implemented, what warns at
runtime, and what is missing.

## Why river, and why this is possible

river's master branch (0.5.0-dev, not the released 0.3.x) does not implement
window management at all. It defers *all* policy — position, size, focus,
keybindings, decorations — to a separate process implementing
[`river-window-management-v1`][wm-protocol]. From its README:

> Allow implementing Wayland window managers in high-level garbage collected
> languages without impacting compositor performance and latency.

That is a description of this project. Three consequences matter for this
config specifically:

1. **`TallWheel` survives.** The WM computes every window's geometry and hands
   it to the compositor via `river_node_v1.set_position` and
   `river_window_v1.propose_dimensions`. A custom layout is ordinary pure code
   again. This is the thing sway cannot offer at any price.

2. **`M-q` survives.** river supports hot-swapping window managers without
   restarting the compositor or its clients. The recompile-and-restart loop
   that makes this config worth having is recoverable — which the earlier
   [`../wayland-port-gaps.md`](../wayland-port-gaps.md) assessment concluded
   was impossible, because it was written against sway.

3. **The keymap ports as data.** xkbcommon reuses X11's keysym numbering, and
   `river_seat_v1.modifiers` reuses X11's modifier mask values. `mod4Mask` is
   still 64; `xK_Return` is still `0xff0d`. So key descriptions like
   `"M-S-<Return>"` mean the same thing without translation.

[wm-protocol]: https://isaacfreund.com/docs/wayland/river-window-management-v1/

## Architecture

```
  ../src/xmonad.hs                     the config, unmodified
        |
  xmonad-river-contrib                 EZConfig, Prompt, CycleWS, Warp, ...
        |
  xmonad-river                         XMonad, .Core, .Operations, .Layout
        |
  XMonad.River.WM                      manage/render state machine
        |
  XMonad.River.Protocol.*              generated protocol bindings
        |
  XMonad.River.Connection              socket, object ids, dispatch loop
        |
  XMonad.River.Wire                    Wayland wire codec (store-core)
```

### No libwayland

The wire protocol is implemented directly rather than binding
`libwayland-client`. This is viable because none of the river window management
protocols pass file descriptors, and fd passing (`SCM_RIGHTS`) is the only part
of the wire format that genuinely needs C. The result has no C dependencies,
and the event loop is ordinary Haskell rather than callbacks reached through
`wl_proxy_marshal`'s variadic interface.

The one thing this forecloses is `river_xkb_config_v1.create_keymap`, which
takes an fd. The generator emits a comment in place of such requests. If
keymap loading is ever wanted, that is the point at which fd passing has to be
added.

### Serialization

`store-core`, not `store`. Wayland's wire format is host-endian and 32-bit
aligned, which is exactly what `pokeStorable` and `peekStorable` do natively —
`binary` and `cereal` are big-endian and would route every field through an
escape hatch. Only the `Data.Store.Core` primitives are used; `store`'s derived
`Store` instances are *not* wire compatible with Wayland. `store-core` is a
single module over dependencies already in this config's closure, which keeps
the `M-q` rebuild fast.

Arguments are built as a `Args` monoid pairing a byte count with a `Poke`,
rather than an ADT interpreted by separate size and write functions. Keeping
those two in step by hand is how buffer overruns get written.

### Code generation

`codegen/generate-protocol.hs` turns the vendored XML in `protocol/` into
Haskell. Generated modules are checked in, and generating at build time was
rejected deliberately: it would put a custom `Setup.hs` and an XML parsing
dependency in the path of every rebuild, and the `M-q` loop is what most needs
to stay fast.

```
./codegen/generate-protocol.hs     # only needed when protocol/ is updated
```

## Status

### The config builds unmodified

`../src/xmonad.hs` and its 27 modules compile and link against this backend
with **no changes to the config**. Three things made that cheap:

- `XMonad.StackSet` is pure and was vendored verbatim.
- Keysyms and modifier masks are numerically identical between X11 and
  xkbcommon/river, so the keymap needed no translation. There is a test
  asserting every key description in this config parses.
- The `Query`/`ManageHook` algebra is backend-independent.

### Implemented

| Area | Notes |
| --- | --- |
| Wire codec | `store-core`. 12 tests, incl. byte sequences derived from the spec. |
| Connection | Socket, object id recycling, registry, dispatch, roundtrip. |
| Protocol bindings | Generated: 11 interfaces, all requests and events. |
| Manage/render loop | Layout and focus in the manage sequence, position/order/borders/hide in render — matching river's split of the two state categories. |
| Workspaces | river has no workspace concept; hidden workspaces are implemented with `river_window_v1.hide`/`show`. |
| Layouts | `LayoutClass`, `Tall`, `Full`, `Mirror`, `\|\|\|`. `TallWheel` works unchanged. |
| Manage hooks | Run during the manage sequence, *before* the window is rendered — the ordering guarantee xmonad has and sway's IPC cannot give. |
| `pid` | From `unreliable_pid`, so `manageSpawn` and `isAutomatedBrowser` work. |
| `isDialog` | From `river_window_v1.parent`, which is `xdg_toplevel.set_parent` — the faithful translation of a dialog under xdg-shell. |
| `transience` | Likewise from `parent`. |
| Pointer warping | `river_seat_v1.pointer_warp`. `warpMid` works. |
| Screens | Reconciled from river outputs every manage sequence; `PhysicalScreens` ordering preserved. |
| Prompts | Shell out to `fuzzel`. |
| `M-q` | `sendRestart` throws an async exception into the event loop thread (xmonad used the X11 event queue for the same reason); the loop then does `stop` → `finished` → `exec`. river keeps every client alive across the swap. |

### Implemented as a warning, not silence

Shims that change behaviour call `warnUnimplemented`, which logs once per
process to stderr — and therefore into the journal, reachable from this
config's own `show-logs` action. Silence would be worse: a config rule that
never fires looks like a bug in the config.

| Shim | What actually happens |
| --- | --- |
| `mouseWindow` (FlexibleManipulate), `mouseMoveWindow`, `mouseResizeWindow` | Mod+drag does nothing. river routes interactive gestures through the seat's `op_start_pointer`/`op_delta`/`op_release` cycle, which is not yet wired into `XConf`. |
| Multi-key submaps (`M-m M-l`, `M-b M-g`) | The prefix key does nothing. Needs a transient binding set installed for the prefix, using `ensure_next_key_eaten` so an unbound key cancels cleanly. |
| `isFullscreen` | Always `False`. river reports fullscreen as an event to answer, not as queryable state. |
| `stringProperty` | Always `""`. Wayland has no window properties at all. |
| `setEwmhActivateHook`, `doAskUrgent` | Never run. river does not surface activation to the window manager — but it also does not steal focus on activation, so the outcome this config wanted holds anyway; only the urgency *hint* is missing. |
| `XPConfig.promptKeymap` | Ignored; fuzzel owns prompt keys. Warns only if the config set one (this one does). Configure equivalents in `fuzzel.ini`. |
| `gnomeRegister` | XSMP is X11-only. Use `systemctl --user start graphical-session.target`. |

Deliberately silent, because genuinely unobservable:

- `isNotification` — notifications are layer-shell surfaces, never toplevels,
  so they never reach a manage hook under any implementation.
- `focusTracking` — the render sequence already places the focused window last
  unconditionally.

### Not done

- **Nothing has run against a live river.** Everything above is compile-time
  and unit-test evidence only.
- Extensible state does not survive a restart. xmonad serialises it into the
  resume state handed to the new process; river's stop-then-exec path has no
  equivalent channel yet, so `PersistentExtension` behaves like
  `StateExtension`.
- `logHook`-driven status bar output has no consumer; a Wayland bar wants
  `ext-workspace-v1` or a direct IPC.
- Floating window geometry is tracked in the `StackSet` but not yet applied
  during the render sequence.

## Building

Standalone, for working on the library:

```
cd xmonad-river && stack build && stack test
```

The config against this backend:

```
stack --stack-yaml stack-river.yaml build --flag xmonad-config:river
```

which produces **`custom-xmonad-river`**, alongside the X11 `custom-xmonad`
that a plain `stack build` produces.

The two are separate `executable` stanzas in `../package.yaml`, each made
`buildable: False` by the flag that selects the other. That shape is forced:
a cabal `if` may only appear inside a component's *fields*, and a component's
name lives in the stanza header with no field to override it — so a flag
cannot rename an executable, only enable or disable one. Everything except the
backend dependency is shared through a YAML anchor, which works because hpack
ignores top-level keys beginning with `_`.

`stack-river.yaml` exists for a separate reason: the set of *local packages*
differs. The X11 `xmonad` and `xmonad-contrib` checkouts are unbuildable
without libX11 headers, so they are omitted from that build plan.

Note that anything referring to the binary by name needs the river spelling:
`scripts/rebuild.sh` and the session desktop file, which for Wayland belongs in
`/usr/share/wayland-sessions/` rather than `xsessions/`.

## Testing

`stack test` covers the wire codec, layout geometry, and the key parser —
everything that can be checked without a compositor. 30 assertions. The protocol stack above that is unverified
at runtime until it is run against a real river.

To try `tinyrwm-hs`, build river from source (zig 0.16, wlroots 0.20) and run:

```
river -c /path/to/tinyrwm-hs
```

If that behaves like the C `tinyrwm`, the wire codec, generated bindings and
state machine are all correct, and anything broken in `custom-xmonad` is in the
xmonad layer above them.
