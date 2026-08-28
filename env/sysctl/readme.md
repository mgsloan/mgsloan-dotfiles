Kernel parameters that belong to this machine rather than to a package.
Everything here is copied into `/etc/sysctl.d/`, which is read at boot and by
`sysctl --system`:

```
./apply.sh
```

`/etc/sysctl.d/` already holds files this directory does not own —
`99-desktop-swap.conf`, `99-uosserver.conf` — so `apply.sh` copies the files
here rather than clearing the directory out.

## `99-sysrq.conf`

REISUB did not work, and it took a reset to find out.

On 2026-08-20 a waynav overlay went up two seconds before the keyboard driving
it fell off the USB bus and failed to re-enumerate:

```
00:18:11  waynav[785528]: overlay created: 1920x1080 on eDP-1
00:18:13  kernel: usb 3-3.2: USB disconnect, device number 6
00:18:13  kernel: usb 3-3.2: new full-speed USB device number 7 using xhci_hcd
00:18:13  kernel: usb 3-3.2: device descriptor read/64, error -32
```

waynav holds an exclusive keyboard grab for as long as it runs, and the key
that dismisses it now had nowhere to come from, so the session looked frozen.
The way out was SysRq, and half of it was refused:

```
00:21:55  kernel: sysrq: Keyboard mode set to system default   (r)
00:21:56  kernel: sysrq: This sysrq operation is disabled.     (e)
00:21:58  kernel: sysrq: This sysrq operation is disabled.     (i)
00:22:00  kernel: sysrq: Emergency Sync                        (s)
```

`kernel.sysrq` was 438, Debian's default, which omits `0x40` — signalling of
processes. So `e` and `i` did nothing, nothing was ever asked to terminate, and
what ran was R-S-U-B: a sync and a reset with every program's unsaved work
still in it. The E and the I are the entire reason the sequence has that shape.

Setting it to 1 — all functions, not a bitmask — fixes that and adds the
debugging dumps (`0x8`) as well, so the next freeze can be recorded before it
is escaped: `w` for blocked tasks, `t` for task states, `l` for backtraces, all
of which land in the kernel log and are there in `journalctl -b -1` afterwards.

This is worth knowing before typing any of it: on a keyboard where SysRq is the
shifted PrtScn, holding Shift through the sequence sends the *uppercase*
commands, which are different commands. Shift+S is `reset-sched-ext`, not
`sync` — which is what happened at 00:19:30 that night, before the lowercase
attempt above.

The trade is that SysRq needs physical access to a keyboard, which on this
machine is already enough to hold the power button.
