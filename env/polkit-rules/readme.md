Custom polkit rules for this machine. Applied with:

```
./apply.sh
```

which copies everything here into `/etc/polkit-1/rules.d/` and restarts
`polkit` (it also picks up changes live via inotify, but restarting makes an
edit's effect immediate and unambiguous).

## `60-cpu-governor.rules`

See the comment in the file itself, and
`~/env/scripts/cpu-governor-apply.sh`.
