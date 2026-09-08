`/etc/default/earlyoom` is copied from `default` here, and the service is
(re)started so config changes take effect:

```
./apply.sh
```

## Why

On 2026-08-23 a runaway build tool (`arenacheck`, an unbounded arena in a
conformance test under `~/oss/succinct-tree-sitter-arena`) grew to 44GB
virtual / 14GB RSS and drove the machine into about a minute of dropped
frames before the kernel's own OOM killer finally picked a victim.
`systemd-oomd` was active too, but its default 30s sustained-pressure dwell
time let a spike that fast outrun it.

`earlyoom` polls free memory/swap directly and kills the largest offender the
moment free memory drops under 5% or free swap under 10% (`-m 5 -s 10`), with
no dwell time — a much faster backstop than either the kernel or oomd.

See also `capmem` in `~/.bash_aliases`, which caps a single risky command's
own cgroup instead of relying on any of this firing at all.
