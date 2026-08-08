# Git setup

## Never mutate git state

**Never run any command that changes git state. That is the user's job, always.**

Forbidden without exception (in the home repo, in submodules, and in any other
repo under this home directory): `commit`, `add`, `rm`, `mv`, `restore`,
`checkout`, `switch`, `reset`, `revert`, `stash`, `clean`, `merge`, `rebase`,
`cherry-pick`, `apply`, `am`, `branch`/`tag` creation or deletion, `push`,
`pull`, `fetch`, `remote` changes, `config` writes, `submodule
update`/`add`/`deinit`, `gc`/`prune`, and anything with `--force`.

Read-only inspection is fine: `status`, `diff`, `log`, `show`, `blame`,
`ls-files`, `check-ignore`, `submodule status`, `config --list`, `rev-parse`.

Editing working-tree files is normal work and is fine. Just leave the index,
refs, stash, and remotes untouched — if a change needs staging or committing,
say so and let the user do it.

## Layout

`~/env` is **not** its own repo. It is a subdirectory of the home-directory
dotfiles repo:

- Git dir: `~/.home.git` (cloned bare, then `core.bare=false`, `core.workdir=../`)
- Work tree: `~`
- Remote `origin`: https://github.com/mgsloan/mgsloan-dotfiles.git, branch `master`

Because there is no `~/env/.git` or `~/.git`, a plain `git status` run from
`~/env` finds no repository (or, inside a submodule, finds that submodule
instead). Use the `cfg` wrapper on `PATH`:

```sh
cfg status          # ~/.local/bin/cfg = git --git-dir="$HOME/.home.git/" --work-tree="$HOME"
```

Paths in `cfg` output are relative to `~`, so files here appear as `env/...`.

Rationale and the full bootstrap procedure are in `home-dir-git.md`.
