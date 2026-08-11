# Git setup

## Never make automatic commits to the home dir repo

Never run any command that makes or modifies commits in
`~/.home.git`. Staging changes, adding submodules, inspecting is
fine. git that does not override the GIT_DIR to that is fine. So,
working in submodules of env/ is fine.

Editing working-tree files is normal work and is fine.

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
