# Conversational style / development style

* Tell me what's happening plainly as you work on it. Feel free to be technical, but keep things to the point.

# Coding style

* Write elegant, concise, efficient code. The priority is performance over elegance, though.

* Pay attention to the existing style of the repo and mimic it.

* Prioritize code clarity and correctness. After that comes performance / lightweight-ness. This is a tradeoff. Clever optimizations are fine when they can be put behind a clear and understandable abstraction.

* Prefer implementing functionality in existing files unless it is a new logical component. Avoid creating many small files.

* Avoid creative additions unless explicitly requested

* Use full words for variable names (no abbreviations like "q" for "queue")

# Comment style

This gets its own section because it's very important.

* Assume that the reader is a skilled software engineer that has context on the codebase

* Write down things that would not be obvious to the reader:

  - If some code is non-obvious or surprising, explain why it is that way. Do **not** give a story about the sequence of events that led to the code. Some narrative is sometimes ok, but the focus is on the timeless why.

  - If some invariant needs to be preserved

  - Do not write down the details of design decisions, that's what the design docs are for.

* Avoid fancy language and superfluous words. Removing unnecessary words is very
  good. Lets acknowledge and respect the cost to the downstream reader (human or
  AI).

* Comments are not capitalized when they are a fragment. They are capitalized
  when they are a complete sentence.

# Documents

Documents should have similar concision and simplicity as comments. However, they can go into a lot more comprehensive details. Even so the principle of using fewer words when possible to convey the information holds.

# Commit style

The body should be concise when possible, but also going into details that might be relevant to someone ending up on the commit via a blame in the future. These do not need to be PR style descriptions - these do not need to advocate for the change or persuade.

# Git setup

## Commit contents are human vetted

Never stage changes or make commits using unstaged changes.  You can create commits, but only using changes that the human changes.  Adding submodules is fine.  Working with git repos that does not override the GIT_DIR to that is fine. So, working in submodules of env/ is fine.

This is very important because this is the user's home directory and so could contain sensitive files, and so selecting which files and hunks to commit is always a human activity.  Writing commit messages can be nice to automate, though.

Editing working-tree files is normal work and is fine.

## Layout

`~/env` is **not** its own repo. It is a subdirectory of the home-directory
dotfiles repo:

- Git dir: `~/.home.git` (cloned bare, then `core.bare=false`; the work tree
  is supplied per invocation by `GIT_WORK_TREE`/`--work-tree`, deliberately not
  recorded in the repo config - see `home-dir-git.md`)
- Work tree: `~`
- Remote `origin`: https://github.com/mgsloan/mgsloan-dotfiles.git, branch `master`

Because there is no `~/env/.git` or `~/.git`, a plain `git status` run from
`~/env` finds no repository (or, inside a submodule, finds that submodule
instead). Use the `cfg` wrapper on `PATH`:

```sh
cfg status          # ~/.local/bin/cfg = git --git-dir="$HOME/.home.git/" --work-tree="$HOME"
```

Paths in `cfg` output are relative to `~`, so files here appear as `env/...`.

Magit in a normal emacs *does* work on this repo - `.emacs.d/git.el` points it
at `~/.home.git` per directory. That does not change anything for the command
line: plain `git` from `~/env` still finds no repository.

Rationale and the full bootstrap procedure are in `home-dir-git.md`.
