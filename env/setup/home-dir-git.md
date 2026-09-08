# No symlink manager, just git

This dotfiles repo uses git directly instead of having an install
procedure or symlink manager. This has quite a few benefits:

* Simplicity - no configuration needed.

* It helps keep my home directory tidy, as new files and folders
  either get added to this repo, deleted, or added to
  [`.gitignore`](/.gitignore).

* No need to research, compare and contrast symlink management
  approaches

This might initially seem like a bad idea, since `git` searches parent
folders for `.git` repos, and so commands might inadvertantly apply to
the home dir repo. There's a pretty good solution to this, though it
requires a bit of setup, described below.

I got the idea for this from [this excellent
post](https://developer.atlassian.com/blog/2016/02/best-way-to-store-dotfiles-git-bare-repo/).
However, my approach soon diverged some from what's recommended there,
as described in the section below.

## How to use this as a home directory

To clone this repo:

```
git clone --bare https://github.com/mgsloan/mgsloan-dotfiles.git .home.git
```

Alternatively, if you want to use this approach for your own, new
repo:

```
mkdir .home.git
cd .home.git
git init --bare
```

After cloning or initing, in your current home directory, run the
following commands.  Note that these commands will not change any of
your files.

```
export GIT_DIR="$PWD/.home.git"
export GIT_WORK_TREE=$PWD
git config core.bare false
git config core.logAllRefUpdates true
git config remote.origin.fetch "+refs/heads/*:refs/remotes/origin/*"
```

After these commands, git's index has not yet been updated, so it
will think that everything has been deleted, even files that exist.
Run this incantation to reset the index:

```
git reset HEAD -- .
```

Now, running `git status` should show a bunch of deleted files and
possibly some modified files.

Heres how to resurrect the deleted files, adding the files from this
repo that didn't exist before:

```
# NOTE: Before executing this, be sure to reset the index, otherwise
# it will overwrite existing files.
git status --porcelain | awk '$1 == "D" {print $2}' | xargs git checkout HEAD --
```

And, to initialize the submodules:

```
git submodule update --init --recursive
```

It's rather inconvenient to need to set these environment variables to
interact with the git repo.  The contents of
[`.local/bin/cfg`](/.local/bin/cfg) is

```
#!/bin/sh
git --git-dir="$HOME/.home.git/" --work-tree="$HOME" "$@"
```

So, this means that you can just run commands like `cfg commit` to
interact with the home dir git repo.

### Details of the above incantations

After running `export GIT_DIR="$PWD/.home.git"`, you will not be able
to use git on other repositories within this shell. To disable this,
use `unset GIT_DIR`.

Here's why each of the config fields are set:

* `bare` must be disabled, because git refuses to accept a work tree for a
  repo marked bare.

* Enabling `logAllRefUpdates` means the reflog will be updated. This is set to
  true when cloning a repo normally, because it is potentially quite useful.

* The work tree is *not* recorded in the repo's config, and that is deliberate.
  It comes from `GIT_WORK_TREE`/`--work-tree` on every invocation, so a stray
  `GIT_DIR=$HOME/.home.git` run from the wrong directory fails loudly - it
  treats the current directory as the work tree and reports everything as
  deleted - rather than silently operating on all of `$HOME`.  (Note that
  `core.workdir`, which earlier versions of these instructions set, is not a
  git config key at all and was silently ignored.  The real key is
  `core.worktree`; setting it is what would throw the failsafe away.)

## Usage with magit

I typically use [magit](https://magit.vc/) to update this repo.

This used to mean running a second emacs with `GIT_DIR` and `GIT_WORK_TREE`
set in its environment, via a `.local/bin/edit_cfg` script, plus a patched
magit - magit deliberately unsets those variables at startup, and links to
[a wiki page](https://github.com/magit/magit/wiki/Don't-set-$GIT_DIR-and-alike)
explaining why.  It has a point: because subprocesses inherit the environment,
that emacs could only ever see this repo, and every other repo was shadowed.
Hence the second emacs.

Now a single, ordinary emacs handles both.  Rather than setting the variables
process-wide, [`.emacs.d/git.el`](https://github.com/mgsloan/.emacs.d) injects
them per git invocation, for directories that belong to this repo.  Magit runs
every git subprocess through one function, `magit-process-environment`, so a
single piece of advice there covers all of them:

```elisp
(defun my-home-git-environment (env)
  (if (my-home-repo-dir-p default-directory)
      (cons (concat "GIT_DIR=" my-home-git-dir)
            (cons (concat "GIT_WORK_TREE=" my-home-work-tree) env))
    env))

(advice-add 'magit-process-environment :filter-return #'my-home-git-environment)
```

Because the variables are never in emacs's own environment, magit's startup
assertion has nothing to unset, and no patched magit is needed.

`my-home-repo-dir-p` decides which directories count, in this order:

1. `~/.home.git` and anything under it - required, because the `COMMIT_EDITMSG`
   and `git-rebase-todo` buffers live there.
2. Anything outside `$HOME` - no.
3. Anything with a `.git` at or above it - no.  A real repo always wins, so
   `~/.emacs.d`, `~/proj/*` and this repo's own submodules are unaffected.
4. Anything under `~/env` - yes, including untracked files and brand-new
   directories, so a freshly created `env/foo/bar.sh` is stageable right away.
5. Anywhere else under `$HOME`, only if this repo tracks content there.  So
   `~/proj`, `~/dl` and `~/docs` keep reporting no repository, exactly as they
   would without any of this.

Rule 5 uses a single `git ls-files` cached at startup; run
`M-x my-home-repo-refresh` after tracking a new top-level entry outside `env/`.

Magit shows this repo as `env` rather than `mgsloan` (the basename of its work
tree), and its status buffer carries a `HOME DOTFILES REPO` header line.

## Idea: Hooks for more safety

So far, using a .gitignore and careful commits has been sufficient to
avoid putting anything unwanted into the repository.  I also planned
to add some git hooks for extra safety, but haven't yet gotten around
to it.  A few things these hooks might do:

* It might be nice to have a commit hook which prevents committing any
  directories that you definitely don't want to commit.  `.gitignore`
  nearly achieves this, but it can be side-stepped via `git add
  --force`.  A commit hook could add a further layer of safety.

* It would be good to avoid absolute paths to my home directory, or
  absolute github urls. So, it might make sense to ban uses of my
  username, `mgsloan`, other than for some paths on an allowlist.

It'd also make sense to version these hooks.  Using the
`env/git-hooks` dir for this can be done via:

```
git config core.hooksPath ./env/git-hooks
```
