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
git config core.workdir ../
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
git --git-dir=$HOME/.home.git/ --work-tree=$HOME $@
```

So, this means that you can just run commands like `cfg commit` to
interact with the home dir git repo.

### Details of the above incantations

After running `export GIT_DIR="$PWD/.home.git"`, you will not be able
to use git on other repositories within this shell. To disable this,
use `unset GIT_DIR`.

Here's why each of the config fields are set:

* `bare` must be disabled before setting `workdir`.

* Enabling `logAllRefUpdates` means the reflog will be updated. This is set to
  true when cloning a repo normally, because it is potentially quite useful.

* Setting `workdir` to `../` causes it to use your `HOME` dir.  Could also set it
  to `$HOME` if you'd prefer to be able to move `.home.git` elsewhere.

## Usage with magit

I typically use [magit](https://magit.vc/) to update this repo. To do
this, I run emacs with `GIT_DIR` and `GIT_WORK_TREE` set, via the
[`.local/bin/edit_cfg`](/.local/bin/edit_cfg) script:

```sh
#!/bin/sh
GIT_DIR=$HOME/.home.git/ GIT_WORK_TREE=$HOME emacs $HOME/env/src/xmonad.hs $@
```

However, magit unsets these environment variables.  It's
straightforward to patch it, though:

```diff
diff --git a/lisp/magit.el b/lisp/magit.el
index 5bd9146f..e7c805a2 100644
--- a/lisp/magit.el
+++ b/lisp/magit.el
@@ -541,14 +541,14 @@ See info node `(magit)Debugging Tools' for more information."
 ;;; Startup Asserts

 (defun magit-startup-asserts ()
-  (when-let ((val (getenv "GIT_DIR")))
-    (setenv "GIT_DIR")
-    (message "Magit unset $GIT_DIR (was %S).  See \
-https://github.com/magit/magit/wiki/Don't-set-$GIT_DIR-and-alike" val))
-  (when-let ((val (getenv "GIT_WORK_TREE")))
-    (setenv "GIT_WORK_TREE")
-    (message "Magit unset $GIT_WORK_TREE (was %S).  See \
-https://github.com/magit/magit/wiki/Don't-set-$GIT_DIR-and-alike" val))
+;;   (when-let ((val (getenv "GIT_DIR")))
+;;     (setenv "GIT_DIR")
+;;     (message "Magit unset $GIT_DIR (was %S).  See \
+;; https://github.com/magit/magit/wiki/Don't-set-$GIT_DIR-and-alike" val))
+;;   (when-let ((val (getenv "GIT_WORK_TREE")))
+;;     (setenv "GIT_WORK_TREE")
+;;     (message "Magit unset $GIT_WORK_TREE (was %S).  See \
+;; https://github.com/magit/magit/wiki/Don't-set-$GIT_DIR-and-alike" val))
   (let ((version (magit-git-version)))
     (when (and version
                (version< version magit--minimal-git)
```

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
