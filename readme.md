# mgsloan's dotfiles

This repo contains my computer configuration files! The code and
configuration here is covered by the MIT license - see [the text
here](env/LICENSE).

This repo also contains a [log of setup notes](env/setup.md) with info
about specific steps I've taken to setup my computer.

My emacs configuration is in a [separate
repo](https://github.com/mgsloan/mgsloan-emacs).

## No symlink manager - straight git

The idea is to directly use git to version contents of your home directory.  At
first, this seems like a poor idea, because git searches for `.git` directories
in all parent directories.  So, it might be easy to accidentally invoke git
commands on the home directory git repo.  There's an easy fix to this problem,
though - just avoid using the directory name `.git` - and specify environment
variables that let git invocations know where to look.

I got the seed of this approach from [this excellent
post](https://developer.atlassian.com/blog/2016/02/best-way-to-store-dotfiles-git-bare-repo/).
However, my implementation soon diverged some from what's recommended there, as
described below.

## How to use this as a home directory

In your current home directory, run the following:

```
git clone --bare git@github.com:mgsloan/mgsloan-dotfiles .home.git
export GIT_DIR="$PWD/.home.git"
export GIT_WORK_TREE=$PWD
git config core.bare false
git config core.logAllRefUpdates true
git config core.workdir ../
git config core.hooksPath ./env/git-hooks
```

Note that after running `export GIT_DIR="$PWD/.home.git"`, you will not be
able to use git on other repositories within this shell. To disable this, use
`unset GIT_DIR`.

Here's why each of these config fields are set:

* `bare` must be disabled before setting `workdir`.

* Enabling `logAllRefUpdates` means the reflog will be updated. This is set to
  true when cloning a repo normally, because it is potentially quite useful.

* Setting `workdir` to `../` causes it to use your `HOME` dir.  Could also set it
  to `$HOME` if you'd prefer to be able to move `.home.git` elsewhere.

* Setting `hooksPath` to `./env/git-hooks` uses a precommit hook described in
  the "Safety git hooks" section below.

Now, running `git status` should show a bunch of deleted files and modified
files. Heres how to resurrect the deleted files:

```
git status --porcelain | awk '$1 == "D" {print $2}' | xargs git checkout HEAD --
```

### Use of submodules

I've added quite a few submodules to this repo, and will likely add more.
Sometimes it's to track the source trees of programs that I've built from
source, that I frequently use.  Sometimes I symlink from `~/.local/bin` to the
build results of these source trees.

The submodules can be initialized via

```
git submodule update --init
```

### Safety git hooks

FIXME: This isn't currently committed to the repo

By using a work-dir separate from the git-dir, running git commands in projects
in your home dir will not affect the dotfiles cfg. This is important, because it
greatly diminished the risk of accidentally checking in / pushing stuff you
don't want to. Git's "look for a .git dir in parents" logic will not find one in
`~/` since it is instead called `.dotfiles-git`.

However, even so, I want to make it really hard / impossible to accidentally
commit stuff I didn't intend. `git add` will refuse to add stuff from git
commit. However, it can be side-stepped via `git add --force`. Perhaps it's
overkill since you're asking for trouble with `--force`. However, I've decided
to add a commit hook to further check that nothing which is ignored has been
added.

TODO: package this up / write a blog post about it.

### Environment variables to enable computer specific settings

FIXME: Currently nothing actually uses these variables, and I haven't moved my
old laptop over to an updated version of this config quite yet.

As much as possible, I want to be able to still use this repo with my old
computer setup. I didn't want to fuss around with also reinstalling things on
that drive / machine.

To facilitate this, there's script in [`env/settings.sh`](env/settings.sh)
which sets environment variables to determine the following:

* `USE_HIDPI`, to enable various mechanisms for rescaling applications. Ubuntu
  17.10 has pretty nice support for hidpi. Unfortunately, this support relies on
  the wayland display server, which is not compatible with xmonad. I might give
  Ongy's [waymonad](https://github.com/Ongy/waymonad) a try one of these days,
  looks like a potential solution to this!
