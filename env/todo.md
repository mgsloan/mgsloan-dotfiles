# Installing "adobe source code pro"

I figured I'd try installing the font suggested by spacemacs. Previously I'd
used "Hack". So, from
https://github.com/adobe-fonts/source-code-pro/issues/17#issuecomment-167650167

```
FIXME: figure out an approach that actually works.  Clean up ~/.fonts
```

# Installing zsh

# Making magit work with bare dotfiles repo

readme.md now includes a pretty good strategy for this!  However, hitting enter
on a working copy file does not work :/

`cfg config --get core.worktree`

# Actually use docker-machine?`

# Firefox extensions to install / configure

* Figure out a good home page to use with "New Tab Override". Something not too
  distracting, but good quotes

* Install tree style tabs
  https://addons.mozilla.org/en-US/firefox/addon/tree-style-tab/

# Wanted firefox extensions

* Something to store list of extensions / configuration of extensions, so that
  they can be added to this dotfiles repo, easily moved to other computers. FEBE
  and OPIE seem like they might be what I want, but they do not work in Firefox
  Quantum.

# Better stack behavior when offline

* "Downloading lts-7.19 build plan ..." doesn't fail quick for intero. Things to
  resolve here:

  * Make it fail after only a couple tries when offline.

  * Have a flag which is passed by intero, which indicates stack should fail as
    quick as possible for cases like this.

# Tweaks to xmonad package.yaml

* Add more warnings

* Add some of the stuff to xmonad-contrib

* Make `xmonad` / `xmonad-contrib` be extra deps

# Install redshift

# Ensure MIT LICENSE is actually the one used

I don't have internet ATM, I need to verify that the "MIT" specified in the
cabal file matches the license used in the repo.

# Figure out why CPU is locked at 3ghz

# Manage xmonad process

If it crashes, restart. If it immediately crashes after restart, then start up
the standard ubuntu manager, whatever that is.

# Figure out what .config/monitors.xml is

Is there some sort of builtin way to do autorandr?

# Sort .gitignore

# Have .bashrc and zsh rc complain about missing env/settings.sh, once it matters


# Bash Completions for cfg command?

`complete -o default -o nospace -F _git cfg` doesn't work.

# System to mitigate destructiveness of "rm -r"

Of course, you should always have backups. However, it may have been some time
since your last one, and it is inconvenient to restore. It'd be best to avoid
accidental "rm -rf ~/". Unfortuantely, I have not been able to find any
out-of-the-box ways to do this, particularly not one that generalizes beyond `rm
-rf` prevention.

One nice mitigation is described in https://serverfault.com/a/268628 - use a
bash alias so that -I will get passed to rm and it will prompt for large
deletions.

However, note that this will not apply to other invocations of rm, like from
scripts.  That could be managed by having `.local/bin/rm` (or hell, just put it
in usr bin), and having it do a GUI prompt.  It'd also output a note on stdout
that it is bringing up the GUI prompt.  Lets say you are accessing via SSH and
cannot access the GUI.  To handle this case, there might be a special env var to
bypass the GUI.

One decent way is to make a dir unwriteable. Then, have a script for adding
subdirs that makes the dir writeable, adds the subdir, and makes the dir
unwriteable again. Unfortunately, this approach can't be used directly for the
home dir because things expect to be able to modify it.  Ugly thing about this
is that it'd mean structuring my dirs with this in mind.  It also does not avoid
accidental rm -rf destruction that is more localized.

Best solution yet - shadow dir of hard links -
https://stackoverflow.com/a/8590342/1164871 .  However, this relies on inotify
and that doesn't work well for recursive watching of big trees.  There are a
couple ways to resolve this:

* A fuse filesystem, such as loggedfs.  A fuse filesystem could solve this in
  the first place by having backups / disallowing delete / etc.

* fanotify  http://www.lanedo.com/filesystem-monitoring-linux-kernel/

  - https://github.com/jeffwalter/fsnoop

A nice additional mitigation

# Use fatrace? https://launchpad.net/fatrace

# Find a utility to keep a process running

* Keep keynav running, report errors

*

# Log every command run by windows-P

* Report log if the program exits with an error code

* Ideally, make it easy to bring up a terminal with the tail of the log

* Limit the length of the log?

# Figure out dmcrypt integrity checking for encryption

# Project idea: super fast dmcrypt

My ideal implementation would be weak but fast encryption: Have a fast way to go
from byte location to bytes to xor when writing to the disk. This is weak, but
also could be super fast. Weakness is that if the attacker can image your disk
while in use, and they can make predictions about the contents of changed
blocks, then they can decrypt.

# decrypt on unsuspend

https://github.com/vianney/arch-luks-suspend
https://github.com/jonasmalacofilho/ubuntu-luks-suspend

# Switch to NILFS so that everything is recoverable?

No need for the fanotify / shadow hardlinks. Also recovers from accidental
modifications.

# Setup firefox sync

* Use password management? Ehhh, would only trust if it had 2fa

# Git hook

https://coderwall.com/p/jp7d5q/create-a-global-git-commit-hook

# Consider a pre-commit hook that prevents committing secrets, beyond just .gitignore

https://github.com/cypher/dotfiles/blob/master/git-hooks/dont-commit-token.rb

https://github.com/awslabs/git-secrets

Also have a white list of the use of "mgsloan" and "treetop".

# TODO actually rename .dotfiles to .dotfiles.git

# TODO remove use of --work-tree from alias, it's not needed any more

# TODO figure out why ls-files is breaking

# TODO whitelist in git hook

# TODO make sure git hook works properly with "git commit -a"

what if the precommit hook gets called before the add?? It probably doesn't.

# TODO move xmonad submodules into oss. Update readme.md

# System for git which allows me to work on commit message, stash it, etc

Use the approach here
https://coderwall.com/p/jp7d5q/create-a-global-git-commit-hook to add to
existing repos.

# TODO setup noscript / umatrix

# TODO test REISUB
