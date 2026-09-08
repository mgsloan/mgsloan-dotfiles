# mgsloan's dev setup

# Decisions

* GCP instance that's the main home for projects. Builds, agents, all live here

* Projects are split by informational domains for privacy/security. Locally this categorization will be determined by the path to the repo.

* Use podman locally, also split at these same informational domains

* JJ is used. Less likely for agents to lose data / fumble

* Use btrfs for faster worktrees / CoW build artifacts

  - FIXME: maybe not, intead just share the rust target dir

* Magit-via-tramp is used to view the git state of the GCP instance

* Zed and Emacs will both have commands to switch between local and cloud for a given project.  This will be clearly indicated.

# Goals

* Controlling the agent and build environment for security / privacy /
  efficiency

* Remote agent access

* Access to the latest version when offline

* Ability to work on the project while offline

* Ability to offload compute intensive stuff to the cloud (such as when running on battery when working outside).

* Low latency

* Low cloud cost

# TODO

* [ ] Research how to configure JJ to work well for agents - I believe things get funky if it's a colocated git repo and they use git mutations.

* [ ] Figure out the interaction between JJ and autopush

* [ ] See if jjui is a suffiient replacement for magit

* [ ] Research whether it makes sense to start using JJ locally too or over tramp/ssh.  Is there anything like magit??  I don't need all of magit's features, I mostly just need the status pane.  Maybe it's worthwhile to just work on cloning the Magit-like interface in Zed?

  * Split sections of untracked / unstaged / staged

  * `magit-commit-autofixup`

  - `s` to stage

  - `u` to unstage

  - `k` to kill (delete untracked, or delete hunk, etc)

  - `g` to refresn

  - `F` to open pull menu
    - `-f` to fast-forward only
    - `-r` for rebase
    - `-F` to force
    - `p` for pushRemote
    - `u` for upstream
    - `e` for elsewhere

  - `n` / `p` to move cursor up and down

  - `1` / `2` / `3` / `4` to switch between folding levels

  - `c c` to commit.  `C-c C-c` to create commit once it is open

- [ ] Evaluate what tool should be used for using worktrees along with btrfs CoW.

- [ ] This is probably just something the agent will sort out - but how to develop UI / browser applications?  Ideally I'd also want to be able to follow along locally which might mean some remote desktop support

- [ ] How to configure / structure automatic download?

- [ ] Decide whether podman is also used in the cloud. I think probably it should be, because then

- [ ] Decide whether to use nix.

# Alternatives considered

Initially the hope was to have full mirroring between local and remote:

* Mutagen for bidi sync between laptop and cloud

* Make the laptop available to the agent's build / bench / test. Possibly in a
  way that is transparent to the agent.

Deferring this idea because distributed edits and VCS sync are *fraught*. The complexity is not worth it vs just having separate local repos.

Instead, commits on the cloud repos will be pushed directly to the laptop.

The goals of low latency and offline access are in tension with running harnesses on the cloud.  The natural thing to do is to make the instance running the harnesses be the source of truth for the state of the projects.
