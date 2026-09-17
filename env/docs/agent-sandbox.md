# Agent sandbox

Run from the directory the agent should be able to edit:

```sh
cd ~/project
agent-sandbox claude
agent-sandbox codex
agent-sandbox claude --resume
agent-sandbox codex resume --last
agent-sandbox --shell codex
```

Options after the agent name go to that CLI. Both skip tool permission prompts
inside Bubblewrap; first-run onboarding or workspace trust prompts can still
appear. Failure to create the sandbox stops the command. The installed
CLIs are used, so updating them also updates subsequent sandbox launches.

The current directory is writable at `/workspace`. Each directory and agent has
a persistent home under `~/.local/state/agent-sandbox/`, with separate sessions,
settings, and caches. Renaming the project selects a new home. The first launch
copies only the agent's existing file-based login credentials. Global settings,
hooks, plugins, and history are not copied. If login is missing or expires, use
`agent-sandbox claude auth login` or `agent-sandbox codex login --device-auth`.
Authentication changes inside the sandbox are not written back to the host login.

The rest of the host home is hidden. System tools, selected system configuration,
the Nix store and active environment, Rust tools, and the selected CLI installation
are mounted read-only. The Nix daemon, SSH agent, desktop sockets, and host
processes are unavailable. Environment variables are cleared except terminal and
locale settings and the selected provider's API key or OAuth token.

Network access is unrestricted, including localhost and the LAN. Use
`agent-sandbox --offline --shell codex` for an offline shell; agents need network
access to their providers. This is filesystem and process isolation, not a network
or resource limit. The agent can read and transmit project files and its own login
credentials, and can modify or delete anything in the selected directory. Its
shell commands use the same boundary as its built-in tools.

Symlinks to files outside the selected directory cannot reach hidden host paths.
Git worktrees and submodules whose Git metadata lives outside that directory need
to be launched from a common parent or replaced with a standalone checkout.
For `~/env`, the home repository's `~/.home.git` is deliberately unavailable.
Tools installed elsewhere in the host home are not exposed automatically.

Bubblewrap is declared in the public Nix environment. Linux must permit
unprivileged user namespaces. The mount policy follows the
[Bubblewrap security guidance](https://github.com/containers/bubblewrap#sandbox-security).
Codex's bypass option is intended for an
[external sandbox](https://learn.chatgpt.com/docs/security).
