# enable programmable completion features (you don't need to enable
# this, if it's already enabled in /etc/bash.bashrc and /etc/profile
# sources /etc/bash.bashrc).
if ! shopt -oq posix; then
  if [ -f /usr/share/bash-completion/bash_completion ]; then
    . /usr/share/bash-completion/bash_completion
  elif [ -f /etc/bash_completion ]; then
    . /etc/bash_completion
  fi
fi

if [ ! -e ~/env/untracked/settings.sh ]; then
    echo "~/env/untracked/settings.sh doesn't exist, but it's expected to"
fi

if [ -e ~/env/untracked/local.bashrc ]; then
    source ~/env/untracked/local.bashrc
else
    echo "~/env/untracked/local.bashrc doesn't exist, but it's expected to"
fi

eval "$(stack --bash-completion-script stack)"

if [ -x "$(command -v hub)" ]; then
    eval "$(hub alias -s)"
fi

# https://serverfault.com/a/268628
alias rm='rm -I'

alias sl='ls'

function kill_detatched_tmux() {
    tmux list-sessions | grep -E -v '\(attached\)$' | while IFS='\n' read line; do
        tmux kill-session -t "${line%%:*}"
    done
}

# The next line updates PATH for the Google Cloud SDK.
if [ -f '/home/mgsloan/.local/google-cloud-sdk/path.bash.inc' ]; then . '/home/mgsloan/.local/google-cloud-sdk/path.bash.inc'; fi

# The next line enables shell command completion for gcloud.
if [ -f '/home/mgsloan/.local/google-cloud-sdk/completion.bash.inc' ]; then . '/home/mgsloan/.local/google-cloud-sdk/completion.bash.inc'; fi
