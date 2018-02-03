# ~/.profile: executed by the command interpreter for login shells.
# This file is not read by bash(1), if ~/.bash_profile or ~/.bash_login
# exists.
# see /usr/share/doc/bash/examples/startup-files for examples.
# the files are located in the bash-doc package.

# the default umask is set in /etc/profile; for setting the umask
# for ssh logins, install and configure the libpam-umask package.
#umask 022

# if running bash
if [ -n "$BASH_VERSION" ]; then
    # include .bashrc if it exists
    if [ -f "$HOME/.bashrc" ]; then
	. "$HOME/.bashrc"
    fi
fi

# set PATH so it includes user's private bin if it exists
if [ -d "$HOME/bin" ] ; then
    PATH="$HOME/bin:$PATH"
fi

export PATH="$HOME/.cargo/bin:$PATH"
export PATH="$HOME/.local/bin:$PATH"
export PATH="$HOME/env/bin:$PATH"

export XMONAD_DATA_DIR="$HOME/env"
export XMONAD_CONFIG_DIR="$HOME/env"

# FIXME: After installing nvidia driver, this no longer seems to be necessary.
# Figure out if it is
#
# source /home/mgsloan/env/settings.sh
#
# if [ "$USE_HIDPI" = true ] ; then
#    xrandr --dpi 282
# fi
if [ -e $HOME/.nix-profile/etc/profile.d/nix.sh ]; then . $HOME/.nix-profile/etc/profile.d/nix.sh; fi # added by Nix installer

if [ -e $HOME/dl/google-cloud-sdk/path.bash.inc ]; then
    source $HOME/dl/google-cloud-sdk/path.bash.inc
    source $HOME/dl/google-cloud-sdk/completion.bash.inc
fi
if [ -e /home/mgsloan/.nix-profile/etc/profile.d/nix.sh ]; then . /home/mgsloan/.nix-profile/etc/profile.d/nix.sh; fi # added by Nix installer
