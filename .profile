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

# set PATH so it includes user's private bin if it exists
if [ -d "$HOME/.local/bin" ] ; then
    PATH="$HOME/.local/bin:$PATH"
fi

export PATH="$HOME/env/bin:$PATH"

if [ -x $HOME/env/untracked/settings.sh ]; then
    source $HOME/env/untracked/settings.sh
else
    echo "~/env/untracked/settings.sh does not exist or is not executable"
fi

mkdir -p $HOME/env/untracked/
if ! [ -z "$HIDPI" ]; then
   export GDK_SCALE=2
   export GDK_DPI_SCALE=0.75
   echo 'URxvt*font: xft:Bitstream Vera Sans Mono:pixelsize=20' > $HOME/env/untracked/xresources-font
else
   echo 'URxvt*font: xft:Bitstream Vera Sans Mono:pixelsize=10' > $HOME/env/untracked/xresources-font
fi

# .profile also gets run by tmux / screen. Skip init-only startup in
# that case.
if ! [[ "$TERM" =~ "screen".* ]] && [ -z "$DISPLAY" ] ; then
  # Faster key repeat
  xset r rate 200 30

  # Set mouse acceleration to 4x with no threshold
  xset m 4/1 0
fi

# Seems to be needed for android studio to launch
export _JAVA_AWT_WM_NONREPARENTING=1

# Set default browser for commands like "man -H"
export BROWSER=google-chrome
