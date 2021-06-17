#------------------------------------------------------------------------
# Initial definitions
#------------------------------------------------------------------------
# Prepends directories to path, if they exist.
#      pathadd /path/to/dir            # add to PATH
# or   pathadd OTHERPATH /path/to/dir  # add to OTHERPATH
function pathadd {
    # If no variable name is supplied, just append to PATH
    # otherwise append to that variable.
    _pa_varname=PATH
    _pa_new_path="$1"
    if [ -n "$2" ]; then
        _pa_varname="$1"
        _pa_new_path="$2"
    fi

    # Do the actual prepending here.
    eval "_pa_oldvalue=\$${_pa_varname}"

    if [ -d "$_pa_new_path" ] && [[ ":$_pa_oldvalue:" != *":$_pa_new_path:"* ]];
    then
        # convert path to absolute path if it is not one
        _pa_new_path=$(cd $_pa_new_path && pwd)

        # Add it to the PATH
        if [ -n "$_pa_oldvalue" ]; then
            eval "export $_pa_varname=\"$_pa_new_path:$_pa_oldvalue\""
        else
            export $_pa_varname="$_pa_new_path"
        fi
    fi
}

# Remove duplicate entries from PATH
function clean_path {
    export PATH=$(echo "$PATH" | awk -F: '{for (i=1;i<=NF;i++) { if ( !x[$i]++ ) printf("%s:",$i); }}')
}

# Source a file if it exists
function source_if_exists {
    if [ -f "$1" ]; then
        . "$1"
    fi
}

# Other convenient aliases
alias rb="rm -f *~ .*~ \#* .\#*"
alias f='finger'

# disable stupid X11 programs that ask for your ssh password
export SSH_ASKPASS=

source_if_exists /etc/lc.bashrc
source_if_exists /etc/bashrc

#------------------------------------------------------------------------
# EXIT HERE if this is a non-interactive shell
#------------------------------------------------------------------------
# Test whether this is an interactive shell.
case $- in
    *i*) ;;       # interactive
    *) return ;;  # non-interactive
esac

#------------------------------------------------------------------------
# Spack environment and paths
#------------------------------------------------------------------------
time source_if_exists $HOME/src/spack/share/spack/setup-env.sh
default_env=~/src/spack/var/spack/environments/default/.spack-env/view

pathadd /usr/sbin
pathadd /usr/global/etc/bin     # LC's sbin tools
pathadd $default_env/bin
pathadd $HOME/bin
pathadd $HOME/bin/scripts
pathadd $HOME/bin/eor-request

#------------------------------------------------------------------------
# Go
#------------------------------------------------------------------------
export GOPATH="${HOME}/src/go"
pathadd "${GOPATH}/bin"

#------------------------------------------------------------------------
# Editors
#------------------------------------------------------------------------
# Emacs setup
#export EDITOR='emacsclient -t -nw'   # Set up emacs as a server
export EDITOR="emacs -nw"           # Set up emacs without server.

# Various emacs aliases.
export ALTERNATE_EDITOR=""
alias estop="emacsclient -e '(client-save-kill-emacs)'"
alias emacs="$EDITOR"
alias e="$EDITOR"

#------------------------------------------------------------------------
# ls options
#------------------------------------------------------------------------
# Give ls decent colors and options depending on version
if ls --color -d . >/dev/null 2>&1; then
    export LS_OPTIONS="--color=auto -F -B"
elif ls -G -d . >/dev/null 2>&1; then
    export LS_OPTIONS="-G -F"
fi

alias ls="ls $LS_OPTIONS"
alias ll="ls -lh $LS_OPTIONS"
alias l=ll

# load appropriate dir colors for terminal
if [ -e "$(which dircolors)" ]; then
    if [ "$TERM" = "xterm-256color" -a -e ~/.dir_colors.256 ]; then
        eval $(dircolors ~/.dir_colors.256)
    else
        eval $(dircolors ~/.dir_colors)
    fi
fi

#------------------------------------------------------------------------
# Limits and shell settings
#------------------------------------------------------------------------
stty erase '^?'

# use the system's max stack size: prevents crashes on cluster apps
ulimit -s $(ulimit -Hs)

# enable/disable core dumps
# ulimit -c $(ulimit -Hc)
ulimit -c 0

# Make bash set LINES and COLUMNS after each command.
shopt -s checkwinsize

# bash history options
shopt -s histappend              # append instead of overwrite (good for multiple sessions)
export HISTCONTROL=ignoreboth    # don't save duplicate entries
export HISTSIZE=10000

#------------------------------------------------------------------------
# Prompt
#------------------------------------------------------------------------
# color prompt with hostname and current directory.  Here are some color codes.
# These need to be wrapped in \001 and \002 so that readline knows to ignore
# non-printing characters.
red="\001\[\033[0;31m\]\002"
cyan="\001\[\033[0;36m\]\002"
gray="\001\[\033[1;38m\]\002"
green="\001\[\033[0;32m\]\002"
ltgreen="\001\[\033[1;32m\]\002"
ltblue="\001\[\033[1;34m\]\002"
reset="\001\[\033[0m\]\002"

red="\[\033[0;31m\]"
cyan="\[\033[0;36m\]"
gray="\[\033[1;90m\]"
green="\[\033[0;32m\]"
ltgreen="\[\033[1;32m\]"
ltblue="\[\033[1;34m\]"
reset="\[\033[0m\]"

# Use a blue prompt by default
host_color="$ltblue"

if [ "$USER" = "root" ]; then
    # If we're in a root shell, make the prompt red
    host_color="$red"
elif [ ! -z "$SLURM_JOBID" ]; then
    # If we're in a SLURM salloc shell use a green prompt so we know it's parallel.
    host_color="$green"
fi

export PS1="${gray}(${host_color}\h${gray}):${cyan}\W${gray}\$${reset} "
export PROMPT_COMMAND='echo -ne "\033]0;$(hostname -s): ${PWD}\007"'

#------------------------------------------------------------------------
# Completion
#------------------------------------------------------------------------
source_if_exists $HOME/.completion.d/cmake-completion.sh
source_if_exists $HOME/.completion.d/git-completion.bash

#------------------------------------------------------------------------
# Rust
#------------------------------------------------------------------------
source_if_exists "$HOME/.cargo/env"

#------------------------------------------------------------------------
# Other settings
#------------------------------------------------------------------------
# Tell apple to shut up about bash being deprecated
export BASH_SILENCE_DEPRECATION_WARNING=1

# Make grep highlight search string in red
export GREP_OPTIONS='--color=auto'

# Python startup file
export PYTHONSTARTUP=~/.python

alias more='less'
alias screen='screen -R -D'
alias d=docker
alias k=kubectl
