# Prepends directories to path, if they exist.
function pathadd {
    if [ -d "$1" ] && [[ ":$PATH:" != *":$1:"* ]]; then
        export PATH="$1:$PATH"
    fi
}

# Removes duplicate elements from path environment variables without affecting precedence order
function clean_path {
    export $1=`perl -e "print join(q{:}, grep(!\\$saw{\\$_}++,split(/:/,\\$ENV{$1})));"`
}

# Source a file if it exists
function source_if_exists {
    if [ -f "$1" ]; then
        . "$1"
    fi
}

source_if_exists /etc/lc.bashrc
source_if_exists /etc/bashrc
source_if_exists /usr/local/tools/dotkit/init.sh

# Test whether this is an interactive shell.
case $- in
    *i*) interactive=true ;;
    *)   interactive=false ;;
esac

# This is setup specifically for interactive shells
if $interactive; then
    # Pick a good terminal for the machine we're on
    OS=$(uname -s)
    case $OS in
        'Linux')   export TERM='xterm-color' ;;
        'Darwin' ) export TERM='xterm-color' ;;
        'AIX')     export TERM='aixterm' ;;
        * )        export TERM='vt100' ;;
    esac

    # X settings
    if [ -f ~/.Xdefaults ]; then
        xrdb ~/.Xdefaults
    fi

    # Use an erase character that emacs understands properly
    stty erase ^?

    # use the system's max stack size: prevents crashes on cluster apps
    ulimit -s $(ulimit -Hs)
    ulimit -c unlimited

    # color prompt with hostname and current directory.  Here are some color codes:
    cyan="\[\033[0;36m\]"
    gray="\[\033[0;37m\]"
    ltblue="\[\033[1;34m\]"
    green="\[\033[1;32m\]"
    reset="\[\033[0m\]"

    # Use a blue prompt by default
    host_color="$ltblue"

    # If we're in a SLURM salloc shell use a green prompt so we know it's parallel.
    if [ ! -z "$SLURM_JOBID" ]; then
        host_color="$green"
    fi

    export PS1="${gray}(${host_color}\h${gray}):${cyan}\W${gray}\$${reset} "
    export PROMPT_COMMAND='echo -ne "\033]0;${USER} @ ${HOSTNAME} : ${PWD}\007"'
fi


# bash history options
shopt -s histappend              # append instead of overwrite (good for multiple sessions)
export HISTCONTROL=ignoreboth    # don't save duplicate entries
export HISTSIZE=10000


export EDITOR="emacs -nw"

# Macports setup is only done on Darwin.
if [ "$OS" = 'Darwin' ]; then
    #export MACPORTS_HOME=/opt/local
    export MACPORTS_HOME=$HOME/macports
    if [ -f $MACPORTS_HOME/share/macports/setupenv.bash ]; then
        . $MACPORTS_HOME/share/macports/setupenv.bash
    fi

    # put GNU coreutils in path ahead of BSD tools
    pathadd $MACPORTS_HOME/libexec/gnubin
fi


# Figure out which ls we're using and set some options.
if ls --color -d . >/dev/null 2>&1; then
    export LS_OPTIONS="--color=auto -F -B -I '*pyc'"
elif ls -G -d . >/dev/null 2>&1; then
    export LS_OPTIONS="-G -F"
fi

alias ls="ls $LS_OPTIONS"
alias ll="ls -lh $LS_OPTIONS"
alias l=ll

if [ -e "$(which dircolors)" -a -e ~/.dir_colors ]; then
    eval $(dircolors ~/.dir_colors)
fi


# Likewise for grep
export GREP_OPTIONS=--color=auto


# disable stupid X11 programs that ask for your ssh password
export SSH_ASKPASS=


# Other convenient aliases
alias rb="rm -f *~ .*~ \#* .\#*"
alias f='finger'
alias more='less'
alias screen='screen -R -D'


# Init other config files as necessary.  File should be put in ~/.bash.d,
# and can be disabled by putting a ~ anywhere in the name.
bashd=$(readlink -f ~/.bash.d)
if [ -d $bashd ]; then
  for config_file in $(find $bashd ! -path '*~*' -type f) ; do
    . $config_file
  done
fi

pathadd /usr/sbin
pathadd $HOME/bin
pathadd .

clean_path PATH
clean_path DYLD_LIBRARY_PATH


# Use TextMate if we're in a GUI session and it exists, otherwise emacs.
# Do editor setup after path setup as it depends on the PATH
mate=`which mate 2>/dev/null`
if [ "$Apple_PubSub_Socket_Render" != "" -a ! -z "$mate" ]; then
    export EDITOR="mate -w"
fi
