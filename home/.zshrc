#------------------------------------------------------------------------
# Spack environment and paths
#------------------------------------------------------------------------
source_if_exists $HOME/src/spack/share/spack/setup-env.sh
default_env=~/src/spack/var/spack/environments/default/.spack-env/view

if [ -x ${default_env}/bin/python ]; then
    export SPACK_PYTHON=${default_env}/bin/python
fi

pathadd /usr/sbin
pathadd /usr/global/etc/bin     # LC's sbin tools
pathadd $default_env/bin
pathadd $HOME/bin
pathadd $HOME/bin/scripts
pathadd $HOME/bin/eor-request
pathadd /usr/local/opt/ruby/bin  # homebrew ruby

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

# Use TextMate if we're in a GUI session and it exists, otherwise emacs.
# Do editor setup after path setup as it depends on the PATH
use_textmate=false
if [ "$Apple_PubSub_Socket_Render" != "" -a "$use_textmate" = "true" ]; then
    mate=$(which mate 2>/dev/null)
    if [ ! -z "$mate" ]; then
        export EDITOR="mate -w"
    fi
fi

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

#------------------------------------------------------------------------
# Prompt
#------------------------------------------------------------------------
# Use a blue prompt by default
host_color="blue"
if [ "$USER" = "root" ]; then
    # If we're in a root shell, make the prompt red
    host_color="red"
elif [[ "$(hostname -s)" == "rz"* ]]; then
    host_color='magenta'
elif [ ! -z "$SLURM_JOBID" ]; then
    # If we're in a SLURM salloc, use a green prompt
    host_color="green"
fi

# use PROMPT instead of PS1, as PS1 interferes with dash
export PROMPT="%F{8}%B(%b%F{$host_color}%B%m%b%F{8}%B)%b%F{white}:%F{cyan}%1~%F{8}%B>%b%f "
precmd() { echo -ne "\033]0;$(hostname -s): ${PWD}\007" }

# timestamp on right
# export RPROMPT="%F{8}[%D{%F} %D{%L:%M:%S}]%f"

#------------------------------------------------------------------------
# Make key bindings for M-[fb], M-delete, etc. work
#------------------------------------------------------------------------
bindkey -me >& /dev/null
bindkey '[3~' delete-char
bindkey '[4~' end-of-line
bindkey '[1~' beginning-of-line
bindkey 'OH' beginning-of-line
bindkey 'OF' end-of-line
bindkey '[H' beginning-of-line
bindkey '[F' end-of-line
bindkey '[1;3D' beginning-of-line
bindkey '[1;3C' end-of-line
bindkey '[1;9C' forward-word
bindkey '[1;9D' backward-word

#------------------------------------------------------------------------
# Completion
#------------------------------------------------------------------------
# Load Git completion
zstyle ':completion:*:*:git:*' script ~/.completion.d/git-completion.bash
fpath=(~/.completion.d $fpath)

autoload -Uz compinit && compinit

#------------------------------------------------------------------------
# Rust
#------------------------------------------------------------------------
source_if_exists "$HOME/.cargo/env"

#------------------------------------------------------------------------
# Other settings
#------------------------------------------------------------------------

# Make grep highlight search string in red
export GREP_OPTIONS='--color=auto'

# Python startup file
export PYTHONSTARTUP=~/.python

alias more='less'
alias screen='screen -R -D'
alias d=docker
alias k=kubectl

# make which behave like it does elsewhere
alias which='whence -p'

# zsh time doesn't time shell functions, or behave like anyone wants
if command -v /usr/bin/time &>/dev/null; then
    alias time='/usr/bin/time'
fi

# don't delete whole paths with M-delete -- / is a separator
autoload -U select-word-style
select-word-style bash

# dont' error out when globs match nothing -- pass the unevaluated glob
setopt nonomatch

#------------------------------------------------------------------------
# History
#------------------------------------------------------------------------

export HISTSIZE=10000      # set history size
export SAVEHIST=10000      # save history after logout
setopt INC_APPEND_HISTORY  # append into history file
setopt HIST_IGNORE_DUPS    # save only one command if 2 are same
setopt EXTENDED_HISTORY    # add timestamp for each entry

#------------------------------------------------------------------------
# Antigen
#------------------------------------------------------------------------
antigen_setup() {
    . $HOME/.zsh/antigen.zsh

    # `antigen apply` has concurrency issues, so make it a critical section
    if [ ! -e ~/.zsh/lock ]; then
        mkdir -p ~/.zsh
        touch ~/.zsh/lock
    fi

    count=0
    ntries=2000
    while ! ln ~/.zsh/lock ~/.zsh/taken 2> /dev/null; do
        sleep .001

        count=$(($count+1))
        if [ "$count" = "$ntries" ]; then
            echo "Couldn't get antigen lock after $ntries tries."
            echo "Consider removing ~/.zsh/taken."
            return
        fi
    done
    # begin critical section

    antigen bundle zsh-users/zsh-syntax-highlighting
    antigen bundle zsh-users/zsh-autocomplete
    antigen bundle zsh-users/zsh-autosuggestions
    antigen apply || echo "`antigen apply` failed"

    # end critical section
    rm -f ~/.zsh/taken
}
antigen_setup

bindkey '^[[Z' reverse-menu-complete
bindkey '^@' autosuggest-accept
bindkey '^\n' autosuggest-execute
