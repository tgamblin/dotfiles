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
elif [ ! -z "$SLURM_JOBID" ]; then
    # If we're in a SLURM salloc, use a green prompt
    host_color="green"
fi

# use PROMPT instead of PS1, as PS1 interferes with dash
export PROMPT="%F{8}%B(%b%F{$host_color}%B%m%b%F{8}%B)%b%F{white}:%F{cyan}%1~%F{8}%B>%b%f "
precmd() { echo -ne "\033]0;$(hostname -s): ${PWD}\007" }

# timestamp on right
export RPROMPT="%F{8}[%D{%F} %D{%L:%M:%S}]%f"

#------------------------------------------------------------------------
# Completion
#------------------------------------------------------------------------
# Load Git completion
zstyle ':completion:*:*:git:*' script ~/.completion.d/git-completion.bash
fpath=(~/.completion.d $fpath)

autoload -Uz compinit && compinit

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
alias kc=kubectl

# don't delete whole paths with M-delete -- / is a separator
autoload -U select-word-style
select-word-style bash

#------------------------------------------------------------------------
# Antigen
#------------------------------------------------------------------------
. $HOME/.zsh/antigen.zsh

antigen bundle zsh-users/zsh-syntax-highlighting
antigen bundle zsh-users/zsh-autosuggestions
antigen apply

bindkey '^[[Z' reverse-menu-complete
bindkey '^@' autosuggest-accept
bindkey '^\n' autosuggest-execute
