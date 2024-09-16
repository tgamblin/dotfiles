typeset -U path

# Prepends directories to path, if they exist.
#      pathadd /path/to/dir            # add to PATH
# or   pathadd OTHERPATH /path/to/dir  # add to OTHERPATH
# Prepends directories to path, if they exist.
#      pathadd /path/to/dir            # add to PATH
# or   pathadd OTHERPATH /path/to/dir  # add to OTHERPATH
function pathadd {
    if [ -d "$1" ]; then
        path=($1 "$path[@]")
    elif [ -d "$2" ]; then
        eval "$1=($2 \$$1[@])"
    fi
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
