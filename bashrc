#
# ~/.bashrc
#

# If not running interactively, don't do anything
[[ $- != *i* ]] && return
alias ls='ls --color=auto'

# Minimalistic
alias e='emacsclient -t'
export EDITOR='emacsclient -t'
export MC_XDG_OPEN='exo-open'
export PATH="$HOME/bin:$PATH"

