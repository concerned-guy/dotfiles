#
# ~/.bashrc
#

# If not running interactively, don't do anything
[[ $- != *i* ]] && return
alias ls='ls --color=auto'

# Minimalistic
alias e='emacsclient -t'
export PATH="$HOME/bin:$PATH:/usr/local/sbin:/usr/sbin:/sbin"
export EDITOR='emacsclient -t'
export MC_XDG_OPEN='exo-open'

