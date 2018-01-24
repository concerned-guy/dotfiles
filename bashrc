#
# ~/.bashrc
#

# If not running interactively, don't do anything
[[ $- != *i* ]] && return

# Aliases
# customize commands
alias ls='ls --color=auto'
alias e='emacsclient -t'
# torrent aliases
alias trnd='transmission-daemon'
alias trnc='transmission-remote'
# platformio aliases
alias pioi='pio init --board esp12e'
alias pior='pio run'
alias pioc='pio run --target clean'
alias piou='pkill screen; pio run --target upload'

# environment variables. do not change these or you'll have unwanted effects
export PATH="$HOME/bin:$PATH:/usr/local/sbin:/usr/sbin:/sbin"
export TERM='xterm-256color'
export EDITOR='emacsclient -t'
# export VISUAL='emacsclient -t'
# export PAGER='less'
# export LESS='FRXMi'
export MC_XDG_OPEN='exo-open'
# export GTAGSLABEL='ctags'

