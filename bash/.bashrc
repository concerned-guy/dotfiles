#
# ~/.bashrc
#

# If not running interactively, don't do anything
[[ $- != *i* ]] && return
alias ls='ls --color=auto'
export PS1='\[\033[00;32m\]\u@\h\[\033[00m\]: \[\033[00;36m\]\w\[\033[00m\] \$ '

# Aliases
# customize commands
alias cp='cp -vi'
alias mv='mv -vi'
alias rm='rm -vf'
alias e='emacsclient -t'
alias sdcv='sdcv -c'
alias octave='octave -q --no-gui'
alias aria2c='aria2c -j1 -x4 -Z -d$HOME/usr/tmp'
alias youtube-dl='youtube-dl -o$HOME/usr/tmp/%\(title\)s-%\(id\)s.%\(ext\)s'
alias stow='stow -v'
# deluge aliases
alias dcon='deluge-console'
alias dadd='deluge-console add'
alias drm='deluge-console rm'
alias drma='deluge-console rm {A..z}'
alias drmd='deluge-console rm --rem'
alias dinfo='deluge-console info --sort-r state'
alias dhalt='deluge-console halt'
# platformio aliases
alias pioi='pio init --board esp12e'
alias pior='pio run'
alias pioc='pio run --target clean'
alias piou='pkill dterm; pio run --target upload'

# environment variables. do not change these or you'll have unwanted effects
export PATH="$HOME/bin:/usr/local/sbin:/usr/sbin:/sbin:$PATH"
export TERM='xterm-256color'
export EDITOR='emacsclient -t'
export VISUAL='emacsclient -t'
export GTAGSLABEL='ctags'
export SDCV_PAGER='less'
export PAGER='less'
export LESS='FRXMi'

# bash-completion
[[ -r /usr/share/bash-completion/bash_completion ]] &&
    . /usr/share/bash-completion/bash_completion
complete -c man which # complete arguments

# Path to the bash it configuration
export BASH_IT="$HOME/.bash-it"
source "$BASH_IT"/bash_it.sh

