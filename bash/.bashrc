#
# ~/.bashrc
#

# If not running interactively, don't do anything
[[ $- != *i* ]] && return

# Aliases
# customize commands
alias ls='ls --color=auto'
alias cp='cp -vi'
alias mv='mv -vi'
alias rm='rm -vf'
alias e='emacsclient -t'
alias octave='octave -q --no-gui'
alias youtube-dl='youtube-dl -o$HOME/usr/tmp/%\(title\)s-%\(id\)s.%\(ext\)s'
alias stow='stow -v'
alias screen='screen -q'
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
export VISUAL='emacsclient -t'
export PAGER='less'
export LESS='FRXMi'
export MC_XDG_OPEN='exo-open'
export GTAGSLABEL='ctags'

# Path to the bash it configuration
export BASH_IT="$HOME/.bash-it"
source "$BASH_IT"/bash_it.sh

