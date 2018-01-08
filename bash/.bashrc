#
# ~/.bashrc
#

# If not running interactively, don't do anything
[[ $- != *i* ]] && return

alias ls='ls --color=auto'
PS1='[\u@\h \W]\$ '

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

# environment variables. dont change or you'll have unwanted effects
export PATH="$HOME/bin:$PATH"
export TERM='xterm-256color'
export EDITOR='emacsclient -t'
export VISUAL='emacsclient -t'
export GTAGSLABEL='ctags'
export SDCV_PAGER='less'
export PAGER='less'
export LESS='FRXMi'

# Path to the bash it configuration
export BASH_IT="$HOME/.bash-it"

# Lock and Load a custom theme file
# location /.bash_it/themes/
export BASH_IT_THEME='sirup'

# For theme 'sirup'
if [[ -f /usr/share/git/git-prompt.sh ]]; then
     source /usr/share/git/git-prompt.sh
fi

# Set this to false to turn off version control status checking within the prompt for all themes
export SCM_CHECK=true

# Load Bash It
source "$BASH_IT"/bash_it.sh
