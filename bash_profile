#
# ~/.bash_profile
#

[[ -f $HOME/.bashrc ]] && . $HOME/.bashrc

if [ ! -n "$DISPLAY" ] && [ "$(tty)" = /dev/tty1 ] && [ $(command -v icewm) ]; then
    startx
    logout
else
    if [ ! -n "$(pgrep -x emacs)" ] && [ $(command -v emacs) ]; then
        emacs --daemon 2> /dev/null &
    fi
    if [ ! -n "$STY" ] && [ $(command -v screen) ]; then
        screen
        logout
    fi
fi
