#
# ~/.bash_profile
#

[[ -f $HOME/.bashrc ]] && . $HOME/.bashrc

if [ -z $DISPLAY ] && [ $(tty) = /dev/tty1 ] && [ $(command -v fvwm) ]; then
    startx; logout
elif [ -z $STY ] && [ $(command -v screen) ]; then
    screen; logout
fi
