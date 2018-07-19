#
# ~/.bash_profile
#

[[ -f $HOME/.bashrc ]] && . $HOME/.bashrc

[[ -z $DISPLAY && $(tty) = /dev/tty1 ]] && { startx; logout; }
