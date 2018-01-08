# X startup script

# Software requirements
#   compton dunst emacs feh imagemagick mpd udiskie unclutter xscreensaver netpbm sutils-git

xrdb -merge $HOME/.Xresources &
xsetroot -cursor_name left_ptr &
setxkbmap -option ctrl:nocaps &
xset r rate 450 30 &
tp d &
wallpaper.sh &
compton &
dunst &
emacs --daemon &
ibus-daemon &
mpd &
udiskie &
unclutter &
xscreensaver -no-splash &

# dwm status line
while sleep 1; do
    xsetroot -name "$(date +"%a %b %d, %R") | VOL $(volume) | BAT $(battery)"
done &
exec dwm
