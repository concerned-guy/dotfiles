# X startup script

cat "$HOME/.Xresources" "$(find $HOME/.Xcolor.d/ -type f | shuf -n 1)" | xrdb -load - &
xsetroot -cursor_name left_ptr &
setxkbmap -option ctrl:nocaps &
xset r rate 450 30 &
tp d &
wallpaper.sh &

devmon &
emacs --daemon &
ibus-daemon &
# mpd &
lxqt-notificationd &
xscreensaver -no-splash &

# dwm status line
while sleep 1; do
  xsetroot -name "[ BAT $(battery) ][ VOL $(volume) ][ $(date '+%R') ]"
done &
exec dwm

# NOTE: Requires these packages
#   feh imagemagick
#   emacs ibus lxqt-notificationd udevil
#   xscreensaver netpbm
#   sutils-git
