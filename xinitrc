# X startup script

cat ~/.Xresources "$(ls ~/.Xcolor.d | shuf -n 1)" | xrdb -load - &
if [ -d ~/bin ]; then
   keyboard.sh &
   tp d &
   wallpaper.sh &
fi

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
