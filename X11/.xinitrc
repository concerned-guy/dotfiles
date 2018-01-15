# X startup script

# Software requirements
#   emacs feh imagemagick mpd udiskie xscreensaver netpbm pamixer

cat "$(find $HOME/.Xcolor.d/ -type f | shuf -n 1)" "$HOME/.Xresources" | xrdb -load - &
xsetroot -cursor_name left_ptr &
setxkbmap -option ctrl:nocaps &
xset r rate 450 30 &
tp d &
wallpaper.sh &
emacs --daemon &
# ibus-daemon &
# mpd &
# udiskie &
xscreensaver -no-splash &

# dwm status line
while sleep 1; do
    xsetroot -name "$(printf " %s%s%s" \
                            "[ BAT $(acpi | tr -d , | awk '{print $3, $4}') ]" \
                            "[ VOL $(amixer sget Master | grep -m1 '\w*%' | tr -d '][' | awk '{print $6, $5}') ]" \
                            "[ $(date '+%R') ]")"
done &
exec dwm
