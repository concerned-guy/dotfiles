# X startup script

xrandr | grep "HDMI-1 connected" && xrandr --output eDP-1 --off --output HDMI-1 --auto &
feh --bg-scale --no-fehbg ~/usr/img/$(shuf -n1 ~/usr/img/art.txt) &
cat $(shuf -e -n1 ~/.Xcolor.d/*) ~/.Xresources | xrdb -load - &
emacs --daemon &
exec icewm
