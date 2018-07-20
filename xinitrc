# X startup script

xrandr | grep "HDMI-1 conn" &&
xrandr --output eDP-1 --off --output HDMI-1 --auto &
cat $(shuf -e -n1 ~/.Xcolor.d/*) ~/.Xresources | xrdb -load - &
exec fvwm
