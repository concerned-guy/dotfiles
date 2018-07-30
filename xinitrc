# X startup script
xrandr | grep "HDMI-1 conn" &&
xrandr --output eDP-1 --off --output HDMI-1 --auto &
echo | cat .Xcolor.d/$((RANDOM%63))-* - .Xresources | xrdb -load - &
exec fvwm2 -f .fvwm2rc
