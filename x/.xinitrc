export _JAVA_AWT_WM_NONREPARENTING=1 

xset r rate 230 60
hsetroot -solid "#333333"
picom -b
dunst &
redshift &
dwmstatus &

exec dbus-launch dwm
