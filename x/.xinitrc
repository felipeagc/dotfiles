xset r rate 230 60
hsetroot -solid "#333333"
picom -b
dunst &
dwmstatus &

exec dbus-launch dwm
