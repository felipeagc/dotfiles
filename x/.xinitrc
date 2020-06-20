xset r rate 230 60 &
picom -b &
nitrogen --restore &
dunst &
gnome-keyring-daemon --start &
dwmstatus &

csum=$(sha1sum $(which dwm))
new_csum=""
while true
do
    if [ "$csum" != "$new_csum" ]
    then
        csum=$new_csum
		dbus-launch dwm
    else
        exit 0
    fi
    new_csum=$(sha1sum $(which dwm))
    sleep 0.5
done
