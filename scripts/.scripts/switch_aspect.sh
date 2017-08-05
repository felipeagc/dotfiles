xrandr | grep current | grep 1920

if [ $? -ne 0 ]; then
	# 21:9
	cp /dev/null /tmp/felipe_restart_polybar
	echo "1" >> /tmp/felipe_restart_polybar
	xrandr --output HDMI-0 --mode 1920x1080
else
	# 16:9
	cp /dev/null /tmp/felipe_restart_polybar
	echo "1" >> /tmp/felipe_restart_polybar
	xrandr --output HDMI-0 --mode 2560x1080
fi
