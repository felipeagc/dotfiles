function restartpolybar {
	pid=$(pidof polybar)
	echo $pid
	polybar top & disown;
	sleep 0.2
	kill $pid
}

xrandr | grep current | grep 1920

if [ $? -ne 0 ]; then
	# 21:9
	xrandr --output HDMI-0 --mode 1920x1080
	restartpolybar
else
	# 16:9
	xrandr --output HDMI-0 --mode 2560x1080
	restartpolybar
fi
