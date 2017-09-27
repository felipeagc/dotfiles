touch /tmp/felipe_restart_polybar

sleep 2

polybar main & disown

while true
do
	sleep 3

	if [ "1" == "$(cat /tmp/felipe_restart_polybar)" ]; then
		killall polybar
		cp /dev/null /tmp/felipe_restart_polybar
		sleep 2
		polybar main & disown
	fi
done
