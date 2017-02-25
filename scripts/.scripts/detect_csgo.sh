PREV_STATE=0

nvidia-settings -a "DigitalVibrance=0" > /dev/null

while :
do
	if [ ! -z $(pidof csgo_linux64) ]
	then
		if [ $PREV_STATE -eq 0 ]
		then
			nvidia-settings -a "DigitalVibrance=1000" > /dev/null
			PREV_STATE=1
		fi
	else
		if [ $PREV_STATE -eq 1 ]
		then
			nvidia-settings -a "DigitalVibrance=0" > /dev/null
			PREV_STATE=0
		fi
	fi
	sleep 5
done
