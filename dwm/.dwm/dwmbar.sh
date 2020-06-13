#!/bin/bash

SEP='|'

# battery() {
# 	bat="$(cat /sys/class/power_supply/BAT?/capacity)"
# 	status="$(cat /sys/class/power_supply/BAT?/status)"
# 	if [ "$status" = "Charging" ]; then
# 		ico=""
# 	else
# 		if [ "$bat" -lt 15 ]; then
# 			ico="   "
# 		elif [ "$bat" -lt 30 ]; then
# 			ico="   "
# 		elif [ "$bat" -lt 45 ]; then
# 			ico=" "
# 		elif [ "$bat" -lt 60 ]; then
# 			ico=" "
# 		elif [ "$bat" -lt 75 ]; then
# 			ico=" "
# 		elif [ "$bat" -lt 90 ]; then
# 			ico=" "
# 		elif [ "$bat" -le 100 ]; then
# 			ico=" "
# 		fi
# 	fi
# 	printf "%s %s%%" "$ico" "$bat"
# }

_date() {
	printf " %s $SEP  %s" "$(date "+%I:%M %p")" "$(date "+%Y-%m-%d")"
}

volume() {
	vol="$(amixer get Master | awk -F"[][%]" '$0~/%/ { print $2; exit }')"
	if [ "$vol" -lt 30 ]; then
		ico=""
	elif [ "$vol" -lt 60 ]; then
		ico=""
	else
		ico=""
	fi
	amixer get Master | grep -q '\[off\]' && ico="婢"
	printf "%s %s%%" "$ico" "$vol"
}

while :; do
	xsetroot -name " $(volume) $SEP $(_date) "
	sleep 1
done
