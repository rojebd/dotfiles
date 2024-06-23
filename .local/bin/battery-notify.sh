#!/bin/sh

while :
do
    sleep 300;
    battery_life=$(sysctl -n hw.acpi.battery.life | awk '{print $0}')
    b=$(($battery_life))
    battery_status=$(apm | grep 'AC Line status:' | awk '{print $4}')

    if [ "$b" -gt 85 ] && [ "$battery_status" = "on-line" ]; then
       notify-send --icon=$HOME/.config/mako/battery-full-solid.svg "Unplug Battery" "Battery Level is Over 85% Unplug"

    elif [ "$b" -lt 25 ] && [ "$battery_status" = "off-line" ]; then
       notify-send --icon=$HOME/.config/mako/battery-quarter-solid.svg "Plug in Battery" "Battery level is less than 25% plug it in"
    else
      :
    fi

done
