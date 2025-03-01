#!/bin/bash

# Get volume status (and set to 'Muted' if muted)
volume=$(pactl get-sink-volume @DEFAULT_SINK@ | grep -oP '\d+(?=%)' | head -n 1)
muted=$(pactl get-sink-mute @DEFAULT_SINK@ | awk '{print $2}')
if [ "$muted" == "yes" ]; then
  volume="Muted"
else
    volume="$volume%"
fi

# Get CPU usage
cpu=$(top -bn1 | grep "Cpu(s)" | sed "s/.*, *\([0-9.]*\)%* id.*/\1/" | awk '{print 100 - $1"%"}')

# Get memory usage
memory=$(free -h | grep Mem | awk '{print $3 "/" $2}')

# Get battery status and format it
battery_info=$(upower -i $(upower -e | grep '/battery') | grep -E "state|percentage")
battery_state=$(echo "$battery_info" | grep -i "state" | awk '{print $2}' | sed 's/^\(.\)/\U\1/') # Capitalize first letter of state
battery_percentage=$(echo "$battery_info" | grep -i "percentage" | awk '{print $2}')
battery="$battery_state $battery_percentage"

# Get backlight brightness
backlight_max=$(cat /sys/class/backlight/*/max_brightness)
backlight_current=$(cat /sys/class/backlight/*/brightness)
backlight_percentage=$(( 100 * backlight_current / backlight_max ))

# Get current time
clock=$(date +"Date [%a %d %b %Y] [%I:%M %p]")

# Output all the gathered information
echo "Volume: $volume | Cpu: $cpu | Mem: $memory | Battery: $battery | Backlight: $backlight_percentage% | $clock"
