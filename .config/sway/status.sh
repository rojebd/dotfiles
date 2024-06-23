#!/bin/sh

# Sway does not need to be reloaded for the bar to update just change this script

### Time --------------------------------------------------------------------
date=$(date '+Date: %h-%d-%Y')
date_time=$(date '+Time: %I:%M %p')

### Battery Stuff ------------------------------------------------------------
battery_life=$(sysctl -n hw.acpi.battery.life | awk '{print "Battery:", $0"%"}')
battery_status=$(apm | grep 'AC Line status:' | awk '{print $4}')

# Check the AC Line status and set the variable accordingly
if [ "$battery_status" == "off-line" ]; then
  battery_status="Discharging"
elif [ "$battery_status" == "on-line" ]; then
  battery_status="Charging"
elif [ "$battery_status" == "backup power" ]; then
  battery_status="Backup Power"
else
  battery_status="Unknown"
fi

### Other --------------------------------------------------------------------
separator=" | "
system=$(uname -snr | cut -d '-' -f1) # On FreeBSD it justs says FreeBSD by Default

### Memory --------------------------------------------------------------------

total_ram_gb=$(echo "scale=2; $(sysctl -n hw.physmem) / (1024^3)" | bc)
free_ram_gb=$(echo "scale=2; (($(sysctl -n vm.stats.vm.v_free_count) + $(sysctl -n vm.stats.vm.v_inactive_count) + $(sysctl -n vm.stats.vm.v_cache_count)) * $(sysctl -n hw.pagesize)) / (1024^3)" | bc)
used_ram=$(echo "$total_ram_gb - $free_ram_gb - 0.95" | bc)
ram_string="Memory: ${used_ram} GB / ${total_ram_gb} GB"

# CPU -------------------------------------------------------------------------
cpu_percentage=$(iostat -c 2 | sed -n 4p | awk '{print "CPU: "100 - $16"% / 100%"}')

# Volume ----------------------------------------------------------------------
volume=$(mixer -a | sed -n 9p | awk '{print $3}' | sed 's/:/\n/g' | sed -n 1p | awk '{print $1 * 100}')
is_muted=$(mixer -a | sed -n 9p | grep 'mute')

if [ -z "$is_muted" ]; then
    volume_percentage="Volume: $volume%"
else
    volume_percentage="Muted"
fi

echo $date $separator $date_time $separator $volume_percentage $separator $battery_life $battery_status $separator $ram_string $separator $cpu_percentage '' 
