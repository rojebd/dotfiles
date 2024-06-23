#!/bin/sh

sysctl hw.acpi.battery | grep hw.acpi.battery.life | grep -o -E ': .*' | sed 's|[: ,]||g' | rev | awk '{print $1"% :yrettaB"}' | rev
