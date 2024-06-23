#!/bin/sh

# For Posix Shells
# You need to source this ( source|. <path/to/script>) for it to take effect of cd

choice=$(find . -type d | grep -E -v '^(\./\.cache|\./\.dbus|\./\.ssh|\./\.local/share|\./\.local/state|\./\.pki)' | cut -c 3- | tofi --font-size 14 --width 60% --height 60% --fuzzy-match false --prompt-text "Project > " | awk -v H="$HOME" '{print H"/"$1}')

[ -z "$choice" ] && exit 0

echo $choice
cd $choice

