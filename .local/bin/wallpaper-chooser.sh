#!/bin/sh

wallpaper_dir="$HOME/.config/sway/wallpapers/"
wallpaper_choice=$(ls $wallpaper_dir | tofi)

[ -z "$wallpaper_choice" ] && exit 0

wallpaper=$wallpaper_dir$wallpaper_choice

swaymsg output "*" bg $wallpaper fill

