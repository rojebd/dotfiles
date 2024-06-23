#!/bin/sh


url=$1
choice=$(echo -e "Video\nAudio" | tofi --height 150 --width 190)

[ -z "$choice" ] && exit 0

if [ "$choice" = "Video" ]; then
    #yt-dlp -N 9 --throttled-rate 100K --downloader aria2c -o - $url | mpv --geometry=1050x680 --no-resume-playback --wayland-app-id=MPV_FLOATING -
    mpv --geometry=1050x680 --no-resume-playback --wayland-app-id=MPV_FLOATING $url
else
    #yt-dlp -N 9 --throttled-rate 100K --downloader aria2c -o - $url | mpv --loop --geometry=350x230 --no-resume-playback --wayland-app-id=MPV_FLOATING -
    mpv --loop --geometry=350x230 --no-resume-playback --wayland-app-id=MPV_FLOATING $url
fi
