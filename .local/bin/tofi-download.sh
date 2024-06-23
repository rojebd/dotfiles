#!/bin/sh

url=$1
choice=$(echo -e "Video\nAudio" | tofi --height 150 --width 190)

[ -z "$choice" ] && exit 0

if [ "$choice" = "Video" ]; then
    # yt-dlp -N 9 --throttled-rate 100K --embed-metadata --merge-output-format mkv --downloader aria2c -o - $url
    foot -e yt-dlp -N 9 --embed-metadata --merge-output-format mkv --downloader aria2c $url
else
    # Download Audio Only
    # yt-dlp -x --audio-format wav -N 9 --throttled-rate 100K --downloader aria2c -o - $url
    foot -e yt-dlp -x --audio-format wav -N 9 --downloader aria2c $url
fi
