setxkbmap fi
pulseaudio &
# xautolock -time 15 -locker lockscreen -notify 5 -notifier screenoff &
xautolock -time 15 -locker lockscreen &
feh --bg-scale ~/wallpapers/foggy-wallpaper-13880-14351-hd-wallpapers.jpg
exec i3 -V >> ~/logs/i3log-$(date +"%Y-%m-%d-%H-%M-%S") 2>&1
