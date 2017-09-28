setxkbmap fi
pulseaudio &
xautolock -time 15 -locker lockscreen &
exec i3 -V >> ~/logs/i3log-$(date +"%Y-%m-%d-%H-%M-%S") 2>&1
feh --bg-scale ~/wallpapers/wkEK1lc.jpg
