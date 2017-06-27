setxkbmap fi
pulseaudio &
xautolock -time 15 -locker lockscreen &
exec i3 -V >> ~/i3log-$(date +'%F-%k-%M-%S') 2>&1
