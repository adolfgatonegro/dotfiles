#!/bin/bash

function run {
  if ! pgrep $1 ;
  then
    $@&
  fi
}

#start the conky to learn the shortcuts
#(conky -c $HOME/.config/qtile/scripts/system-overview) &

#start sxhkd to replace Qtile native key-bindings
run sxhkd -c ~/.config/qtile/sxhkd/sxhkdrc &

#starting utility applications at boot time
run nm-applet &
#run pamac-tray &
run xfce4-power-manager &
numlockx on &
blueberry-tray &
picom &
/usr/lib/polkit-gnome/polkit-gnome-authentication-agent-1 &
/usr/lib/xfce4/notifyd/xfce4-notifyd &

#starting user applications at boot time
echo -n --head=0,--head=1 | xargs -n 1 -d , nitrogen --random --set-zoom-fill &
#run volumeicon &
run redshift &
run mailspring &
