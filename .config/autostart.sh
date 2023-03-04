#!/bin/sh
# gatoneg.ro
# start all the basics when logging in

# detect the window manager
# id=$(xprop -root -notype _NET_SUPPORTING_WM_CHECK)
# id=${id##* }
# wm=$(xprop -id "$id" -notype -len 25 -f _NET_WM_NAME 8t)
# wm=${wm##*_NET_WM_NAME = \"}
# wm=${wm%%\"*}

lxsession &
picom -b --experimental-backends &
dunst &
# sxhkd -c &
redshift &
# nm-applet &
setwall &
canberra-gtk-play -f $XDG_CONFIG_HOME/startup.ogg &

# Foxes
case $HOSTNAME in
	foxes)
		solaar -w hide &
		numlockx on &
		;;
esac
