#!/bin/sh
# start all the basics when logging in

lxsession &
picom -b --experimental-backends &
dunst &
sxhkd -c &
redshift &
nm-applet &
random_wallpaper &

# Foxes
case $HOSTNAME in
	foxes)
		solaar -w hide &
		numlockx on &
		canberra-gtk-play -f $XDG_CONFIG_HOME/qtile/scripts/startup.ogg &
		;;
esac

