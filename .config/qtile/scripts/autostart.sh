#!/usr/bin/env bash
# gatoneg.ro
# start all the basics when logging in

lxsession &
picom -b --experimental-backends &
dunst &
sxhkd -c &
redshift &
nm-applet &
random_wallpaper &
canberra-gtk-play -f $XDG_CONFIG_HOME/qtile/scripts/startup.ogg &

# Foxes
case $HOSTNAME in
	foxes)
		solaar -w hide &
		numlockx on &
		;;
esac
