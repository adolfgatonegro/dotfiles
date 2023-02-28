#!/usr/bin/env bash
# gatoneg.ro
# start all the basics when logging in

lxsession &
picom -b --experimental-backends &
dunst &
sxhkd -c &
redshift &
nm-applet &
setwall &
canberra-gtk-play -f $XDG_CONFIG_HOME/startup.ogg &

# Foxes
case $HOSTNAME in
	foxes)
		solaar -w hide &
		numlockx on &
		;;
esac
