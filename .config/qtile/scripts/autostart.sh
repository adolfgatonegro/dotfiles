#!/bin/bash
#
# ⠀⠀⠀⠀⠀⠀⠀⠀⢀⡀⠀⣠⣄⠀⠀⠀⠀⠀⠀
#⠀⠀⠀⠀⠀⠀⡠⠖⣿⣧⢻⣿⢿⣷⣤⡀⠄⠀⠀⠀autostart.sh
#⠀⠀⠀⠀⣠⠊⠀⠂⣿⡏⣾⣿⠈⢻⠟⠉⠀⠀⠀⠀------------
#⠀⠀⠀⢸⣿⠀⠀⢰⣿⣷⢻⣿⠴⣿⣷⣦⡀⠀⠀⠀Autostart script for use with Qtile.
#⠀⠀⠀⣿⣿⡄⠀⡇⣿⣧⣿⣿⠀⠈⢿⣿⡇⠀⠀⠀
#⠀⠀⠀⠈⢿⣿⣦⣱⠃⠀⣿⠟⠁⠀⠀⡿⠃⠀⠀⠀Foxes-specific section can be commented out on Hekate.
#⠀⠀⠀⠀⠀⠙⢿⣿⣿⣶⣧⣤⣤⡤⠚⠁⠀⠀⠀⠀
#⠀⠀⠀⠀⠀⠀⠀⠌⠉⠛⠛⠛⠉⠀⠀⠀⠀⠀⠀⠀
#
# -----------------------------------------------------------------------------

# The basics
xmodmap ~/.Xmodmap &				# set custom keymap
xset r rate 300 50 &				# modify keyboard repeat rate to double-ish
lxsession &							# polkit
picom &								# compositor
nm-applet &							# network manager applet
dunst &								# notification daemon
numlockx on &						# set numlock to on
sxhkd -c ~/.config/sxhkd/sxhkdrc &	# start sxhkd
redshift &							# adjust display colour temperature
echo -n --head=0,--head=1 | \		# set random wallpaper (dual monitor)
	xargs -n 1 -d , \
	nitrogen --random --set-zoom-fill --save &

# Foxes
solaar -w hide &					# mouse management
mailspring -b &						# email client
