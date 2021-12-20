#!/bin/bash
# ⠀⠀⠀⠀⠀⠀⠀⠀⢀⡀⠀⣠⣄⠀⠀⠀⠀⠀⠀
#⠀⠀⠀⠀⠀⠀⡠⠖⣿⣧⢻⣿⢿⣷⣤⡀⠄⠀⠀⠀autostart.sh
#⠀⠀⠀⠀⣠⠊⠀⠂⣿⡏⣾⣿⠈⢻⠟⠉⠀⠀⠀⠀------------
#⠀⠀⠀⢸⣿⠀⠀⢰⣿⣷⢻⣿⠴⣿⣷⣦⡀⠀⠀⠀Autostart script for use with Qtile.
#⠀⠀⠀⣿⣿⡄⠀⡇⣿⣧⣿⣿⠀⠈⢿⣿⡇⠀⠀⠀
#⠀⠀⠀⠈⢿⣿⣦⣱⠃⠀⣿⠟⠁⠀⠀⡿⠃⠀⠀⠀
#⠀⠀⠀⠀⠀⠙⢿⣿⣿⣶⣧⣤⣤⡤⠚⠁⠀⠀⠀⠀
#⠀⠀⠀⠀⠀⠀⠀⠌⠉⠛⠛⠛⠉⠀⠀⠀⠀⠀⠀⠀
# -----------------------------------------------------------------------------

lxsession &
picom -b --experimental-backends &
nm-applet &
numlockx on &
dunst &
sxhkd -c ~/.config/sxhkd/sxhkdrc &
redshift &
canberra-gtk-play -f ~/documents/startup.ogg &
echo -n --head=0,--head=1 | \
	xargs -n 1 -d , \
	nitrogen --random --set-zoom-fill --save &

# Foxes
case $HOSTNAME in
	foxes)
		solaar -w hide &
		# mailspring -b &
		;;
esac

