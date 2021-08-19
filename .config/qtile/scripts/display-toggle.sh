#!/bin/bash
#
#            /o   ooooo
#         oooooo oooooooo+
#      /.  o ooo oooo ooooo\
#    oo    /oooo ooo    \           DISPLAY TOGGLE
#  .oo     ( ooo ooo+oooooo         display-toggle.sh
#  ooo     ooooo&ooo   oooooo       ........................
#  oooo    &oooooooo     oooo       Gatonegro
#   ooooo, / (   oooo.    /oo       https://gatoneg.ro/
#     ooooooo    o        oo
#       ooooooooooo&//ooo(
#          ooooooooooo/
#
# A quick script to toggle my second display, which I don't use as frequently.

if xrandr --listactivemonitors | grep "DP-1"
then
	echo "Switching off the second display." && \
	xrandr --output DP-1 --off --output HDMI-2 --mode 1920x1200 --rate 74.93 --pos 0x0 && \
	nitrogen --random --set-zoom-fill --save
else
	echo "Extending desktop to second display." && \
	xrandr --output DP-1 --mode 1920x1080 --rate 60 --pos 960x0 --output HDMI-2 --mode 1920x1200 --rate 74.93 --pos 0x1080 && \
	echo -n --head=0,--head=1 | xargs -n 1 -d , nitrogen --random --set-zoom-fill --save
fi
