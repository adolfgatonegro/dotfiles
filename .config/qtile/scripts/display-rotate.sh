#!/bin/bash
#
#            /o   ooooo
#         oooooo oooooooo+
#      /.  o ooo oooo ooooo\
#    oo    /oooo ooo    \           DISPLAY ROTATE
#  .oo     ( ooo ooo+oooooo         display-rotate.sh
#  ooo     ooooo&ooo   oooooo       ........................
#  oooo    &oooooooo     oooo       Gatonegro
#   ooooo, / (   oooo.    /oo       https://gatoneg.ro/
#     ooooooo    o        oo
#       ooooooooooo&//ooo(
#          ooooooooooo/
#
# Simple script to rotate my main display.

if xrandr -q | grep "HDMI-2" | grep "1920x1200"
then
	echo "Switching display to vertical." && \
	xrandr --output HDMI-2 --rotate left
else
	echo "Switching display to horizontal." && \
	xrandr --output HDMI-2 --rotate normal
fi
