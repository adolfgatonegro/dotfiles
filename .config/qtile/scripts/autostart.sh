#!/bin/bash
#
#            /o   ooooo
#         oooooo oooooooo+
#      /.  o ooo oooo ooooo\
#    oo    /oooo ooo    \           AUTOSTART SCRIPT
#  .oo     ( ooo ooo+oooooo         autostart.sh
#  ooo     ooooo&ooo   oooooo       ........................
#  oooo    &oooooooo     oooo       Gatonegro
#   ooooo, / (   oooo.    /oo       https://gatoneg.ro/
#     ooooooo    o        oo
#       ooooooooooo&//ooo(
#          ooooooooooo/
#
#  Just some stuff that I want to autostart. That's what autostart
#  scripts tend to be for, right?

# Set custom keymap
xmodmap ~/.Xmodmap &

# Set keyboard repeat rate to double(ish)
xset r rate 300 50 &

# Start sxhkd for extra keybindings
sxhkd -c ~/.config/qtile/sxhkd/sxhkdrc &

# System applications
nm-applet &
xfce4-power-manager &
numlockx on &
solaar -w hide &
picom &
/usr/lib/polkit-gnome/polkit-gnome-authentication-agent-1 &
/usr/lib/xfce4/notifyd/xfce4-notifyd &

# User applications
echo -n --head=0,--head=1 | xargs -n 1 -d , nitrogen --random --set-zoom-fill --save &
redshift &
mailspring -b &
