#
#            /o   ooooo          
#         oooooo oooooooo+       
#      /.  o ooo oooo ooooo\     
#    oo    /oooo ooo    \           QTILE WINDOW MANAGER
#  .oo     ( ooo ooo+oooooo         config.py
#  ooo     ooooo&ooo   oooooo       ....................
#  oooo    &oooooooo     oooo       Gatonegro
#   ooooo, / (   oooo.    /oo       https://gatoneg.ro/
#     ooooooo    o        oo     
#       ooooooooooo&//ooo(       
#          ooooooooooo/         
#
# Adapted from the default ArcoLinux config by Erik Dubois, with bits and bobs
# borrowed from Derek Taylor (DistroTube on YouTube).
#
# Copyright (c) 2010 Aldo Cortesi, 2010, 2014 dequis, (c) 2012 Randall Ma,
# 2012-2014 Tycho Andersen, 2012 Craig Barnes, 2013 horsik, 2013 Tao Sauvage
#
# Permission is hereby granted, free of charge, to any person obtaining a copy
# of this software and associated documentation files (the "Software"), to deal
# in the Software without restriction, including without limitation the rights
# to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
# copies of the Software, and to permit persons to whom the Software is
# furnished to do so, subject to the following conditions:
#
# The above copyright notice and this permission notice shall be included in
# all copies or substantial portions of the Software.

### IMPORTS ###
import os
import re
import socket
import subprocess
from libqtile import qtile
from libqtile.config import Click, Drag, Group, Key, KeyChord, Match, Screen
from libqtile.command import lazy
from libqtile import layout, bar, widget, hook
from libqtile.lazy import lazy
from typing import List  # noqa: F401

### VARIABLES ###
mod = "mod4"		# Set mod key to Super
myTerm = "kitty"	# Set kitty as default terminal, no need to guess

### KEYBINDINGS ###

	# THE BASICS
	# ----------
	# mod+Return			Launch terminal
	# mod+shift+Return		Run application launcher
	# mod+shift+c			Close the focused window
	# mod+Escape			Launch xkill
	# mod+control+r			Restart Qtile
	# mod+control+q			Shutdown Qtile
	#
	# LAYOUTS
	# -------
	# mod+Tab				Cycle through available layouts
	# mod+j					Lazy layout down (switch focus down)
	# mod+k					Lazy layout up (switch focus up)
	# mod+shift+j			Lazy layout shuffle down (move windows down in stack)
	# mod+shift+k			Lazy layout shuffle up ((move windows up in stack)
	# mod+shift+h			Shrink window size in MonadTall
	# mod+shift+l			Expand window size in MonadTall
	# mod+shift+m			Toggle between minimum and maximum window sizes
	# mod+shift+n			Normalise windo size ratios
	# mod+shift+f			Toggle floating
	# mod+shift+F11			Toggle fullscreen
	# mod+shift+Tab			Flip MonadTall layout
	# mod+space				Switch focus to other panes in stack
	# mod+shift+space		Toggle between split and unsplit sides of stack
	#
	# MULTI-MONITOR
	# -------------
	# mod+shift+w			Switch focus to monitor 1
	# mod+shift+e			Switch focus to monitor 2
	# mod+shift+period		Switch focus to next monitor
	# mod+shift+comma		Switch focus to previous monitor
	#
	# GROUPS
	# ----------
	# mod+(1-9)				Switch to group (1-9)
	# mod+shift+(1-9)		Send focused window to group (1-9)
	#

keys = [
	### THE BASICS ###
	Key([mod], "Return",
		lazy.spawn(myTerm+" -e"),
		desc = "Launch terminal"
		),
	Key([mod, "shift"], "Return",
		lazy.spawn("rofi -show drun"),
		desc = "Run application launcher"
		),
	Key([mod, "shift"], "c",
		lazy.window.kill(),
		desc = "Close the focused window"
		),
	Key([mod], "Escape",
		lazy.spawn("xkill"),
		desc = "Launch xkill"
		),
	Key([mod, "control"], "r",
		lazy.restart(),
		desc = "Restart Qtile"
		),
	Key([mod, "control"], "q",
		lazy.shutdown(),
		desc = "Shutdown Qtile"
		),
	### LAYOUTS ###
	Key([mod], "Tab",
		lazy.next_layout(),
		desc = "Cycle through available layouts"
		),
	Key([mod], "j",
		lazy.layout.down(),
		desc = "Switch focus down"
		),
	Key([mod], "k",
		lazy.layout.up(),
		desc = "Switch focus up"
		),
	Key([mod, "shift"], "j",
		lazy.layout.shuffle_down(),
		lazy.layout.section_down(),
		desc = "Move window down in stack"
		),
	Key([mod, "shift"], "k",
		lazy.layout.shuffle_up(),
		lazy.layout.section_up(),
		desc = "Move window up in stack"
		),
	Key([mod, "shift"], "h",
		lazy.layout.shrink(),
		lazy.layout.decrease_nmaster(),
		desc = "Shrink window (MonadTall), decrease number in master pane (Tile)"
		),
	Key([mod, "shift"], "l",
		lazy.layout.grow(),
		lazy.layout.increase_nmaster(),
		desc = "Expand window (MonadTall), increase number in master pane (Tile)"
		),
	Key([mod, "shift"], "m",
		lazy.layout.maximize(),
		desc = "Toggle between minimum and maximum window sizes"
		),
	Key([mod, "shift"], "n",
		lazy.layout.normalize(),
		desc = "Normalise window size ratios"
		),
	Key([mod, "shift"], "f",
		lazy.window.toggle_floating(),
		desc = "Toggle floating"
		),
	Key([mod, "shift"], "F11",
		lazy.window.toggle_fullscreen(),
		desc = "Toggle fullscreen"
		),
	Key([mod, "shift"], "Tab",
		lazy.layout.rotate(),
		lazy.layout.flip(),
		desc = "Flip master pane side (MonadTall)"
		),
	Key([mod], "space",
		lazy.layout.next(),
		desc = "Switch focus to other panes in stack"
		),
	Key([mod, "shift"], "space",
		lazy.layout.toggle_split(),
		desc = "Toggle between split and unsplit sides of stack"
		),
	### MULTI-MONITOR ###
	Key([mod, "shift"], "w",
		lazy.to_screen(1),
		desc = "Switch focus to monitor 1"
		),
	Key([mod, "shift"], "e",
		lazy.to_screen(2),
		desc = "Switch focus to monitor 2"
		),
	Key([mod, "shift"], "period",
		lazy.next_screen(),
		desc = "Switch focus to next monitor"
		),
	Key([mod, "shift"], "comma",
		lazy.prev_screen(),
		desc = "Switch focus to previous monitor"
		)
]

### GROUPS ###
groups = [
	Group("1", label=" ", layout="monadtall", matches=[Match(wm_class=["firefox", "mailspring"])]),	# Web
	Group("2", label=" ", layout="monadtall", matches=[Match(wm_class=["thunar", "pcmanfm", "transmission-gtk"])]),	# System
	Group("3", label=" ", layout="monadtall"),	# Music
	Group("4", label=" ", layout="monadtall", matches=[Match(wm_class=["discord", "whatsapp-nativefier-d40211"])]),	# Chat
	Group("5", label=" ", layout="monadtall", matches=[Match(wm_class=["subl", "obsidian"])]),	# Text
	Group("6", label=" ", layout="monadtall", matches=[Match(wm_class=["Steam"])]),	# Gaming
	Group("7", label=" ", layout="monadtall", matches=[Match(wm_class=["gimp","gimp-2.10"])]),	# Graphics
	Group("8", label=" ", layout="monadtall"),	# Audio/video
	Group("9", label=" ", layout="floating", matches=[Match(wm_class=["VirtualBox Manager", "VirtualBox Machine"])]),	# VirtualBox
]

### KEYBINDINGS - GROUPS ###
for i in groups:
    keys.extend([
        Key([mod], i.name,
			lazy.group[i.name].toscreen(),
            desc="Switch to group {}".format(i.name)
			),
        Key([mod, "shift"], i.name,
			lazy.window.togroup(i.name),
			desc="Send focused window to group {}".format(i.name)
			),
    ])

### DEFAULT LAYOUT SETTINGS ###
layout_theme = {"border_width" : 2,
				"margin" : 8,
				"border_focus" : "ff00aa",
				"border_normal" : "666666",
			   }

### LAYOUTS ###
layouts = [
    layout.MonadTall(**layout_theme),
    layout.Max(**layout_theme),
    layout.Tile(**layout_theme),
    layout.Stack(num_stacks=2,**layout_theme),
	layout.Zoomy(**layout_theme),
	layout.Floating(**layout_theme),
    # layout.Columns(border_focus_stack='#d75f5f'),
    # layout.Bsp(),
    # layout.Matrix(),
    # layout.MonadWide(),
    # layout.RatioTile(),
    # layout.TreeTab(),
    # layout.VerticalTile(),
]

### COLOUR PALETTE ###
colors = [["#222222", "#222222"], # 0 Panel background
		  ["#ffffff", "#ffffff"], # 1 Active group foreground
		  ["#ff00aa", "#b32d86"], # 2 Active group background
		  ["#888888", "#888888"], # 3 Inactive group foreground
		  ["#7f205f", "#7f205f"], # 4 Other groups background
		  ["#cdcdcd", "#cdcdcd"], # 5 Generic text foreground
		  ["#66194c", "#66194c"], # 6 Coloured widget background
]

### WIDGET DEFAULTS ###
widget_defaults = dict(
    font = "UbuntuMono Nerd Font",
    fontsize = 12,
    padding = 4,
	background = colors[0],
	foreground = colors[5]
)
extension_defaults = widget_defaults.copy()

### WIDGETS - DEFINE ###
def init_widgets_list():
	widgets_list = [
		# 0
		widget.Sep(
			linewidth = 0,
			padding = 4
		),
		# 1
		widget.Image(
			filename = "~/.config/qtile/icons/gato.png",
			margin = 3,
			scale = True
			),
		# 2
		widget.GroupBox(
			fontsize = 14,
			borderwidth = 2,
			active = colors[1],
			inactive = colors[3],
			highlight_method = "block",
			block_highlight_text_color = colors[1],
			this_current_screen_border = colors[2],
			this_screen_border = colors[2],
			other_current_screen_border = colors[4],
			other_screen_border = colors[4],
			urgent_border = colors[6],
			urgent_text = colors[1],
			),
		# 3
		widget.WindowName(
			),
		# 4
		widget.Cmus(
			update_interval = 1
			),
		# 5
		widget.Systray(
			),
		# 6
		widget.Net(
		  # interface = "enp0s3", # VBox adapter
			interface = "eno1",
			format = "{down}  {up}  "
			),
		# 7
		widget.CheckUpdates(
			update_interval = 7200,
			distro = "Arch_checkupdates",
			display_format = "  {updates}",
			mouse_callbacks = {'Button1': lambda: qtile.cmd_spawn(myTerm + ' -e sudo pacman -Syu')}
			),
		# 8
		widget.Volume(
			fmt = "  {}"
			),
		# 9
		widget.CurrentLayoutIcon(
            scale = 0.4,
            custom_icon_paths = [os.path.expanduser("~/.config/qtile/icons")],
			background = colors[6]
            ),
		# 10
		widget.CurrentLayout(
			background = colors[6]
			),
		# 11
		widget.Clock(
			format = "%a %d %H:%M"
			),
		widget.Sep(
			linewidth = 0,
			padding = 2
			),
	]
	return widgets_list

widgets_list = init_widgets_list()

### WIDGETS - BUILD WIDGET LISTS FOR EACH SCREEN ###
def init_widgets_screen1():
	widgets_screen1 = init_widgets_list()
	return widgets_screen1

def init_widgets_screen2():
	widgets_screen2 = init_widgets_list()
	del widgets_screen2[4:8] # Slice the widgets we don't want on screen 2
	return widgets_screen2

widgets_screen1 = init_widgets_screen1()
widgets_screen2 = init_widgets_screen2()

### INITIALISE SCREENS ###
def init_screens():
	return [Screen(top=bar.Bar(widgets=init_widgets_screen1(), size=25, opacity=0.85)),
			Screen(top=bar.Bar(widgets=init_widgets_screen2(), size=25, opacity=0.85))]
screens = init_screens()

### DRAG FLOATING WINDOWS ###
mouse = [
    Drag([mod], "Button1", lazy.window.set_position_floating(),
         start=lazy.window.get_position()),
    Drag([mod], "Button3", lazy.window.set_size_floating(),
         start=lazy.window.get_size()),
    Click([mod], "Button2", lazy.window.bring_to_front())
]

dgroups_key_binder = None
dgroups_app_rules = []

### AUTOSTART SCRIPT ###
@hook.subscribe.startup_once
def start_once():
	home = os.path.expanduser('~')
	subprocess.call([home + '/.config/qtile/scripts/autostart.sh'])

### SET FLOATING WINDOWS AUTOMATICALLY ###
@hook.subscribe.client_new
def set_floating(window):
	if (window.window.get_wm_transient_for()
		or window.window.get_wm_type() in floating_types):
	   window.floating = True

floating_types = ["notification", "toolbar", "splash", "dialog"]

follow_mouse_focus = True
bring_front_click = False
cursor_warp = False

### FLOATING LAYOUT RULES ###
floating_layout = layout.Floating(float_rules=[
    {'wmclass': 'confirmreset'},
    {'wmclass': 'makebranch'},
    {'wmclass': 'maketag'},
    {'wmclass': 'ssh-askpass'},
    {'wmname': 'branchdialog'},
    {'wmname': 'pinentry'},
    {'wmclass': 'Arcolinux-welcome-app.py'},
    {'wmclass': 'Arcolinux-tweak-tool.py'},
    {'wmclass': 'Arcolinux-calamares-tool.py'},
    {'wmclass': 'arcolinux-logout'},
    {'wmclass': 'confirm'},
    {'wmclass': 'dialog'},
    {'wmclass': 'download'},
    {'wmclass': 'error'},
    {'wmclass': 'file_progress'},
    {'wmclass': 'notification'},
    {'wmclass': 'splash'},
    {'wmclass': 'toolbar'},
    {'wmclass': 'Arandr'},
    {'wmclass': 'feh'},
    {'wmclass': 'Galculator'},
    {'wmclass': 'xfce4-terminal'},
    {'wname': 'Open File'},
    {'wmname': 'Picture-in-Picture'},
    {'wmclass': 'Steam'},
    {'wmclass': 'VirtualBox Manager'},
#   {'wmclass': 'VirtualBox Machine'},
    {'wmclass': 'transmission-gtk'},
    {'wmclass': 'gimp-2.10'},
], fullscreen_border_width = 0, border_width = 0)

auto_fullscreen = True
focus_on_window_activation = "smart"
reconfigure_screens = True
auto_minimize = True

wmname = "LG3D"


