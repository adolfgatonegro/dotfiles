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

# KEYS_START
keys = [
	# KEYS_GROUP Qtile Basics #
	Key([mod], "F1", lazy.spawn([os.path.expanduser("~/.config/qtile/scripts/show-keybindings.sh")]), desc = "Show Qtile keybindings"),
	Key([mod], "Return", lazy.spawn(myTerm+" -e"), desc = "Launch terminal"),
	Key([mod], "r", lazy.spawn("rofi -show drun"), desc = "Run application launcher"),
	Key([mod, "shift"], "c", lazy.window.kill(), desc = "Close the focused window"),
	Key([mod], "Escape", lazy.spawn("xkill"), desc = "Launch xkill"),
	Key([mod, "control"], "r", lazy.restart(), desc = "Restart Qtile"),
	Key([mod, "control"], "q", lazy.shutdown(), desc = "Shutdown Qtile"),
	# KEYS_GROUP Layout control #
	Key([mod], "Tab", lazy.next_layout(), desc = "Switch to next layout"),
	Key([mod, "shift"], "Tab", lazy.prev_layout(), desc = "Switch back to previous layout"),
	Key([mod], "j", lazy.layout.down(),	desc = "Switch focus down"),
	Key([mod], "k", lazy.layout.up(), desc = "Switch focus up"),
	Key([mod, "shift"], "j", lazy.layout.shuffle_down(), lazy.layout.section_down(), desc = "Move window down in stack"),
	Key([mod, "shift"], "k", lazy.layout.shuffle_up(), lazy.layout.section_up(), desc = "Move window up in stack"),
	Key([mod, "shift"], "h", lazy.layout.shrink(), lazy.layout.decrease_nmaster(), desc = "Shrink window (MonadTall), decrease number in master pane (Tile)"),
	Key([mod, "shift"], "l", lazy.layout.grow(), lazy.layout.increase_nmaster(), desc = "Expand window (MonadTall), increase number in master pane (Tile)"),
	Key([mod, "control"], "Tab", lazy.layout.rotate(), lazy.layout.flip(), desc = "Flip master pane side (MonadTall)"),
	Key([mod], "space", lazy.layout.next(), desc = "Switch focus to next pane in stack (Tile)"),
	Key([mod, "shift"], "space", lazy.layout.previous(), lazy.layout.toggle_split(), desc = "Switch focus to previous pane in stack (Tile) Toggle sides of stack"),
	Key([mod, "control"], "m", lazy.layout.maximize(), desc = "Toggle between minimum and maximum window sizes"),
	Key([mod, "control"], "n", lazy.layout.normalize(), desc = "Normalise window size ratios"),
	Key([mod, "control"], "f", lazy.window.toggle_floating(), desc = "Toggle floating"),
	Key([mod, "control"], "F11", lazy.window.toggle_fullscreen(), desc = "Toggle fullscreen"),
	# KEYS_GROUP Multi-monitor #
	Key([mod, "shift"], "w", lazy.to_screen(1), desc = "Switch focus to display 1"),
	Key([mod, "shift"], "e", lazy.to_screen(2), desc = "Switch focus to display 2"),
	Key([mod, "shift"], "period", lazy.next_screen(), desc = "Switch focus to next display"),
	Key([mod, "shift"], "comma", lazy.prev_screen(),desc = "Switch focus to previous display"),
	Key([mod, "control"], "p", lazy.spawn([os.path.expanduser("~/.config/qtile/scripts/display-toggle.sh")]), desc = "Toggle display 2 on/off"),
	Key([mod, "control"], "o", lazy.spawn([os.path.expanduser("~/.config/qtile/scripts/display-rotate.sh")]), desc = "Rotate display 1"),
]
# KEYS_END

### GROUPS ###
#          - Group icons
groups = [
	Group("1", label="1", layout="monadtall", matches=[Match(wm_class=["firefox"])]),
	Group("2", label="2", layout="monadtall", matches=[Match(wm_class=["Thunar", "pcmanfm", "transmission-gtk"])]),
	Group("3", label="3", layout="monadtall"),
	Group("4", label="4", layout="monadtall", matches=[Match(wm_class=["discord", "mailspring", "whatsapp-nativefier-d40211"])]),
	Group("5", label="5", layout="monadtall", matches=[Match(wm_class=["subl", "DesktopEditors"])]),
	Group("6", label="6", layout="monadtall", matches=[Match(wm_class=["Steam"])]),
	Group("7", label="7", layout="monadtall", matches=[Match(wm_class=["gimp","gimp-2.10","org.inkscape.Inkscape"])]),
	Group("8", label="8", layout="monadtall"),
	Group("9", label="9", layout="monadtall", matches=[Match(wm_class=["VirtualBox Manager", "VirtualBox Machine"])]),
]

### KEYBINDINGS - GROUPS ###
for i in groups:
    keys.extend([
        Key([mod], i.name,
			lazy.group[i.name].toscreen(),
            desc="Switch to group {}".format(i.name)
			),
        Key([mod, "shift"], i.name, lazy.window.togroup(i.name, switch_group=True),
            desc="Send focused window to group {} and switch".format(i.name)),
        Key([mod, "control"], i.name,
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
    layout.RatioTile(ratio=1.5,**layout_theme),
    layout.Stack(num_stacks=2,**layout_theme),
]

### COLOUR PALETTE ###
colors = [["#0d0d0d", "#111111"], # 0 Panel background
		  ["#ffffff", "#ffffff"], # 1 Selected group foreground
		  ["#e60099", "#ff00aa"], # 2 Selected group background
		  ["#666666", "#666666"], # 3 Inactive group foreground
		  ["#4d0033", "#660044"], # 4 Other groups background
		  ["#cccccc", "#cccccc"], # 5 Generic text foreground
		  ["#4d0033", "#330022"], # 6 Coloured widget background
		  ["#00ffff", "#00ffff"], # 7 Coloured widget foreground
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
			scale = True,
            mouse_callbacks = {'Button1': lambda:
                qtile.cmd_spawn([os.path.expanduser("~/.config/qtile/scripts/random-wallpaper.sh")])}
			),
		# 2
		widget.GroupBox(
			disable_drag = True,
			rounded = False,
			#fontsize = 15,
			#borderwidth = 0,
			#padding_x = 7,
			#padding_y = 4,
			#margin_x = 4,
			#margin_y = 3,
			#active = colors[5],
			#inactive = colors[3],
			#highlight_method = "block",
			#highlight_color = colors[1],
			#block_highlight_text_color = colors[1],
			#this_current_screen_border = colors[2],
			#this_screen_border = colors[2],
			#other_current_screen_border = colors[4],
			#other_screen_border = colors[4],
			#urgent_border = colors[6],
			#urgent_text = colors[1],
            padding_x = 0,
            highlight_method = "text",
            this_current_screen_border = colors[2],
            other_current_screen_boder = colors [4],
            ),
		# 3
		widget.Sep(
			linewidth = 0,
			padding = 6
			),
		# 4
		widget.WindowName(
			),
		# 5
		widget.Sep(
			linewidth = 0,
			padding = 6
			),
		# 6
		widget.Cmus(
			update_interval = 1,
			play_color = colors[7]
			),
		# 7
		widget.Systray(
			),
		# 8
		widget.Net(
			interface = "eno1",
			format = "  {down}  {up}"
			),
		# 9
		widget.CheckUpdates(
			update_interval = 3600,
			distro = "Arch_checkupdates",
			display_format = "  {updates}",
			mouse_callbacks = {'Button1': lambda: qtile.cmd_spawn(myTerm + ' -e sudo pacman -Syyu')}
			),
		# 10
		widget.Volume(
			fmt = "  {}",
			mouse_callbacks = {'Button1': lambda: qtile.cmd_spawn(myTerm + ' -e pulsemixer')}
			),
		# 11
		widget.Sep(
			linewidth = 0,
			padding = 6
			),
		# 12
		widget.Sep(
			linewidth = 0,
			padding = 4,
			background = colors[6]
			),
		# 13
		widget.CurrentScreen(
            active_color = colors[1],
			active_text = "",
			inactive_text = "",
			inactive_color = colors[0],
			fontsize = 12,
			padding = 6,
			background = colors[6]
            ),
		# 14
		widget.CurrentLayoutIcon(
            scale = 0.4,
			margin = 0,
            custom_icon_paths = [os.path.expanduser("~/.config/qtile/icons")],
			background = colors[6]
            ),
		# 15
		widget.Sep(
			linewidth = 0,
			padding = 6
			),
		# 16
		widget.Clock(
			format = "%a %d %H:%M"
			),
		# 17
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
	del widgets_screen2[6:10] # Slice the widgets we don't want on screen 2
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
def floating_dialogs(window):
    dialog = window.window.get_wm_type() == 'dialog'
    transient = window.window.get_wm_transient_for()
    if dialog or transient:
        window.floating = True

follow_mouse_focus = True
bring_front_click = False
cursor_warp = False

### FLOATING LAYOUT RULES ###
floating_layout = layout.Floating(float_rules=[
    *layout.Floating.default_float_rules,
	Match(func=lambda c: bool(c.is_transient_for())),
    Match(wm_class='confirmreset'),
    Match(wm_class='makebranch'),
    Match(wm_class='maketag'),
    Match(wm_class='ssh-askpass'),
    Match(title='branchdialog'),
    Match(title='pinentry'),
    Match(role='Dialog'),
	Match(role='PictureInPicture'),
	Match(role='GtkFileChooserDialog'),
    Match(wm_class='arcolinux-logout'),
    Match(wm_class='confirm'),
    Match(wm_class='dialog'),
    Match(wm_class='download'),
    Match(wm_class='error'),
    Match(wm_class='file_progress'),
    Match(wm_class='notification'),
    Match(wm_class='splash'),
    Match(wm_class='toolbar'),
    Match(wm_class='feh'),
    Match(wm_class='xfce4-terminal'),
    Match(wm_class='Steam'),
    Match(wm_class='VirtualBox Manager'),
    Match(wm_class='transmission-gtk'),
    Match(wm_class='gimp-2.10'),
    Match(wm_class='org.inkscape.Inkscape'),
    Match(wm_class='Yad'),
    Match(wm_class='nvidia-settings'),
    Match(wm_class='pulseaudio-equalizer-gtk'),
], fullscreen_border_width = 0, border_width = 0)

auto_fullscreen = True
focus_on_window_activation = "smart"
reconfigure_screens = True
auto_minimize = True

wmname = "LG3D"

