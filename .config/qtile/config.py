# Gatonegro (https://gatoneg.ro/)
# Custom config.py for Qtile window manager. 
#
# Copyright (c) 2010 Aldo Cortesi, 2010, 2014 dequis, 2012 Randall Ma,
#  2012-2014 Tycho Andersen, 2012 Craig Barnes, 2013 horsik, 2013 Tao Sauvage
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

import os
import re
import socket
import subprocess
from libqtile.config import Drag, Key, Screen, Group, Drag, Click, Rule, Match
from libqtile.command import lazy
from libqtile import layout, bar, widget, hook
from libqtile.widget import Spacer
#import arcobattery

#mod4 or mod = super key
mod = "mod4"
mod1 = "alt"
mod2 = "control"
home = os.path.expanduser('~')


@lazy.function
def window_to_prev_group(qtile):
    if qtile.currentWindow is not None:
        i = qtile.groups.index(qtile.currentGroup)
        qtile.currentWindow.togroup(qtile.groups[i - 1].name)

@lazy.function
def window_to_next_group(qtile):
    if qtile.currentWindow is not None:
        i = qtile.groups.index(qtile.currentGroup)
        qtile.currentWindow.togroup(qtile.groups[i + 1].name)

keys = [

# Most of our keybindings are in sxhkd file - except these

# SUPER + FUNCTION KEYS

    Key([mod], "F11", lazy.window.toggle_fullscreen()),
#    Key([mod], "q", lazy.window.kill()),
#    Key([mod], "w", lazy.to_screen(0)),
#    Key([mod], "e", lazy.to_screen(1)),

# SUPER + SHIFT KEYS

    Key([mod, "shift"], "q", lazy.window.kill()),
    Key([mod, "shift"], "r", lazy.restart()),
    Key([mod, "shift"], "w", lazy.to_screen(0)),
    Key([mod, "shift"], "e", lazy.to_screen(1)),

# QTILE LAYOUT KEYS
    Key([mod], "n", lazy.layout.normalize()),
    Key([mod], "space", lazy.next_layout()),

# CHANGE FOCUS
    Key([mod], "Up", lazy.layout.up()),
    Key([mod], "Down", lazy.layout.down()),
    Key([mod], "Left", lazy.layout.left()),
    Key([mod], "Right", lazy.layout.right()),
    Key([mod], "k", lazy.layout.up()),
    Key([mod], "j", lazy.layout.down()),
    Key([mod], "h", lazy.layout.left()),
    Key([mod], "l", lazy.layout.right()),


# RESIZE UP, DOWN, LEFT, RIGHT
    Key([mod, "control"], "l",
        lazy.layout.grow_right(),
        lazy.layout.grow(),
        lazy.layout.increase_ratio(),
        lazy.layout.delete(),
        ),
    Key([mod, "control"], "Right",
        lazy.layout.grow_right(),
        lazy.layout.grow(),
        lazy.layout.increase_ratio(),
        lazy.layout.delete(),
        ),
    Key([mod, "control"], "h",
        lazy.layout.grow_left(),
        lazy.layout.shrink(),
        lazy.layout.decrease_ratio(),
        lazy.layout.add(),
        ),
    Key([mod, "control"], "Left",
        lazy.layout.grow_left(),
        lazy.layout.shrink(),
        lazy.layout.decrease_ratio(),
        lazy.layout.add(),
        ),
    Key([mod, "control"], "k",
        lazy.layout.grow_up(),
        lazy.layout.grow(),
        lazy.layout.decrease_nmaster(),
        ),
    Key([mod, "control"], "Up",
        lazy.layout.grow_up(),
        lazy.layout.grow(),
        lazy.layout.decrease_nmaster(),
        ),
    Key([mod, "control"], "j",
        lazy.layout.grow_down(),
        lazy.layout.shrink(),
        lazy.layout.increase_nmaster(),
        ),
    Key([mod, "control"], "Down",
        lazy.layout.grow_down(),
        lazy.layout.shrink(),
        lazy.layout.increase_nmaster(),
        ),


# FLIP LAYOUT FOR MONADTALL/MONADWIDE
    Key([mod, "shift"], "f", lazy.layout.flip()),

# FLIP LAYOUT FOR BSP
    Key([mod, "mod1"], "k", lazy.layout.flip_up()),
    Key([mod, "mod1"], "j", lazy.layout.flip_down()),
    Key([mod, "mod1"], "l", lazy.layout.flip_right()),
    Key([mod, "mod1"], "h", lazy.layout.flip_left()),

# MOVE WINDOWS UP OR DOWN BSP LAYOUT
    Key([mod, "shift"], "k", lazy.layout.shuffle_up()),
    Key([mod, "shift"], "j", lazy.layout.shuffle_down()),
    Key([mod, "shift"], "h", lazy.layout.shuffle_left()),
    Key([mod, "shift"], "l", lazy.layout.shuffle_right()),

# MOVE WINDOWS UP OR DOWN MONADTALL/MONADWIDE LAYOUT
    Key([mod, "shift"], "Up", lazy.layout.shuffle_up()),
    Key([mod, "shift"], "Down", lazy.layout.shuffle_down()),
    Key([mod, "shift"], "Left", lazy.layout.swap_left()),
    Key([mod, "shift"], "Right", lazy.layout.swap_right()),

# TOGGLE FLOATING LAYOUT
    Key([mod, "shift"], "space", lazy.window.toggle_floating()),

    ]

groups = [
    Group("1", label="WEB", layout="mx", matches=[Match(wm_class=["firefox"])]),
    Group("2", label="SYS", layout="mt", matches=[Match(wm_class=["thunar"])]),
    Group("3", label="MEDIA", layout="mt"),
    Group("4", label="CHAT", layout="mt", matches=[Match(wm_class=["discord", "whatsapp-nativefier-d40211"])]),
    Group("5", label="TEXT", layout="mt", matches=[Match(wm_class=["subl", "obsidian"])]),
    Group("6", label="GAMES", layout="mt", matches=[Match(wm_class=["Steam"])]),
    Group("7", label="GFX", layout="mt", matches=[Match(wm_class=["gimp","gimp-2.10"])]),
    Group("8", label="AV", layout="mt"),
    Group("9", label="VBOX", layout="fl", matches=[Match(wm_class=["VirtualBox Manager", "VirtualBox Machine"])]),
]

for i in groups:
    keys.extend([

#CHANGE WORKSPACES
        Key([mod], i.name, lazy.group[i.name].toscreen()),
        Key([mod], "Tab", lazy.screen.next_group()),
        Key([mod, "shift" ], "Tab", lazy.screen.prev_group()),
        Key(["mod1"], "Tab", lazy.screen.next_group()),
        Key(["mod1", "shift"], "Tab", lazy.screen.prev_group()),

# MOVE WINDOW TO SELECTED WORKSPACE 1-10 AND STAY ON WORKSPACE
        #Key([mod, "shift"], i.name, lazy.window.togroup(i.name)),
# MOVE WINDOW TO SELECTED WORKSPACE 1-10 AND FOLLOW MOVED WINDOW TO WORKSPACE
        Key([mod, "shift"], i.name, lazy.window.togroup(i.name) , lazy.group[i.name].toscreen()),
    ])


def init_layout_theme():
    return {"margin":3,
            "border_width":1,
            "border_focus": "#ff00aa",
            "border_normal": "#444444"
            }

layout_theme = init_layout_theme()


layouts = [
    layout.MonadTall(name="mt", **layout_theme),
	layout.RatioTile(name="rt", **layout_theme),
    layout.Floating(name="fl", **layout_theme),
    layout.Max(name="mx", **layout_theme)
]

# COLORS FOR THE BAR

def init_colors():
    return [["#ff00aa", "#ff00aa"], # color 0 accent
            ["#800055", "#800055"], # color 1 accent dark
            ["#ff80d4", "#ff80d4"], # color 2 accent light
            ["#111111", "#111111"], # color 3 bg
            ["#ffffff", "#ffffff"], # color 4 lightest
            ["#cdcdcd", "#cdcdcd"], # color 5 light
            ["#888888", "#888888"], # color 6 darker
            ["#444444", "#444444"], # color 7 dark
            ["#222222", "#222222"]] # color 8 darkest

colors = init_colors()


# WIDGETS FOR THE BAR

def init_widgets_defaults():
    return dict(font="Ubuntu",
                fontsize = 11,
                padding = 0,
                background=colors[3])

widget_defaults = init_widgets_defaults()

def init_widgets_list():
    prompt = "{0}@{1}: ".format(os.environ["USER"], socket.gethostname())
    widgets_list = [
               widget.Image(
                        filename = "~/.config/qtile/icons/gato.png",
                        background = colors[3],
                        margin = 3,
                        scale = True
                        ),
               widget.GroupBox(
			            font="Ubuntu Bold",
                        fontsize = 9,
                        padding_y = 2,
                        padding_x = 2,
                        borderwidth = 1,
                        disable_drag = True,
                        active = colors[4],
                        inactive = colors[6],
                        rounded = True,
                        highlight_method = "block",
                        block_highlight_text_color = colors[8],
						this_current_screen_border = colors[0],
                        this_screen_border = colors[0],
                        other_current_screen_border = colors[1],
                        other_screen_border = colors[1],
						urgent_alert_method = "block",
						urgent_border = colors[2],
						urgent_text = colors[4],
                        foreground = colors[4],
                        background = colors[3]
                        ),
               widget.Sep(
                        linewidth = 0,
                        padding = 10,
                        background = colors[3]
                        ),
               widget.WindowName(
			            font="Ubuntu",
                        fontsize = 11,
                        foreground = colors[5],
                        background = colors[3]
                        ),
               widget.Sep(
                        linewidth = 0,
                        padding = 10,
                        background = colors[3]
                        ),
		       widget.Cmus(
			  			background = colors[3],
						foreground = colors[5],
						font = "Ubuntu",
						fontsize = 10,
						noplay_color = colors[6],
						padding = 0,
						update_interval = 1
			            ),
               widget.Sep(
                        linewidth = 0,
                        background = colors[3],
                        padding = 5
                        ),
               widget.Systray(
                        background = colors[3],
                        icon_size = 16,
                        padding = 5
                        ),
               widget.Sep(
                        linewidth = 0,
                        background = colors[3],
                        padding = 10
                        ),
#               widget.CurrentLayoutIcon(
#                        foreground = colors[5],
#                        background = colors[3],
#						scale = 0.4,
#						custom_icons_path = "~/.config/qtile/icons",
#                        ),
               widget.CurrentLayout(
                        font = "Ubuntu",
                        fontsize = 9,
						padding = 0,
                        foreground = colors[5],
                        background = colors[3]
                        ),
               widget.Sep(
                        linewidth = 0,
                        background = colors[3],
                        padding = 5
                        ),
               widget.Clock(
                        foreground = colors[5],
                        background = colors[3],
                        font="Ubuntu",
                        fontsize = 10,
                        padding = 5,
                        format="%a %d %b  %H:%M"
                        ),
			   widget.Sep(
			            background = colors[3],
						linewidth = 0,
						padding = 2
						)]
    return widgets_list

widgets_list = init_widgets_list()

def init_widgets_screen1():
    widgets_screen1 = init_widgets_list()
    return widgets_screen1

def init_widgets_screen2():
    widgets_screen2 = init_widgets_list()
    del widgets_screen2[5:8] 
    return widgets_screen2

widgets_screen1 = init_widgets_screen1()
widgets_screen2 = init_widgets_screen2()


def init_screens():
    return [Screen(top=bar.Bar(widgets=init_widgets_screen1(), size=24, opacity=0.8)),
            Screen(top=bar.Bar(widgets=init_widgets_screen2(), size=24, opacity=0.8))]
screens = init_screens()


# MOUSE CONFIGURATION
mouse = [
    Drag([mod], "Button1", lazy.window.set_position_floating(),
         start=lazy.window.get_position()),
    Drag([mod], "Button3", lazy.window.set_size_floating(),
         start=lazy.window.get_size())
]

dgroups_key_binder = None
dgroups_app_rules = []

main = None

@hook.subscribe.startup_once
def start_once():
    home = os.path.expanduser('~')
    subprocess.call([home + '/.config/qtile/scripts/autostart.sh'])

@hook.subscribe.startup
def start_always():
    # Set the cursor to something sane in X
    subprocess.Popen(['xsetroot', '-cursor_name', 'left_ptr'])

@hook.subscribe.client_new
def set_floating(window):
    if (window.window.get_wm_transient_for()
            or window.window.get_wm_type() in floating_types):
        window.floating = True

floating_types = ["notification", "toolbar", "splash", "dialog"]


follow_mouse_focus = True
bring_front_click = False
cursor_warp = False
floating_layout = layout.Floating(float_rules=[
    {'wmclass': 'Arcolinux-welcome-app.py'},
    {'wmclass': 'Arcolinux-tweak-tool.py'},
    {'wmclass': 'Arcolinux-calamares-tool.py'},
    {'wmclass': 'confirm'},
    {'wmclass': 'dialog'},
    {'wmclass': 'download'},
    {'wmclass': 'error'},
    {'wmclass': 'file_progress'},
    {'wmclass': 'notification'},
    {'wmclass': 'splash'},
    {'wmclass': 'toolbar'},
    {'wmclass': 'confirmreset'},
    {'wmclass': 'makebranch'},
    {'wmclass': 'maketag'},
    {'wmclass': 'Arandr'},
    {'wmclass': 'feh'},
    {'wmclass': 'Galculator'},
    {'wmclass': 'arcolinux-logout'},
    {'wmclass': 'xfce4-terminal'},
    {'wname': 'branchdialog'},
    {'wname': 'Open File'},
    {'wname': 'pinentry'},
    {'wmclass': 'ssh-askpass'},
    {'wmname': 'Picture-in-Picture'},
    {'wmclass': 'Steam'},
    {'wmclass': 'VirtualBox Manager'},
    {'wmclass': 'VirtualBox Machine'},

],  fullscreen_border_width = 0, border_width = 0)
auto_fullscreen = True

focus_on_window_activation = "focus" # or smart

wmname = "LG3D"

