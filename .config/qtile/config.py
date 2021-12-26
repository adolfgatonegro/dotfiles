# ⠀⠀⠀⠀⠀⠀⠀⠀⢀⡀⠀⣠⣄⠀⠀⠀⠀⠀⠀
#⠀⠀⠀⠀⠀⠀⡠⠖⣿⣧⢻⣿⢿⣷⣤⡀⠄⠀⠀⠀
#⠀⠀⠀⠀⣠⠊⠀⠂⣿⡏⣾⣿⠈⢻⠟⠉⠀⠀⠀⠀Qtile
#⠀⠀⠀⢸⣿⠀⠀⢰⣿⣷⢻⣿⠴⣿⣷⣦⡀⠀⠀⠀-------------
#⠀⠀⠀⣿⣿⡄⠀⡇⣿⣧⣿⣿⠀⠈⢿⣿⡇⠀⠀⠀Configuration file for the Qtile window manager.
#⠀⠀⠀⠈⢿⣿⣦⣱⠃⠀⣿⠟⠁⠀⠀⡿⠃⠀⠀⠀
#⠀⠀⠀⠀⠀⠙⢿⣿⣿⣶⣧⣤⣤⡤⠚⠁⠀⠀⠀⠀
#⠀⠀⠀⠀⠀⠀⠀⠌⠉⠛⠛⠛⠉⠀⠀⠀⠀⠀⠀⠀

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

# Imports
import os

from socket import gethostname
from subprocess import call

from libqtile import qtile, bar, hook, layout
from libqtile.config import Click, Drag, Group, Key, Match, Screen
from libqtile.lazy import lazy
from libqtile.widget import (Battery, Backlight, CheckUpdates, Clock, Cmus, CurrentLayout,
                            CurrentLayoutIcon, CurrentScreen, GmailChecker, GroupBox, Image, 
                            Net, PulseVolume, Sep, Systray, Volume, WindowName)

# Variables
mod = "mod4"
terminal = "kitty"
host = gethostname()
home_dir = os.path.expanduser('~/')
bin_dir = os.path.expanduser('~/.local/bin/')

net_adapter = ""
if host == "foxes":
    net_adapter = "eno1"
elif host == "hekate":
    net_adapter = "wlp3s0"

widgets_hekate = (
    Backlight(
        fmt = '  {}',
        backlight_name = 'amdgpu_bl0',
        ),
    Battery(
        charge_char = '',
        discharge_char = '',
        empty_char = '',
        full_char = '',
        unknown_char = '',
        format = '{char} {percent:2.0%}',
        show_short_text = False
        )
    )

colour_bg = "#0a0a15"
colour_text = "#a1a1a1"
colour_text_dark = "#35353d"
colour_accent = "#ff00aa"
colour_accent_dark = "#660044"
colour_alt = "#00ffed"

# colour #0a0a15
# colour #202029
# colour #35353d
# colour #4b4b51
# colour #767679
# colour #ff00aa
# colour #EC20A8
# colour #660044
# colour #ff74d2
# colour #aaee00
# colour #A8DF20
# colour #ff8700
# colour #EC8C20
# colour #0077ff
# colour #207FEC
# colour #004687
# colour #0a0047
# colour #bb00ff
# colour #B620EC
# colour #00ffed
# colour #20ECDE
# colour #8b8b8d
# colour #a1a1a1

# KEYS_START
keys = [
    # KEYS_GROUP Qtile Basics #
    Key([mod], "F1", lazy.spawn([home_dir + ".config/qtile/scripts/show-keybindings.sh"]), desc = "Show Qtile keybindings"),
    Key([mod], "Return", lazy.spawn(terminal+" -e"), desc = "Launch terminal"),
    Key([mod], "r", lazy.spawn("rofi -show drun"), desc = "Run application launcher"),
    Key([mod, "shift"], "c", lazy.window.kill(), desc = "Close the focused window"),
    Key([mod], "Escape", lazy.spawn("xkill"), desc = "Launch xkill"),
    Key([mod, "control"], "r", lazy.restart(), desc = "Restart Qtile"),
    Key([mod, "control"], "q", lazy.shutdown(), desc = "Shutdown Qtile"),
    # KEYS_GROUP Layout control #
    Key([mod], "Tab", lazy.next_layout(), desc = "Switch to next layout"),
    Key([mod, "shift"], "Tab", lazy.prev_layout(), desc = "Switch back to previous layout"),
    Key([mod], "j", lazy.layout.down(),    desc = "Switch focus down"),
    Key([mod], "k", lazy.layout.up(), desc = "Switch focus up"),
    Key([mod, "shift"], "j", lazy.layout.shuffle_down(), lazy.layout.section_down(), desc = "Move window down in stack"),
    Key([mod, "shift"], "k", lazy.layout.shuffle_up(), lazy.layout.section_up(), desc = "Move window up in stack"),
    Key([mod, "shift"], "h", lazy.layout.shrink(), lazy.layout.decrease_nmaster(), desc = "Shrink window (MonadTall), decrease number in master pane (Tile)"),
    Key([mod, "shift"], "l", lazy.layout.grow(), lazy.layout.increase_nmaster(), desc = "Expand window (MonadTall), increase number in master pane (Tile)"),
    Key([mod, "control"], "Tab", lazy.layout.rotate(), lazy.layout.flip(), desc = "Flip master pane side (MonadTall)"),
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
    Key([mod, "control"], "p", lazy.spawn([bin_dir + "display-toggle"]), desc = "Toggle display 2 on/off"),
    Key([mod, "control"], "o", lazy.spawn([bin_dir + "display-rotate"]), desc = "Rotate display 1"),
]
# KEYS_END

### GROUPS ###
groups = [
    Group("1", label="1", layout="monadtall", matches=[Match(wm_class=["firefox"])]),
    Group("2", label="2", layout="monadtall", matches=[Match(wm_class=["Thunar", "transmission-gtk"])]),
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
            lazy.group[i.name].toscreen(toggle=True),
            desc="Switch to group {}".format(i.name)
            ),
        Key([mod, "shift"], i.name, lazy.window.togroup(i.name, switch_group=True),
            desc="Send focused window to group {} and switch".format(i.name)),
        Key([mod, "control"], i.name,
            lazy.window.togroup(i.name),
            desc="Send focused window to group {}".format(i.name)
            ),
    ])

### Mouse
mouse = [
    Drag([mod], "Button1", lazy.window.set_position_floating(),
         start=lazy.window.get_position()),
    Drag([mod], "Button3", lazy.window.set_size_floating(),
         start=lazy.window.get_size()),
    Click([mod], "Button2", lazy.window.bring_to_front()),
    Click([], "Button8", lazy.screen.prev_group()),
    Click([], "Button9", lazy.screen.next_group()),
]

### DEFAULT LAYOUT SETTINGS ###
layout_theme = {"border_width" : 2,
                "margin" : 8,
                "border_focus" : colour_accent,
                "border_normal" : colour_bg,
               }

### LAYOUTS ###
layouts = [
    layout.MonadTall(**layout_theme),
    layout.Max(**layout_theme),
    layout.Tile(ratio=0.5,**layout_theme),
    layout.Stack(num_stacks=2,**layout_theme),
]

### WIDGET DEFAULTS ###
widget_defaults = dict(
    font = "JetBrainsMono Nerd Font Medium",
    fontsize = 10,
    padding = 4,
    background = colour_bg,
    foreground = colour_text
)

### WIDGETS - DEFINE ###
def init_widgets_list():
    widgets_list = [
        Sep(linewidth = 0, padding = 4),
        Image(
            filename = "~/.config/qtile/icons/gato.png",
            margin = 3,
            scale = True,
            mouse_callbacks = {'Button3': lambda: 
                               qtile.cmd_spawn([bin_dir + "random-wallpaper"])}
        ),
        GroupBox(
            disable_drag = True,
            rounded = False,
            use_mouse_wheel = False,
            padding_x = 4,
            padding_y = 8,
            fontsize = 9,
            highlight_method = "block",
            active = colour_text,
            inactive = colour_text_dark,
            urgent_border = colour_alt,
            block_highlight_text_color = colour_bg,
            this_current_screen_border = colour_accent,
            other_current_screen_boder = colour_alt,
        ),
        Sep(linewidth = 0, padding = 6),
        WindowName(),
        Sep(linewidth = 0, padding = 6),
        Cmus(
            update_interval = 1,
            play_color = colour_alt,
            max_chars = 50
        ),
        Systray(),
        CheckUpdates(
            update_interval = 1800,
            distro = "Arch_checkupdates",
            display_format = "  {updates}",
            colour_have_updates = colour_text,
            colour_no_updates = colour_text,
            mouse_callbacks = {'Button1': lambda: 
                               qtile.cmd_spawn([bin_dir + "arch-update-notifier"]),
                               'Button3': lambda: qtile.cmd_spawn(terminal + ' -e sudo pacman -Syu')}
        ),
        Net(
           interface = net_adapter,
           format = "  {down}  {up}"
        ),
        PulseVolume(
            fmt = "  {}",
            update_interval = 0.1,
            mouse_callbacks = {'Button3': lambda: qtile.cmd_spawn('easyeffects')}
        ),
        Sep(linewidth = 0, padding = 6),
        Sep(linewidth = 0, padding = 4, background = colour_accent_dark),
        CurrentScreen(
            active_color = colour_text,
            active_text = "",
            inactive_text = "",
            inactive_color = colour_bg,
            background = colour_accent_dark,
            fontsize = 10,
            padding_x = 6,
            padding_y = 2
        ),
        CurrentLayoutIcon(
            scale = 0.4,
            margin = 0,
            custom_icon_paths = [home_dir + ".config/qtile/icons"],
            background = colour_accent_dark
        ),
        Sep(linewidth = 0, padding = 6),
        Clock(format = "%a %d %H:%M"),
        Sep(linewidth = 0, padding = 2 ),
    ]
    if host == "hekate":
        widgets_list.insert(-7, widgets_hekate)

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
    return [Screen(top=bar.Bar(widgets=init_widgets_screen1(), size=25, opacity=0.8)),
            Screen(top=bar.Bar(widgets=init_widgets_screen2(), size=25, opacity=0.8))]
screens = init_screens()

### AUTOSTART SCRIPT ###
@hook.subscribe.startup_once
def start_once():
    call([home_dir + '.config/qtile/scripts/autostart.sh'])

### SET FLOATING WINDOWS AUTOMATICALLY ###
@hook.subscribe.client_new
def floating_dialogs(window):
    dialog = window.window.get_wm_type() == 'dialog'
    transient = window.window.get_wm_transient_for()
    if dialog or transient:
        window.floating = True

follow_mouse_focus = True
cursor_warp = False
bring_front_click = False
focus_on_window_activation = "smart"
reconfigure_screens = True
auto_fullscreen = True
auto_minimize = True

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
    Match(role='About'),
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
    # Match(wm_class='org.inkscape.Inkscape'),
    Match(wm_class='Yad'),
    Match(wm_class='nvidia-settings'),
    Match(wm_class='pulseaudio-equalizer-gtk'),
], fullscreen_border_width = 0, border_width = 0)

wmname = "LG3D"
