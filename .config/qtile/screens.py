from os.path import expanduser
from socket import gethostname
from libqtile import qtile, widget, bar
from libqtile.config import Screen
from Xlib import display as xdisplay
from widgets.volume import Volume

host = gethostname()
terminal = "kitty"
calendar = "gsimplecal"

net_adapter = {
    "foxes" : "eno1",
    "hekate" : "wlp3s0",
    "lucille" : "wlp2s0b1"
}

colours = dict(
    # white = "#a1a1a1",
    # light_grey = "#4b4b51",
    # dark_grey = "#35353d",
    white = "#c1c1d1",
    light_grey = "#6c6c93",
    dark_grey = "#363649",
    black = "#101010",
    dark_blue = "#0a0a20",
    hot_pink = "#ff00aa",
    burgundy = "#660044",
    cyan = "#00ffe0",
    mint = "#58aa99",
    lime_green = "#aaee00",
    orange = "#ff8700",
    blue = "#0077f0",
    purple = "#bb00f0",
)

bar_defaults = dict(
    size = 23,
    background = colours["dark_blue"],
    # margin = [6,6,0,6],
    opacity = 0.8,
)

widget_defaults = dict(
    font = "JetBrainsMonoMedium Nerd Font",
    fontsize = 10,
    font_size = 10,
    foreground = colours["white"],
    padding = 4,
)

gato_logo = widget.Image(
    filename = expanduser("~/.config/qtile/icons/gato.png"),
    margin = 3,
    scale = True,
    mouse_callbacks = {"Button3": lambda: qtile.cmd_spawn("random-wallpaper")},
)

groupbox_defaults = dict(
    disable_drag = True,
    rounded = False,
    use_mouse_wheel = False,
    borderwidth = 3,
    # highlight_color = ['#000000', '#282828'],
    highlight_method = "block",
    hide_unused = True,
    inactive = colours["dark_grey"],
    margin_y = 4,
    margin_x = 2,
    other_screen_border = colours["burgundy"],
    other_current_screen_border = colours["dark_grey"],
    active = colours["white"],
    block_highlight_text_color = colours["dark_blue"],
    this_current_screen_border = colours["hot_pink"],
    this_screen_border = colours["light_grey"],
    urgent_alert_method = "block",
    urgent_border = colours["orange"],
    urgent_text = colours["orange"],
)
tasklist_defaults = dict(
    highlight_method = "block",
    border = colours["dark_blue"],
    urgent_border = colours["cyan"],
    unfocused_border = colours["dark_blue"],
    rounded = False,
    margin = 2,
    spacing = 10,
    icon_size = 12,
    txt_floating = "缾 ",
    txt_maximized = "类 ",
    txt_minimized = "絛 ",
)
currentscreen_defaults = dict(
    active_color = colours["white"],
    active_text = "",
    inactive_text = "",
    inactive_color = colours["dark_blue"],
    padding_x = 0,
    padding_y = 2,
)
currentlayouticon_defaults = dict(
    scale = 0.5,
    margin = 0,
    padding = 0,
    custom_icon_paths = [expanduser("~/.config/qtile/icons")],
)

# spacer = widget.Spacer(length =6)

extension_defaults = widget_defaults.copy()

main_bar_widgets = [
    gato_logo,
    widget.GroupBox(
        **widget_defaults,
        **groupbox_defaults,
    ),
    widget.TaskList(
        **widget_defaults,
        **tasklist_defaults,
    ),
    widget.Cmus(
        **widget_defaults,
        update_interval = 1,
        play_color = colours["cyan"],
        max_chars = 50,
    ),
    widget.Systray(**widget_defaults),
    widget.CheckUpdates(
        **widget_defaults,
        update_interval = 1800,
        distro = "Arch_checkupdates",
        display_format = " {updates}",
        colour_have_updates = colours["white"],
        colour_no_updates = colours["white"],
        mouse_callbacks = {'Button1': lambda: qtile.cmd_spawn("arch-update-notifier")}
    ),
    widget.Net(
        **widget_defaults,
        interface = net_adapter[host],
        prefix = "M",
        format = " {down}  {up}",
    )
]

if host == "hekate" or host == "lucille":
    main_bar_widgets += [
    widget.Battery(
        **widget_defaults,
        charge_char = '',
        discharge_char = '',
        empty_char = '',
        full_char = '',
        unknown_char = '',
        format = '{char} {percent:2.0%}',
        show_short_text = False
        ),
    ]

main_bar_widgets += [
    Volume(
        **widget_defaults,
        fmt = " {}",
        get_volume_command = "pamixer --get-volume",
        check_mute_command = "pamixer --get-mute",
        check_mute_string = "true",
        volume_up_command = "pamixer -i 2",
        volume_down_command = "pamixer -d 2",
        mute_command = "pamixer -t",
        mouse_callbacks = {'Button3': lambda: qtile.cmd_spawn("easyeffects")}
    ),
    widget.CurrentLayoutIcon(
        **currentlayouticon_defaults,
    ),
    widget.Clock(
        **widget_defaults,
        format = "%a %d %H:%M",
        mouse_callbacks = {'Button3' :lambda: qtile.cmd_spawn(calendar)}
    ),
]

screens = [
    Screen(
        top=bar.Bar(
            main_bar_widgets,
            **bar_defaults,
        ),
    ),
]

# https://github.com/qtile/qtile-examples/blob/master/g-wizzy/screens.py
def get_num_monitors():
    num_monitors = 0
    try:
        display = xdisplay.Display()
        screen = display.screen()
        resources = screen.root.xrandr_get_screen_resources()

        for output in resources.outputs:
            monitor = display.xrandr_get_output_info(output, resources.config_timestamp)
            preferred = False
            if hasattr(monitor, "preferred"):
                preferred = monitor.preferred
            elif hasattr(monitor, "num_preferred"):
                preferred = monitor.num_preferred
            if preferred:
                num_monitors += 1
    except Exception as e:
        return 1
    else:
        return num_monitors

monitor_num = get_num_monitors()

if monitor_num > 1:
    main_bar_widgets.insert(-3,widget.CurrentScreen(**widget_defaults, **currentscreen_defaults))
    for monitor in range(monitor_num -1):
        screens.append(
                Screen(
                    top=bar.Bar(
                        [
                            gato_logo,
                            widget.GroupBox(
                                **widget_defaults,
                                **groupbox_defaults
                            ),
                            widget.TaskList(
                                **widget_defaults,
                                **tasklist_defaults
                            ),
                            widget.CurrentScreen(
                                **widget_defaults,
                                **currentscreen_defaults
                            ),
                            widget.CurrentLayoutIcon(
                                **currentlayouticon_defaults
                            ),
                        ],**bar_defaults,
                ),
            )
        )
