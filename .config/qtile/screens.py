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
    "foxes" : "enp42s0",
    "lucille" : "wlp2s0"
}

colours = dict(
    # black   = "#101010",
    # grey0   = "#0a0a20",
    # grey1   = "#1a1a26",
    # grey2   = "#262639",
    # grey3   = "#363649",
    # grey4   = "#414158",
    # grey5   = "#6c6c93",
    # grey6   = "#c1c1d1",
    # white   = "#ffffff",

    none    = "#00000000",
    fg      = "#c1c1d1",
    grey0   = "#6c6c93",
    grey1   = "#363649",
    bg      = "#0a0a20",
    pink0   = "#ff00aa",
    pink1   = "#660044",
    orange0 = "#ff9700",
    orange1 = "#ff8700",
    yellow0 = "#ffd000",
    green0  = "#aaee00",
    green1  = "#a8df20",
    purple0 = "#cf4dff",
    purple1 = "#bb00ff",
    blue0   = "#0077ff",
    blue1   = "#207fec",
    cyan0   = "#00ffe0",
    cyan1   = "#58aa99",
)

bar_defaults = dict(
    size = 25,
    background = colours["bg"],
    opacity = 0.8,
)

widget_defaults = dict(
    font = "JetBrainsMonoMedium Nerd Font",
    fontsize = 10,
    font_size = 10,
    foreground = colours["fg"],
    padding = 6,
)

gato_logo = widget.Image(
    filename = expanduser("~/.config/qtile/icons/gato.png"),
    margin = 2,
    scale = True,
    mouse_callbacks = {"Button3": lambda: qtile.cmd_spawn("random-wallpaper"),
                       "Button1": lambda: qtile.cmd_spawn("rofi-run")},
)

groupbox_defaults = dict(
    disable_drag = True,
    rounded = False,
    use_mouse_wheel = False,
    borderwidth = 3,
    highlight_method = "block",
    hide_unused = True,
    inactive = colours["grey1"],
    margin_y = 4,
    margin_x = 2,
    other_screen_border = colours["pink1"],
    other_current_screen_border = colours["grey1"],
    active = colours["fg"],
    block_highlight_text_color = colours["bg"],
    this_current_screen_border = colours["pink0"],
    this_screen_border = colours["grey0"],
    urgent_alert_method = "block",
    urgent_border = colours["orange0"],
    urgent_text = colours["orange0"],
)
tasklist_defaults = dict(
    highlight_method = "block",
    urgent_alert_method = "text",
    border = colours["grey1"],
    urgent_border = colours["cyan0"],
    unfocused_border = colours["bg"],
    rounded = False,
    margin = 0,
    spacing = 10,
    icon_size = 0,
    txt_floating = "缾 ",
    txt_maximized = "类 ",
    txt_minimized = "絛 ",
    title_width_method = "uniform",
)
currentscreen_defaults = dict(
    active_color = colours["fg"],
    active_text = "",
    inactive_text = "",
    inactive_color = colours["bg"],
    padding_x = 0,
    padding_y = 2,
)
currentlayouticon_defaults = dict(
    scale = 0.5,
    margin = 0,
    padding = 0,
    custom_icon_paths = [expanduser("~/.config/qtile/icons")],
)

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
        play_color = colours["cyan0"],
        max_chars = 50,
    ),
    widget.Systray(**widget_defaults),
    widget.CheckUpdates(
        **widget_defaults,
        update_interval = 21600,
        distro = "Arch_checkupdates",
        display_format = " {updates}",
        colour_have_updates = colours["fg"],
        colour_no_updates = colours["fg"],
        mouse_callbacks = {'Button1': lambda: qtile.cmd_spawn("arch-update-notifier")}
    ),
    widget.Net(
        **widget_defaults,
        interface = net_adapter[host],
        prefix = "M",
        format = " {down} {up}",
    )
]

if host == "lucille":
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
        get_volume_command = "pamixer --get-volume-human",
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
                            Volume(
                                **widget_defaults,
                                fmt = " {}",
                                get_volume_command = "pamixer --get-volume-human",
                                check_mute_command = "pamixer --get-mute",
                                check_mute_string = "true",
                                volume_up_command = "pamixer -i 2",
                                volume_down_command = "pamixer -d 2",
                                mute_command = "pamixer -t",
                                mouse_callbacks = {'Button3': lambda: qtile.cmd_spawn("easyeffects")}
                            ),
                            widget.CurrentLayoutIcon(
                                **currentlayouticon_defaults
                            ),
                        ],**bar_defaults,
                ),
            )
        )
