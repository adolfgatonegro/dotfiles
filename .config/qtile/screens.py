from os.path import expanduser
from socket import gethostname
from libqtile import qtile, widget, bar
from libqtile.config import Screen
from Xlib import display as xdisplay

host = gethostname()
terminal = "kitty"
calendar = "gsimplecal"

net_adapter = ""
if host == "foxes":
    net_adapter = "eno1"
elif host == "hekate":
    net_adapter = "wlp3s0"

colours = dict(
    white = "#a1a1a1",
    light_grey = "#4b4b51",
    dark_grey = "#35353d",
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
    size = 30,
    background = colours["dark_blue"],
    margin = [6,6,0,6],
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
    margin = 6,
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
    inactive = colours["dark_grey"],
    margin = 4,
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
    border = colours["dark_grey"],
    urgent_border = colours["cyan"],
    margin = 5,
    icon_size = 18,
    txt_floating = "缾 ",
    txt_maximized = "类 ",
    txt_minimized = "絛 ",
)
currentscreen_defaults = dict(
    active_color = colours["white"],
    active_text = "",
    inactive_text = "",
    inactive_color = colours["dark_blue"],
    padding_x = 6,
    padding_y = 2,
)
currentlayouticon_defaults = dict(
    scale = 0.36,
    margin_x = 0,
    custom_icon_paths = [expanduser("~/.config/qtile/icons")],
)

spacer = widget.Spacer(length = 6)

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
    widget.Systray(**widget_defaults),
    widget.CheckUpdates(
        **widget_defaults,
        update_interval = 1800,
        distro = "Arch_checkupdates",
        display_format = "  {updates}",
        colour_have_updates = colours["white"],
        colour_no_updates = colours["white"],
        mouse_callbacks = {'Button1': lambda: qtile.cmd_spawn("arch-update-notifier"),
                           'Button3': lambda: qtile.cmd_spawn(terminal + ' -e sudo pacman -Syu')}
    ),
    widget.Net(
        **widget_defaults,
        interface = net_adapter,
        format = "  {down}  {up}",
    )
]

if host == "hekate":
    main_bar_widgets += [
    widget.Backlight(
        **widget_defaults,
        fmt = '  {}',
        backlight_name = 'amdgpu_bl0',
        ),
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
    widget.PulseVolume(
        **widget_defaults,
        fmt = "  {}",
        update_interval = 0.1,
        mouse_callbacks = {'Button3': lambda: qtile.cmd_spawn('easyeffects')}
    ),
    spacer,
    widget.CurrentLayoutIcon(
        **widget_defaults,
        **currentlayouticon_defaults,
    ),
    widget.Clock(
        **widget_defaults,
        format = "%a %d %H:%M",
        mouse_callbacks = {'Button3' :lambda: qtile.cmd_spawn(calendar)}
    ),
    spacer,
]

screens = [
    Screen(
        top=bar.Bar(
            main_bar_widgets,
            **bar_defaults,
        ),
    ),
]


#https://github.com/qtile/qtile-examples/blob/master/g-wizzy/screens.py
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
                                **widget_defaults,
                                **currentlayouticon_defaults
                            ),
                            spacer
                        ],**bar_defaults,
                ),
            )
        )
