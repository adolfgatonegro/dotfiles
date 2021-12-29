from os.path import expanduser
from libqtile.config import EzKey as Key, EzDrag as Drag, EzClick as Click
from libqtile.lazy import lazy

mod="mod4"
terminal = "kitty"
home_dir = expanduser("~/")

# KEYS_START
keys = [
    # KEYS_GROUP Qtile Basics #
    Key("M-<F1>", lazy.spawn([home_dir + ".config/qtile/scripts/show-keybindings.sh"]), desc = "Show Qtile keybindings"),
    Key("M-<Return>", lazy.spawn(terminal+" -e"), desc = "Launch terminal"),
    Key("M-r", lazy.spawn("rofi -show drun"), desc = "Run application launcher"),
    Key("M-S-c", lazy.window.kill(), desc = "Close the focused window"),
    Key("M-<Escape>", lazy.spawn("xkill"), desc = "Launch xkill"),
    Key("M-C-r", lazy.restart(), desc = "Restart Qtile"),
    Key("M-C-q", lazy.shutdown(), desc = "Shutdown Qtile"),
    # KEYS_GROUP Layout control #
    Key("M-<Tab>", lazy.next_layout(), desc = "Switch to next layout"),
    Key("M-S-<Tab>", lazy.prev_layout(), desc = "Switch back to previous layout"),
    Key("M-j", lazy.layout.down(),    desc = "Switch focus down"),
    Key("M-k", lazy.layout.up(), desc = "Switch focus up"),
    Key("M-S-j", lazy.layout.shuffle_down(), lazy.layout.section_down(), desc = "Move window down in stack"),
    Key("M-S-k", lazy.layout.shuffle_up(), lazy.layout.section_up(), desc = "Move window up in stack"),
    Key("M-S-h", lazy.layout.shrink(), lazy.layout.decrease_nmaster(), desc = "Shrink window (MonadTall), decrease number in master pane (Tile)"),
    Key("M-S-l", lazy.layout.grow(), lazy.layout.increase_nmaster(), desc = "Expand window (MonadTall), increase number in master pane (Tile)"),
    Key("M-C-<Tab>", lazy.layout.rotate(), lazy.layout.flip(), desc = "Flip master pane side (MonadTall)"),
    Key("M-S-<space>", lazy.layout.previous(), lazy.layout.toggle_split(), desc = "Switch focus to previous pane in stack (Tile) Toggle sides of stack"),
    Key("M-C-m", lazy.layout.maximize(), desc = "Toggle between minimum and maximum window sizes"),
    Key("M-C-n", lazy.layout.normalize(), desc = "Normalise window size ratios"),
    Key("M-C-f", lazy.window.toggle_floating(), desc = "Toggle floating"),
    Key("M-C-<F11>", lazy.window.toggle_fullscreen(), desc = "Toggle fullscreen"),
    # KEYS_GROUP Multi-monitor #
    Key("M-S-w", lazy.to_screen(1), desc = "Switch focus to display 1"),
    Key("M-S-e", lazy.to_screen(2), desc = "Switch focus to display 2"),
    Key("M-S-<period>", lazy.next_screen(), desc = "Switch focus to next display"),
    Key("M-S-<comma>", lazy.prev_screen(),desc = "Switch focus to previous display"),
    Key("M-C-p", lazy.spawn("display-toggle"), desc = "Toggle display 2 on/off"),
    Key("M-C-o", lazy.spawn("display-rotate"), desc = "Rotate display 1"),
]
# KEYS_END

mouse = [
    Drag("M-1", lazy.window.set_position_floating(),
         start=lazy.window.get_position()),
    Drag("M-3", lazy.window.set_size_floating(),
         start=lazy.window.get_size()),
    Click("M-2", lazy.window.bring_to_front()),
    # Click("8", lazy.screen.prev_group(skip_empty = True)),
    # Click("9", lazy.screen.next_group(skip_empty = True)),
]
