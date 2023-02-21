from libqtile.config import EzKey as Key, EzDrag as Drag, EzClick as Click
from libqtile.lazy import lazy

mod="mod4"
terminal = "kitty"

@lazy.layout.function
def change_layout_gap(layout, adjustment):
    layout.margin = max(layout.margin + adjustment, 0)
    layout.cmd_reset()

keys = [
    Key("M-<Return>", lazy.spawn(terminal), desc = "Launch terminal"),
    Key("M-r", lazy.spawn("rofi_run"), desc = "Run application launcher"),
    Key("M-w", lazy.window.kill(), desc = "Close the focused window"),
    Key("M-<Escape>", lazy.spawn("xkill"), desc = "Launch xkill"),
    Key("M-C-r", lazy.restart(), desc = "Restart Qtile"),
    Key("M-C-q", lazy.shutdown(), desc = "Shutdown Qtile"),
    Key("M-<Tab>", lazy.next_layout(), desc = "Switch to next layout"),
    Key("M-S-<Tab>", lazy.prev_layout(), desc = "Switch back to previous layout"),
    Key("M-j", lazy.layout.down(), desc = "Switch focus down"),
    Key("M-k", lazy.layout.up(), desc = "Switch focus up"),
    Key("M-l", lazy.layout.next(), desc = "Switch focus up"),
    Key("M-h", lazy.layout.previous(), desc = "Switch focus down"),
    Key("M-S-j", lazy.layout.shuffle_down(), lazy.layout.section_down(), desc = "Move window down in stack"),
    Key("M-S-k", lazy.layout.shuffle_up(), lazy.layout.section_up(), desc = "Move window up in stack"),
    Key("M-S-h", lazy.layout.shrink(), lazy.layout.decrease_nmaster(), desc = "Shrink window (MonadTall), decrease number in master pane (Tile)"),
    Key("M-S-l", lazy.layout.grow(), lazy.layout.increase_nmaster(), desc = "Expand window (MonadTall), increase number in master pane (Tile)"),
    Key("M-S-u", lazy.layout.decrease_ratio(), desc = "Decrease master/stack ratio (Tile)"),
    Key("M-S-i", lazy.layout.increase_ratio(), desc = "Increase master/stack ratio (Tile)"),
    Key("M-C-<Tab>", lazy.layout.rotate(), lazy.layout.flip(), desc = "Flip master pane side (MonadTall)"),
    Key("M-S-p", lazy.layout.toggle_split(), desc = "Switch focus to previous pane in stack (Tile) Toggle sides of stack"),
    Key("M-C-m", lazy.layout.maximize(), desc = "Toggle between minimum and maximum window sizes"),
    Key("M-C-n", lazy.layout.normalize(), desc = "Normalise window size ratios"),
    Key("M-C-f", lazy.window.toggle_floating(), lazy.window.center(), desc = "Toggle floating"),
    Key("M-<F11>", lazy.window.toggle_fullscreen(), desc = "Toggle fullscreen"),
    Key("M-C-h", lazy.screen.prev_group(skip_empty = True), desc = "Move to previous group"),
    Key("M-C-l", lazy.screen.next_group(skip_empty = True), desc = "Move to next group"),
    Key("M-S-w", lazy.to_screen(1), desc = "Switch focus to display 1"),
    Key("M-S-e", lazy.to_screen(2), desc = "Switch focus to display 2"),
    Key("M-S-<period>", lazy.next_screen(), desc = "Switch focus to next display"),
    Key("M-S-<comma>", lazy.prev_screen(),desc = "Switch focus to previous display"),
    Key("M-C-p", lazy.spawn("display_toggle"), desc = "Toggle display 2 on/off"),
    Key("M-C-o", lazy.spawn("display_rotate"), desc = "Rotate display 1"),
    Key("M-<minus>", change_layout_gap(adjustment=-2)),
    Key("M-S-<equal>", change_layout_gap(adjustment=2)),
]

mouse = [
    Drag("M-1", lazy.window.set_position_floating(),
         start=lazy.window.get_position()),
    Drag("M-3", lazy.window.set_size_floating(),
         start=lazy.window.get_size()),
    Click("M-2", lazy.window.bring_to_front()),
]
