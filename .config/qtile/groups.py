from libqtile.config import Group, ScratchPad, DropDown, Match, EzKey as Key
from libqtile.lazy import lazy

from keys import keys

groups = [Group(i) for i in "123456789"]

for i in groups:
    keys.extend([
        Key(f"M-{i.name}", lazy.group[i.name].toscreen(toggle=True), desc=f"Switch to group {i.name}"),
        Key(f"M-S-{i.name}", lazy.window.togroup(i.name, switch_group=True), desc=f"Send focused window to group {i.name} and switch"),
        Key(f"M-C-{i.name}", lazy.window.togroup(i.name), desc=f"Send focused window to group {i.name}")
    ])

groups.extend([ScratchPad("scratchpad", [
    DropDown("term", "kitty", x=0.25, opacity = 0.8, width=0.5, height=0.5),])])

keys.extend([
    Key("<F12>", lazy.group['scratchpad'].dropdown_toggle('term')),
    ])

groups[0].matches = [Match(wm_class = ["firefox"])]
groups[1].matches = [Match(wm_class = ["Thunar","transmission-qt"])]
# groups[2].matches = [Match(wm_class = [""])]
groups[3].matches = [Match(wm_class = ["discord","mailspring"])]
groups[4].matches = [Match(wm_class = ["DesktopEditors"])]
groups[5].matches = [Match(wm_class = ["Steam"])]
groups[6].matches = [Match(wm_class = ["gimp-2.10","org.inkscape.Inkscape"])]
# groups[7].matches = [Match(wm_class = [""])]
groups[8].matches = [Match(wm_class = ["VirtualBox Manager","VirtualBox Machine"])]

