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

groups.extend([
    ScratchPad("scratchpad", [
        DropDown("term", "kitty", x=0.25, opacity = 0.8, width=0.5, height=0.5),
        DropDown("sysmon", "kitty -e btop", x=0.25, opacity=0.8, width=0.5, height=0.5),
        DropDown("cmus", "kitty -e cmus", x=0.25, opacity=0.8, width=0.5, height=0.5)
    ]),
])

keys.extend([
    Key("M-m", lazy.group['scratchpad'].dropdown_toggle('cmus')),
    Key("<F10>", lazy.group['scratchpad'].dropdown_toggle('sysmon')),
    Key("<F12>", lazy.group['scratchpad'].dropdown_toggle('term')),
])

# Default layouts
max_groups = [0, 8]
for i, value in enumerate(max_groups):
    groups[i].layout = "max"

# Match rules
groups[0].matches = [Match(wm_class = ["firefox"])]
groups[1].matches = [Match(wm_class = ["transmission-qt","transmission-gtk"])]
groups[3].matches = [Match(wm_class = ["discord"])]
groups[4].matches = [Match(wm_class = ["DesktopEditors", "libreoffice", "libreoffice-writer"])]
groups[5].matches = [Match(wm_class = ["Steam"])]
groups[6].matches = [Match(wm_class = ["gimp-2.10","org.inkscape.Inkscape","darktable"])]
groups[8].matches = [Match(wm_class = ["vmplayer","Vmplayer","virt-manager","VirtualBox Manager","VirtualBox Machine"])]

