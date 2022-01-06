from libqtile import layout
from libqtile.config import Match

from screens import colours

layout_defaults = dict(
    margin = 6,
    border_width = 2,
    border_focus = colours["hot_pink"],
    border_normal = colours["dark_grey"],
)

floating_layout_defaults = layout_defaults.copy()
floating_layout_defaults["border_focus"] = colours["cyan"]
floating_layout_defaults["border_width"] = 1

layouts = [
    layout.MonadTall(**layout_defaults),
    layout.Max(**layout_defaults),
    layout.Tile(ratio = 0.5, **layout_defaults),
    layout.Stack(num_stacks = 2, **layout_defaults),
]

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
    Match(wm_class='transmission-qt'),
    # Match(wm_class='gimp-2.10'),
    # Match(wm_class='org.inkscape.Inkscape'),
    Match(wm_class='Yad'),
    Match(wm_class='nvidia-settings'),
], **floating_layout_defaults)

