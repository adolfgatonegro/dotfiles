import subprocess
import psutil
from os.path import expanduser
from libqtile import hook

@hook.subscribe.startup_once
def start_once():
    subprocess.call(expanduser('~/.config/qtile/scripts/autostart.sh'))

@hook.subscribe.client_new
def floating_dialogs(window):
    dialog = window.window.get_wm_type() == 'dialog'
    transient = window.window.get_wm_transient_for()
    if dialog or transient:
        window.floating = True

@hook.subscribe.client_new
def swallow(window):
    pid = window.window.get_net_wm_pid()
    ppid = psutil.Process(pid).ppid()
    cpids = {c.window.get_net_wm_pid(): wid for wid, c in window.qtile.windows_map.items()}
    for i in range(5):
        if not ppid:
            return
        if ppid in cpids:
            parent = window.qtile.windows_map.get(cpids[ppid])
            parent.minimized = True
            window.parent = parent
            return
        ppid = psutil.Process(ppid).ppid()

@hook.subscribe.client_killed
def unswallow(window):
    if hasattr(window, 'parent'):
        window.parent.minimized = False

@hook.subscribe.screens_reconfigured
def set_wallpaper():
    subprocess.call(expanduser('~/.local/bin/setwall'))

# @hook.subscribe.screen_change
# def restart_on_randr(qtile, ev):
#     qtile.cmd_restart()
