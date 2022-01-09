import subprocess
from os.path import expanduser
from libqtile import hook
# from screens import screens

@hook.subscribe.startup_once
def start_once():
    subprocess.call(expanduser('~/.config/qtile/scripts/autostart.sh'))

@hook.subscribe.client_new
def floating_dialogs(window):
    dialog = window.window.get_wm_type() == 'dialog'
    transient = window.window.get_wm_transient_for()
    if dialog or transient:
        window.floating = True

# @hook.subscribe.screen_change
# def restart_on_randr(qtile):
#     qtile.cmd_restart()

