import subprocess
from os.path import expanduser
from libqtile import hook
# from screens import screens
home_dir = expanduser("~/")

@hook.subscribe.startup_once
def autostart():
    subprocess.call([home_dir + ".config/qtile/autostart.sh"])

@hook.subscribe.client_new
def floating_dialogs(window):
    dialog = window.window.get_wm_type() == 'dialog'
    transient = window.window.get_wm_transient_for()
    if dialog or transient:
        window.floating = True

# @hook.subscribe.screen_change
# def restart_on_randr(qtile):
#     qtile.cmd_restart()

