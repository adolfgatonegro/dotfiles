import gi
gi.require_version("Gdk", "3.0")
from gi.repository import Gdk

active_monitors = 0

disp = Gdk.Display.get_default()
for i in range(disp.get_n_monitors()):
    monitor = disp.get_monitor(i)
    active_monitors += 1

# print(active_monitors)
