-- swayimg configuration file
-- gatoeng.ro

-- General config
swayimg.set_mode("viewer")
swayimg.enable_decoration(false)

-- Image list
swayimg.imagelist.enable_adjacent(true)

-- Gallery
swayimg.gallery.enable_preload(true)
swayimg.gallery.enable_pstore(true)
swayimg.gallery.set_selected_scale(1.0)
swayimg.gallery.set_thumb_size(120)

-- Text overlay configuration
swayimg.text.set_size(14)
swayimg.text.set_timeout(10)
swayimg.text.hide()

-- Keys - Viewer
swayimg.viewer.on_key("i", function()
  if swayimg.text.visible() then
    swayimg.text.hide()
  else
    swayimg.text.show()
  end
end)
swayimg.viewer.on_key("Q", function()
  swayimg.exit()
end)
swayimg.viewer.on_key("H", function()
  local wnd = swayimg.get_window_size()
  local pos = swayimg.viewer.get_position()
  swayimg.viewer.set_abs_position(math.floor(pos.x + wnd.width / 10), pos.y);
end)
swayimg.viewer.on_key("L", function()
  local wnd = swayimg.get_window_size()
  local pos = swayimg.viewer.get_position()
  swayimg.viewer.set_abs_position(math.floor(pos.x - wnd.width / 10), pos.y);
end)
swayimg.viewer.on_key("J", function()
  local wnd = swayimg.get_window_size()
  local pos = swayimg.viewer.get_position()
  swayimg.viewer.set_abs_position(pos.x, math.floor(pos.y - wnd.height / 10));
end)
swayimg.viewer.on_key("K", function()
  local wnd = swayimg.get_window_size()
  local pos = swayimg.viewer.get_position()
  swayimg.viewer.set_abs_position(pos.x, math.floor(pos.y + wnd.height / 10));
end)
swayimg.viewer.on_key("Shift+H", function()
  swayimg.viewer.switch_image("first")
end)
swayimg.viewer.on_key("Shift+L", function()
  swayimg.viewer.switch_image("last")
end)
swayimg.viewer.on_key("Backspace", function()
  swayimg.viewer.switch_image("prev")
end)
swayimg.viewer.on_key("Space", function()
  swayimg.viewer.switch_image("next")
end)
swayimg.viewer.on_key("Delete", function()
  local image = swayimg.viewer.get_image()
  os.remove(image.path)
  swayimg.text.set_status("File "..image.path.." removed")
end)
swayimg.viewer.on_key("Shift+w", function()
  local image = swayimg.viewer.get_image()
  os.execute("qs -c noctalia-shell ipc call wallpaper set "..image.path)
  swayimg.text.set_status("Wallpaper set: "..image.path)
end)

-- Keys - Gallery
swayimg.gallery.on_key("Q", function()
  swayimg.exit()
end)
swayimg.gallery.on_key("H", function()
  swayimg.gallery.switch_image("left")
end)
swayimg.gallery.on_key("J", function()
  swayimg.gallery.switch_image("down")
end)
swayimg.gallery.on_key("K", function()
  swayimg.gallery.switch_image("up")
end)
swayimg.gallery.on_key("L", function()
  swayimg.gallery.switch_image("right")
end)
swayimg.gallery.on_key("Shift+H", function()
  swayimg.gallery.switch_image("first")
end)
swayimg.gallery.on_key("Shift+L", function()
  swayimg.gallery.switch_image("last")
end)
swayimg.gallery.on_key("Delete", function()
  local image = swayimg.gallery.get_image()
  os.remove(image.path)
  swayimg.text.set_status("File "..image.path.." removed")
end)

-- Keys - Slideshow
swayimg.slideshow.on_key("Q", function()
  swayimg.exit()
end)
swayimg.slideshow.on_key("Backspace", function()
  swayimg.slideshow.switch_image("prev")
end)
swayimg.slideshow.on_key("Space", function()
  swayimg.slideshow.switch_image("next")
end)
