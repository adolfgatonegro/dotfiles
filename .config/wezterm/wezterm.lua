local wezterm = require 'wezterm'
local config = {}

config.colors = {
  foreground = '#c1c1d1',
  background = '#0a0a15',
  cursor_bg = '#00ffe0',
  cursor_fg = '#0a0a15',
  cursor_border = '#00ffe0',
  selection_fg = '#0a0a15',
  selection_bg = '#c1c1d1',
  scrollbar_thumb = '#262639',
  split = '#363649',
  ansi = {
    '#0a0a15',
    '#ff00aa',
    '#aaee00',
    '#ff9700',
    '#0077ff',
    '#cf4dff',
    '#00ffe0',
    '#c1c1d1',
  },
  brights = {
    '#6c6c93',
    '#ff00aa',
    '#a8df20',
    '#ff8700',
    '#207fec',
    '#bb00ff',
    '#00cce0',
    '#a1a1c1',
  },
}
config.enable_tab_bar = false
config.font = wezterm.font 'monospace'
config.font_size = 9
config.inactive_pane_hsb = {
  saturation = 0.9,
  brightness = 0.8,
}config.line_height = 1.25
config.window_background_opacity = 0.85
config.window_padding = {
  left = 20,
  right = 20,
  top = 20,
  bottom = 20,
}

return config
