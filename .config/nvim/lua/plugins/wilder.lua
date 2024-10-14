return {
	"gelguy/wilder.nvim",

	config = function()
		local wilder = require("wilder")

		wilder.setup({
			modes = {':', '/', '?'},
			--next_key = '<C-j>',
			--previous_key = '<C-k>',
			accept_key = '<C-l>',
			reject_key = '<C-h>',
		})

		wilder.set_option('renderer', wilder.popupmenu_renderer(
			wilder.popupmenu_border_theme({
				border = 'single',
				left = {' ', wilder.popupmenu_devicons()},
				right = {' ', wilder.popupmenu_scrollbar()},
				max_height = '25%',
				min_width = '100%',
				highlighter = wilder.basic_highlighter(),
				highlights = {
					default = wilder.make_hl('WilderPmenu', {{a = 1}, {a = 1}, {background = 'NONE'}}),
					accent = wilder.make_hl('WilderAccent', 'Pmenu', {{a = 1}, {a = 1}, {foreground = '#ff00aa'}}),
				},
			})
		))

	end
}
