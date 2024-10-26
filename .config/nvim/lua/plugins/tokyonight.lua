return {
	"folke/tokyonight.nvim",
	lazy = false,
	priority = 1000,
	opts = {
		style = "night",
		transparent = true,
		transparent_sidebar = true,
		styles = {
			sidebars = "transparent",
			floats = "transparent",
		},
		on_colors = function(colors)
			colors.fg = "#c1c1d1"
			-- colors.bg = "#111117"
			colors.bg = "#16161c"
			colors.blue = "#0077ff"
			colors.cyan = "#00ffe0"
			colors.green = "#aaee00"
			colors.magenta = "#ff00aa"
			colors.orange = "#ff9700"
			colors.purple = "#cf4dff"
			colors.red = "#ff003c"
			colors.yellow = "#ffd000"
		end
	},
}
