return {
	"folke/twilight.nvim",
	opts = {
		dimming = {
			alpha = 0.45, -- amount of dimming
			-- we try to get the foreground from the highlight groups or fallback color
			color = { "Normal", "#c1c1d1" },
			term_bg = "#0a0a20", -- if guibg=NONE, this will be used to calculate text color
			inactive = false, -- when true, other windows will be fully dimmed (unless they contain the same buffer)
		},
	}
}
