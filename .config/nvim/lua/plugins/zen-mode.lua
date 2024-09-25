return {
	"folke/zen-mode.nvim",
	opts = {
		window = {
			width = 85,
			options = {
				-- signcolumn = "no", -- disable signcolumn
				number = false, -- disable number column
				relativenumber = false, -- disable relative numbers
				-- cursorline = false, -- disable cursorline
				-- cursorcolumn = false, -- disable cursor column
				-- foldcolumn = "0", -- disable fold column
				-- list = false, -- disable whitespace characters
			},
		},
		on_open = function(_)
			vim.o.cmdheight = 1
			require("lualine").hide()
		end,
		on_close = function()
			vim.o.cmdheight = 0
			require("lualine").hide { unhide = true }
		end,
	}
}
