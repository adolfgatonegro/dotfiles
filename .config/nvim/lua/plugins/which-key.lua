return {
	"folke/which-key.nvim",
	event = "VeryLazy",
	opts = {
		win = {
			padding = { 1, 1, 1, 1 },
			border = "single"
		},
		layout = { height = { min = 4, max = 10 } }
	},
	keys = {
		{
			"<leader>?",
			function()
				require("which-key").show({ global = false })
			end,
			desc = "Buffer Local Keymaps (which-key)",
		},
	},
}
