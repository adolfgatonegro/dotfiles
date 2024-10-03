return {
	"nvim-telescope/telescope.nvim", tag = "0.1.8",
	dependencies = {
		"nvim-lua/plenary.nvim",
		-- "nvim-telescope/telescope-file-browser.nvim",
		"nvim-tree/nvim-web-devicons",
	},

	event = "VeryLazy",

	config = function()
		require("telescope").setup({
			defaults = {
				-- path_display = { "truncate" },
				prompt_prefix = " 	",
				selection_caret = "⮞ ",
				mappings = {
					i = {
						["<C-n>"] = "cycle_history_next",
						["<C-p>"] = "cycle_history_prev",
						["<C-j>"] = "move_selection_next",
						["<C-k>"] = "move_selection_previous",
						["<C-u>"] = "preview_scrolling_up",
						["<C-d>"] = "preview_scrolling_down",
					},
					n = {
						["q"] = "close",
						["j"] = "move_selection_next",
						["k"] = "move_selection_previous",
						["<C-u>"] = "preview_scrolling_up",
						["<C-d>"] = "preview_scrolling_down",
					},
				},
				file_ignore_patterns = { ".git/" },
			},
			pickers = {
				find_files = {
					theme = "ivy",
				},
				live_grep = {
					theme = "ivy",
				},
				buffers = {
					theme = "ivy",
				},
			},
			-- extensions = {
			-- 	file_browser = {
			-- 		theme = "ivy",
			-- 		hijack_netrw = false,
			-- 		initial_mode = "normal",
			-- 	},
			-- },
		})
		-- require("telescope").load_extension("file_browser")
	end,
	keys = {
		{ "<leader>f", "<cmd>Telescope find_files<CR>", desc = "Telescope: Find files" },
		{ "<leader>g", "<cmd>Telescope live_grep<CR>", desc = "Telescope: Live grep" },
		{ "<leader>b", "<cmd>Telescope buffers<CR>", desc = "Telescope: Buffers" },
		{ "<leader>F", "<cmd>Telescope file_browser<CR>", desc = "Telescope: File browser" },
	},
}
