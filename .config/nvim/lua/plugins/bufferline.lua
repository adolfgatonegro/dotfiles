return {
	"akinsho/bufferline.nvim",

	config = function ()
		require("bufferline").setup{
			highlights = {
				buffer_selected = { bold = true, italic = false },
				group_label = { bg = "NONE" },
				separator = { fg = "NONE" },
				fill = { bg = "NONE" },
				separator_visible ={ fg = "NONE" },
				separator_selected ={ fg = "NONE" },
				offset_separator = {
					big = "NONE",
					italic = false,
					bold = true,
				},
			},
			options = {
				themable = false,
				indicator = { style = 'none'},
				diagnostics = "nvim_lsp",
				separator_style = "thick",
			}
		}
	end,

	dependencies = { "nvim-tree/nvim-web-devicons" },
}
