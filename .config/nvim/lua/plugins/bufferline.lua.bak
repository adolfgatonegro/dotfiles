return {
	"akinsho/bufferline.nvim",

	config = function ()
		require("bufferline").setup{
			options = {
				themable = false,
				separator_style = { "", "" },
				enforce_regular_tabs = false,
				diagnostics = "nvim_lsp",
			},
			highlights = {
				buffer_selected = { bold = true, italic = false },
				fill = { bg = "NONE" },
			},
		}
	end,

	dependencies = { "nvim-tree/nvim-web-devicons" },
}
