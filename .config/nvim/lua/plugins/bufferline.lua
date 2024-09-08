return {
	"akinsho/bufferline.nvim",

	config = function ()
		require("bufferline").setup{
			options = {
				themable = true,
			}
		}
	end,

	dependencies = { "nvim-tree/nvim-web-devicons" },
}
