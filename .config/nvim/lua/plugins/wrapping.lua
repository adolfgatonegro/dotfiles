return {
	"andrewferrier/wrapping.nvim",

	config = function()
		require("wrapping").setup({
			softener = { typst = 1.5 },
		})
	end
}
