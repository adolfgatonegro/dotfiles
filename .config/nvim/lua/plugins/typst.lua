return {
	"kaarmu/typst.vim",
	ft = "typst",
	-- This sets the option correctly, but conceal is not working. Why?
	init = function()
		vim.g.typst_conceal = 1
	end,
}
