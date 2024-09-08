return {
	"preservim/vim-pencil",

	config = function ()
		vim.g["pencil#wrapModeDefault"] = "soft"
		vim.g["pencil#conceallevel"] = 0
		vim.g["pencil#concealcursor"] = "c"

		vim.cmd([[
			let g:pencil#mode_indicators = {'hard': 'H', 'auto': 'A', 'soft': 'S', 'off': '',}
		]])

		vim.cmd([[
			augroup pencil
				autocmd!
				autocmd FileType markdown,md,typst call pencil#init({'wrap': 'soft'})
				autocmd FileType text call pencil#init({'wrap': 'hard'})
			augroup END
		]])
	end
}
