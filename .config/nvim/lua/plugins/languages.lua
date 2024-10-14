return {
  {
	  'axvr/org.vim',
  },

{
	"MeanderingProgrammer/render-markdown.nvim",
	dependencies = {
	  "nvim-treesitter/nvim-treesitter",
	  "nvim-tree/nvim-web-devicons"
	},

	opts = {
	  render_modes = { 'n', 'v', 'i', 'c' },
	  code = {
	  	  sign = false,
	  	  width = 'block',
	  	  right_pad = 2,
	  	  left_pad = 2,
	  },
	  heading = {
	  	  position = 'inline',
	  	  -- icons = {},
	  },
	  quote = { repeat_linebreak = true },
	  -- win_options = {
	  -- 	showbreak = { default = '', rendered = '  ' },
	  -- 	breakindent = { default = false, rendered = true },
	  -- 	breakindentopt = { default = '', rendered = '' },
	  -- },
	},
},

{
  	"kaarmu/typst.vim",
  	ft = "typst",
  	-- This sets the option correctly, but conceal is not working. Why?
  	init = function()
  		vim.g.typst_conceal = 1
  	end,
  },
}
