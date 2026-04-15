-- Neovim Config
-- gatoneg.ro

-- LOCALS
--
local augroup = vim.api.nvim_create_augroup
local autocmd = vim.api.nvim_create_autocmd
local opt = vim.opt
local cmd = vim.cmd
local g = vim.g
local map = function(mode, lhs, rhs, opts)
	local options = { noremap = true, silent = true }
	if opts then
		options = vim.tbl_extend("force", options, opts)
	end
	vim.keymap.set(mode, lhs, rhs, options)
end
local k = map

--  OPTIONS
-- Theme
-- cmd('colorscheme monochrome')

--  Editor preferences
--
local options = {
	autochdir = true,
	background = "dark",
	backup = true,
	clipboard = "unnamedplus",
	completeopt = "menuone,longest,preview",
	confirm = true,
	cursorline = true,
	expandtab = false,
	hidden = true,
	hlsearch = true,
	ignorecase = true,
	incsearch = true,
	linebreak = true,
	mouse = "a",
	number = true,
	numberwidth = 4,
	scrolloff = 10,
	shiftwidth = 4,
	showtabline = 0,
	showmode = false,
	signcolumn = "yes",
	softtabstop = 0,
	spelllang = "en_gb,es,fr,it",
	splitbelow = true,
	splitright = true,
	tabstop = 4,
	termguicolors = true,
	textwidth = 80,
	timeout = true,
	timeoutlen = 500,
	undofile = true,
	wildignorecase = true,
	wildmode = "longest:full,full",
	wrap = true,
}

opt.backupdir:remove(".") -- makes sure backups aren"t in the current directory
opt.fillchars:append({ eob = " " }) -- remove the ~ from end of buffer
opt.shortmess:append("c")

for k, v in pairs(options) do
	opt[k] = v
end

-- Plugins

vim.pack.add({
  'https://github.com/nvim-mini/mini.nvim',
  'https://github.com/stevearc/oil.nvim',
  'https://github.com/kaarmu/typst.vim',
  'https://github.com/andrewferrier/wrapping.nvim',
  'https://github.com/RRethy/base16-nvim',
  'https://github.com/folke/which-key.nvim',
  'https://github.com/catgoose/nvim-colorizer.lua',
  'https://github.com/neovim/nvim-lspconfig',
  {
    src = 'https://github.com/saghen/blink.cmp',
	name = 'blink.cmp',
	version = 'v1'
  }
})

require('base16-colorscheme').setup()
require('matugen').setup()
require('mini.icons').setup()
require('mini.pairs').setup()
require('mini.pick').setup()
require('blink.cmp').setup()
require('colorizer').setup({
	filetypes = {
		"*",
		"!markdown",
		"!typst",
	},
})

require('which-key').setup({
	preset = "helix",
	delay = "750",
	win = {
		padding = { 1, 1, 1, 1 },
		border = "single"
	},
	layout = { height = { min = 4, max = 10 } }
})

require('oil').setup({
	delete_to_trash = true,
	skip_confirm_for_simple_edits = true,
	columns = { "icon", },
	float = { padding = 4, },
	keymaps = { ["q"] = "actions.close", },
})

require('wrapping').setup({
	softener = { typst = 1.5 },
})

vim.lsp.enable({ 'tinymist' })

--  Set transparent background colours
--
cmd [[
  highlight Normal guibg=none
  highlight Normal ctermbg=none
  highlight NonText guibg=none
  highlight NonText ctermbg=none
  highlight NormalFloat guibg=none
  highlight NormalFloat ctermbg=none
  highlight SignColumn guibg=none
  highlight SignColumn ctermbg=none
  highlight LineNr guibg=none
  highlight LineNr ctermbg=none
]]   

--  KEYMAPS
--

-- Unbind <space> before defining it as leader
k("", "<Space>", "<Nop>")
-- Define leader and localleader (<space> and \)
g.mapleader = " "
g.maplocalleader = "\\"

--  NORMAL mode mappings
k("n", "Q", "<Nop>") -- Disable Ex mode

-- Buffers
k("n", "<leader>b",  "<Nop>",          { desc = "Buffers" } )
k("n", "<leader>bk", ":bd<CR>",        { desc = "Delete current buffer" } )
k("n", "<leader>bn", ":bnext<CR>",     { desc = "Goto next buffer" } )
k("n", "<leader>bp", ":bprevious<CR>", { desc = "Goto previous buffer" } )

k("n", "<leader>p",  "<Nop>", { desc = "Pick" } )
k("n", "<leader>pb",  ":Pick buffers<CR>", { desc = "... buffers" } )
k("n", "<leader>pf",  ":Pick files<CR>", { desc = "... files" } )
k("n", "<leader>ph",  ":Pick help<CR>", { desc = "... help" } )
k("n", "<leader>ph",  ":Pick grep_live<CR>", { desc = "... grep" } )

-- Compiler
k("n", "<leader>c",  "<Nop>",                                { desc = "Compiler" } )
k("n", "<leader>cc", ":w! | silent!  !compiler \"%:p\"<CR>", { desc = "Compile document" })
k("n", "<leader>co", ":silent! !opout \"%:p\"<CR>",          { desc = "Open compiled document" })

-- Typst
k("n", "<leader>t",  "<Nop>", { desc = "Typst" } )
k("n", "<leader>tw", ":call TypstWatch()<CR>", { desc = "Watch this file" })
k("n", "<leader>to", ":silent exec \"!zathura --fork \" .expand (\"%:p:r\") . \".pdf &\"<CR>", { desc = "Open PDF" })

-- Find files
k("n", "<leader>f", ":find ", { desc = "Find files"})

-- Line navigation
k("n", "h", "<Backspace>")
k("n", "j", "<Down>", { remap = true })
k("n", "k", "<Up>", { remap = true })
k("n", "l", "<Space>")
k("v", "j", "gj", { remap = true })
k("v", "k", "gk", { remap = true })

-- oil.nvim
k("n", "-", "<cmd>Oil<CR>", { desc = "Open Oil" })

-- LSP
k("n", "<leader>lf", vim.lsp.buf.format, { desc = "LSP Buffer Format" } )

-- Splits - resize with arrow keys
k("n", "<C-Up>", ":res +2<CR>")
k("n", "<C-Down>", ":res -2<CR>")
k("n", "<C-Left>", ":vert res +2<CR>")
k("n", "<C-Right>", ":vert res -2<CR>")

-- Toggle key bindings
k("n", "<leader>s",  "<Nop>",              { desc = "Set option..." } )
k("n", "<leader>sh", ":set hlsearch!<CR>", { desc = "Highlight for last search term" } )
k("n", "<leader>sl", ":set wrap!<CR>",     { desc = "Line wrapping" } )
k("n", "<leader>sw", ":ToggleWrapMode<CR>",     { desc = "Soft/Hard wrap mode" } )

-- Go to last change in current buffer
k("n", "gl", '`.', { desc = "Go to last change in current buffer" } )

-- Go to URL under cursor
k("", "gx", '<Cmd>call jobstart(["xdg-open", expand("<cfile>")], {"detach": v:true})<CR>', { desc = "Go to URL under cursor" } )

-- Duplicate line and keep cursor in same colum
k("n", "<leader>,", "yymmp`mj", { desc = "Duplicate current line, keep cursor column" } )

-- Write and exit
k("n", "<C-q>", ":x<CR>", { desc = "Write and exit" } )
k("n", "<C-s>", ":up<CR>", { desc = "Write file" } )

--  VISUAL mappings
--
-- Paste replace visual selection without copying it
k("v", "p", '"_dP', { desc = "Paste replace without yanking" } )

-- Search for selection
k("v", "*", "\"zy:let @/=@z<C-r>n<CR>", { desc = "Search for selection" } )

--  VISUAL BLOCK mappings
--
-- Move selected block up/down
k("x", "<C-j>", ":m '>+1<CR>gv-gv")
k("x", "<C-k>", ":m '<-2<CR>gv-gv")

-- Indent selected block
k("x", ">", ">gv")
k("x", "<", "<gv")

-- AUTOCOMMANDS
--
-- -- Automatically rebalance windows on vim resize
-- -- https://dev.to/voyeg3r/my-lazy-neovim-config-3h6o
-- autocmd('VimResized', {
-- 	callback = function()
-- 		cmd('tabdo wincmd =')
-- 	end,
-- 	desc = "Auto resize windows when size changes",
-- })

-- Set conceallevel and spellchecking for markdown, typst, and text files
autocmd({ "FileType" }, {
	pattern = { "markdown", "typst", "tex", "plaintext" },
	command = "set conceallevel=2 | setlocal spell"
})

-- Highlight on yank
augroup('YankHighlight', { clear = true })
autocmd('TextYankPost', {
	group = 'YankHighlight',
	callback = function()
		vim.highlight.on_yank({ higroup = 'IncSearch', timeout = '500' })
	end
})

-- Restore cursor position in buffer
autocmd("BufReadPost", {
	pattern = "",
	command = [[if line("'\"") >= 1 && line("'\"") <= line("$") && &ft !~ 'commit'| execute "normal! g`\"zvzz" | endif]]
})

-- Toggle cursorline
autocmd({ "InsertEnter", "InsertLeave"}, {
	command = [[set cursorline!]]
})

-- Toggle relative numbers based on certain events
-- https://dev.to/voyeg3r/my-lazy-neovim-config-3h6o
augroup('GainFocus', { clear = true })
autocmd({ 'BufLeave', 'FocusLost', 'InsertEnter', 'CmdlineEnter', 'WinLeave' }, {
	pattern = '*',
	group = 'GainFocus',
	callback = function()
		if vim.o.nu then
			opt.relativenumber = false
			cmd('redraw')
		end
	end,
})

-- FUNCTIONS
--
-- TypstWatch
-- cmd [[
-- function GitRoot()
-- 	return fnamemodify(finddir('.git', ";"), ":h")
-- endfunc
--
-- function TypstWatch()
-- 	vsp
-- 	vertical resize 20
-- 	exec 'terminal ' .. 'typst watch --root ' .. GitRoot() .. " " .. expand("%:")
-- 	exec "norm \<c-w>h"
-- endfunc
-- ]]
--
-- vim.lsp.enable({"tinymist"})
