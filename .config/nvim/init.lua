-- neovim config
-- gatoneg.ro

local call = vim.call
local cmd = vim.cmd
local g = vim.g
local opt = vim.opt

-------------
-- PLUGINS --
-------------

-- Using vim-plug with a lua config:
-- https://dev.to/vonheikemen/neovim-using-vim-plug-in-lua-3oom

-- Clone vim-plug if it doesn't exist already
cmd([[
let data_dir = stdpath('data') . '/site'
if empty(glob(data_dir . '/autoload/plug.vim'))
	silent execute '!curl -fLo '.data_dir.'/autoload/plug.vim --create-dirs  https://raw.githubusercontent.com/junegunn/vim-plug/master/plug.vim'
endif

autocmd VimEnter * if len(filter(values(g:plugs), '!isdirectory(v:val.dir)')) | PlugInstall --sync | source $MYVIMRC | endif
]])

-- Set things up to make calling plugins easier
local Plug = vim.fn['plug#']

-- Call plugins
call('plug#begin', '$XDG_CONFIG_HOME/nvim/plugged')
	Plug 'folke/tokyonight.nvim'
	Plug 'folke/which-key.nvim'
	Plug ('gelguy/wilder.nvim', { ['do'] = cmd['UpdateRemotePlugins'] })
	Plug 'jiangmiao/auto-pairs'
	Plug 'NvChad/nvim-colorizer.lua'
	Plug 'nvim-lualine/lualine.nvim'
	Plug 'nvim-tree/nvim-web-devicons'
	Plug 'stevearc/oil.nvim'
	Plug 'tpope/vim-commentary'
	Plug ('kaarmu/typst.vim', { ['for'] = {'typst'} })
call'plug#end'

-- vim-plug in floating window just for a laugh
local w = [[width = math.ceil(vim.api.nvim_get_option("columns") * 0.8), ]]
local h = [[height = math.ceil(vim.api.nvim_get_option("lines") * 0.8), ]]
local r = [[col = math.ceil(vim.api.nvim_get_option("columns") * 0.1 - 1), ]]
local c = [[row = math.ceil(vim.api.nvim_get_option("lines") * 0.1 - 1), ]]
local floating_opts = [[relative = 'editor', style='minimal', border = "single"]]
vim.g.plug_window = [[lua vim.api.nvim_open_win(vim.api.nvim_create_buf(true, false), true, {]] ..
	w .. h .. r .. c .. floating_opts .. "})"

-- Plugin configuration

-- TokyoNight colourscheme
require("tokyonight").setup({
	style = "night",
	transparent = true,
	transparent_sidebar = true,
	styles = {
		sidebars = "transparent", 
		floats = "transparent",
	},
	on_colors = function(colors)
		colors.fg = "#c1c1d1"
		colors.bg = "#1a1a26"
		colors.blue = "#0077ff"
		colors.cyan = "#00ffe0"
		colors.green = "#aaee00"
		colors.magenta = "#ff00aa"
		colors.orange = "#ff9700"
		colors.purple = "#cf4dff"
		colors.red = "#ff003c"
		colors.yellow = "#ffd000"
	end
})

-- Neovim colourscheme
cmd.colorscheme("tokyonight")

-- lualine colourscheme
require('lualine').setup({
	options = {
		theme = 'tokyonight',
		component_separators = '',
		section_separators = '',
	},
	sections = {
		lualine_c = {'buffers'},
	},
	inactive_sections = {
		lualine_c = {'buffers'},
	},
})

-- Set conceal colour for limelight
g.limelight_conceal_ctermfg = 237
g.limelight_conceal_guifg = "#414158"

-- typst.nvim opts
g.typst_conceal = 1

-- nvim-colorizer
require("colorizer").setup()

-- oil.nvim
require("oil").setup({
	delete_to_trash = true,
	skip_confirm_for_simple_edits = true
})

-- which-key
require("which-key").setup({
	keys = {
		scroll_down = "<Tab>",
		scroll_up = "<S-Tab>",
	},
	win = {
		padding = { 1, 1, 1, 1 },
		border = "single"
	},
	layout = { height = { min = 4, max = 10 } }
})

-- wilder.nvim
local wilder = require('wilder')
wilder.setup({modes = {':', '/', '?'}})
wilder.set_option('renderer', wilder.popupmenu_renderer(
	wilder.popupmenu_border_theme({
		border = 'single',
		left = {' ', wilder.popupmenu_devicons()},
		right = {' ', wilder.popupmenu_scrollbar()},
		max_height = '25%',
		min_width = '100%',
		highlighter = wilder.basic_highlighter(),
		highlights = {
			default = wilder.make_hl('WilderPmenu', {{a = 1}, {a = 1}, {background = 'NONE'}}),
			accent = wilder.make_hl('WilderAccent', 'Pmenu', {{a = 1}, {a = 1}, {foreground = '#ff00aa'}}),
		},
	})
))

-------------------------
-- BASIC CONFIGURATION --
-------------------------
local options = {
	autochdir = true,
	background = "dark",
	backup = true,
	clipboard = "unnamedplus",
	cmdheight = 1,
	completeopt = "menuone,longest,preview",
	confirm = true,
	expandtab = false,
	hidden = true,
	hlsearch = true,
	ignorecase = true,
	incsearch = true,
	linebreak = true,
	-- list = true,
	-- listchars = { tab = ">-", trail = "⋄", nbsp = "•", eol = "↴" },
	mouse = "a",
	number = true,
	numberwidth = 4,
	scrolloff = 10,
	shiftwidth = 4,
	showtabline = 0,
	showmode = false,
	softtabstop = 0,
	spelllang = "en_gb,es,fr,it",
	splitbelow = true,
	splitright = true,
	tabstop = 4,
	termguicolors = true,
	textwidth = 0,
	timeout = true,
	timeoutlen = 500,
	undofile = true,
	wildignorecase = true,
	wildmode = "longest:full,full",
	wrap = true,
	wrapmargin = 0,
}

opt.backupdir:remove(".") -- makes sure backups aren"t in the current directory
opt.fillchars:append({ eob = " " }) -- remove the ~ from end of buffer
opt.shortmess:append("c")

for k, v in pairs(options) do
	opt[k] = v
end

------------------
-- KEY BINDINGS --
------------------

-- Custom key mapping
-- https://dev.to/voyeg3r/my-lazy-neovim-config-3h6o
map = function(mode, lhs, rhs, opts)
	local options = { noremap = true, silent = true }
	if opts then
		options = vim.tbl_extend("force", options, opts)
	end
	vim.keymap.set(mode, lhs, rhs, options)
end

local k = map

-- Space as leader
k("", "<Space>", "<Nop>")
g.mapleader = " "
g.maplocalleader = " "

-- Modes
--   normal_mode = "n",
--   insert_mode = "i",
--   visual_mode = "v",
--   visual_block_mode = "x",
--   term_mode = "t",
--   command_mode = "c",

-- ########## Normal ##########

k("n", "Q", "<Nop>") -- Bye-bye, Ex mode

-- Buffers
k("n", "<leader>bd", ":bd<CR>", { desc = "Delete buffer" } )
k("n", "<leader>bb", ":b<Space>", { desc = "Switch buffer" } )
k("n", "<leader>bp", ":bprevious<CR>", { desc = "Previous buffer" } )
k("n", "<leader>bn", ":bnext<CR>", { desc = "Next buffer" } )
k("n", "<leader>bs", ":new<CR>", { desc = "New horizontal split" } )
k("n", "<leader>bv", ":vnew<CR>", { desc = "New vertical split" } )

-- Find file
k("n", "<leader>.", ":find<Space>", { silent = false, desc = "Find file" } )

-- Go to last change in current buffer
k("n", "gl", '`.', { desc = "Go to last change in current buffer" })

-- Line navigation
k("n", "l", "<Space>")
k("n", "h", "<Backspace>")
k("n", "j", "gj")
k("n", "k", "gk")
k({"n", "v"}, "H", "^")
k({"n", "v"}, "L", "$")

-- Go to URL under cursor
k("", "gx", '<Cmd>call jobstart(["xdg-open", expand("<cfile>")], {"detach": v:true})<CR>', { desc = "Go to URL under cursor" })

-- oil.nvim
k("n", "-", "<cmd>Oil<CR>", { desc = "Open parent directory" })

-- Spell check
k("n", "<leader>s", ":setlocal spell!<CR>", { desc = "Toggle spell checking" } )

-- Splits - better navigation
k("n", "<C-h>", "<C-w>h")
k("n", "<C-j>", "<C-w>j")
k("n", "<C-k>", "<C-w>k")
k("n", "<C-l>", "<C-w>l")

-- Splits - resize with arrow keys
k("n", "<C-Up>", ":res +2<CR>")
k("n", "<C-Down>", ":res -2<CR>")
k("n", "<C-Left>", ":vert res +2<CR>")
k("n", "<C-Right>", ":vert res -2<CR>")

-- Toggle key bindings
k("n", "<leader>tg", ":Goyo<CR>", { desc = "Toggle focus mode" })
k("n", "<leader>th", ":set hlsearch!<CR>", { desc = "Toggle highlight for last search term" } )
k("n", "<leader>tw", ":set wrap!<CR>", { desc = "Toggle line wrapping" } )

-- Compiler
k("n", "<leader>c", ":w! | silent!  !compiler \"%:p\"<CR>", { desc = "Compile document" })
k("n", "<leader>p", ":silent! !opout \"%:p\"<CR>", { desc = "Open compiled document" })

-- Write and exit
k("n", "<C-q>", ":x<CR>", { desc = "Write and exit" } )
k("n", "<C-s>", ":up<CR>", { desc = "Write file" } )

-- ########## Visual ##########

-- Paste replace visual selection without copying it
k("v", "p", '"_dP')

-- Search for selection
k("v", "*", "\"zy:let @/=@z<C-r>n<CR>", { desc = "Search for selection" } )

-- ########## Visual Block ##########

-- Move selected block up/down
k("x", "J", ":m '>+1<CR>gv-gv")
k("x", "K", ":m '<-2<CR>gv-gv")

-- Indent selected block
k("x", ">", ">gv")
k("x", "<", "<gv")

------------------
-- AUTOCOMMANDS --
------------------

-- Define autocommands with Lua APIs
local augroup = vim.api.nvim_create_augroup   -- Create/get autocommand group
local autocmd = vim.api.nvim_create_autocmd   -- Create autocommand

-- Automatically rebalance windows on vim resize
-- https://dev.to/voyeg3r/my-lazy-neovim-config-3h6o
autocmd('VimResized', {
  callback = function()
    cmd('tabdo wincmd =')
  end,
  desc = "Auto resize windows when size changes",
})

-- Enable spell checker for certain file types
autocmd({ "BufRead", "BufNewFile" }, {
	pattern = { "*.txt", "*.md", "*.tex", "*.typ" },
	command = "setlocal spell"
})

-- Set conceal for markdown
autocmd({ "BufRead", "BufNewFile" }, {
	pattern = { "*.md", "*.typ" },
	command = "set conceallevel=2 textwidth=80"
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
	command = [[if line("'\"") >= 1 && line("'\"") <= line("$") && &ft !~# 'commit'| execute "normal! g`\"zvzz" | endif]]
})

-- Toggle cursorline
autocmd({ "InsertEnter", "InsertLeave"}, {
	command = [[set cursorline!]]
})

-- Toggle relative numbers based on certain events
-- https://dev.to/voyeg3r/my-lazy-neovim-config-3h6o
augroup('GainFocus', { clear = true })
autocmd({ 'BufEnter', 'FocusGained', 'InsertLeave', 'CmdlineLeave', 'WinEnter' }, {
  pattern = '*',
  group = 'GainFocus',
  callback = function()
    if vim.o.nu and vim.api.nvim_get_mode().mode ~= 'i' then
      opt.relativenumber = true
    end
  end,
})

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
