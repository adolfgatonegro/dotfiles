-- neovim config
-- gatoneg.ro

-------------
-- PLUGINS --
-------------

-- Clone vim-plug if it doesn't exist already
vim.cmd([[
let data_dir = has('nvim') ? stdpath('data') . '/site' : '$XDG_CONFIG_HOME/vim'
if empty(glob(data_dir . '/autoload/plug.vim'))
	silent execute '!curl -fLo '.data_dir.'/autoload/plug.vim --create-dirs  https://raw.githubusercontent.com/junegunn/vim-plug/master/plug.vim'
endif

autocmd VimEnter * if len(filter(values(g:plugs), '!isdirectory(v:val.dir)')) | PlugInstall --sync | source $MYVIMRC | endif
]])

-- Set things up to make calling plugins easier
local call = vim.call
local cmd = vim.cmd
local Plug = vim.fn['plug#']
local PATH = "$XDG_CONFIG_HOME/nvim/plugged"

-- Call plugins
call('plug#begin', PATH)
	Plug 'ap/vim-css-color'
	Plug 'folke/tokyonight.nvim'
	Plug 'jiangmiao/auto-pairs'
	Plug 'junegunn/goyo.vim'
	Plug 'junegunn/limelight.vim'
	Plug 'nvim-lualine/lualine.nvim'
	Plug 'tpope/vim-commentary'
call'plug#end'

-- Plugin configuration

-- tokyonight colourscheme setup
require("tokyonight").setup({
	style = "night",
	transparent = true,
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

-- Set lualine colourscheme
require('lualine').setup {
  options = { theme = 'tokyonight' }
}

-- Set conceal colour for limelight
vim.g.limelight_conceal_ctermfg = 237
vim.g.limelight_conceal_guifg = "#414158"

-- Configure Goyo + Limelight
local goyo_group = vim.api.nvim_create_augroup("GoyoGroup", { clear = true })
vim.api.nvim_create_autocmd("User", {
    desc = "Hide lualine on goyo enter",
    group = goyo_group,
    pattern = "GoyoEnter",
    callback = function()
        require("lualine").hide()
		vim.cmd("Limelight")
		-- Hide weird line at the bottom in focus mode
		vim.cmd("highlight StatusLineNC ctermfg=white")
    end,
})

vim.api.nvim_create_autocmd("User", {
    desc = "Show lualine after goyo exit",
    group = goyo_group,
    pattern = "GoyoLeave",
    callback = function()
        require("lualine").hide({ unhide = true })
		vim.cmd("Limelight!")
    end,
})

-- Set neovim colourscheme
vim.cmd[[colorscheme tokyonight]]

-------------------------
-- BASIC CONFIGURATION --
-------------------------
local options = {
	autochdir = true,
	backup = true,
	clipboard = "unnamedplus",
	cmdheight = 1,
	completeopt = "menuone,longest,preview",
	confirm = true,
	expandtab = false,
	hidden = true,
	hlsearch = false,
	ignorecase = true,
	linebreak = true,
	-- list = true,
	-- listchars = { tab = ">-", trail = "⋄", nbsp = "•", eol = "↴" },
	mouse = "a",
	number = true,
	numberwidth = 4,
	scrolloff = 10,
	shiftwidth = 4,
	showtabline = 1,
	showmode = false,
	softtabstop = 0,
	spelllang = "en_gb,es,fr,it",
	splitbelow = true,
	splitright = true,
	tabstop = 4,
	termguicolors = true,
	textwidth = 0,
	undofile = true,
	wildignorecase = true,
	wildmode = "longest:full,full",
	wrap = true,
	wrapmargin = 0,
}

vim.opt.backupdir:remove(".") -- makes sure backups aren"t in the current directory
vim.opt.fillchars:append({ eob = " " }) -- remove the ~ from end of buffer
vim.opt.shortmess:append("c")

for k, v in pairs(options) do
	vim.opt[k] = v
end

------------------
-- KEY BINDINGS --
------------------
local opts = { noremap = true, silent = true }
local term_opts = { silent = true }
local k = vim.api.nvim_set_keymap

-- Space as leader
k("", "<Space>", "<Nop>", opts)
vim.g.mapleader = " "
vim.g.maplocalleader = " "

-- Modes
--   normal_mode = "n",
--   insert_mode = "i",
--   visual_mode = "v",
--   visual_block_mode = "x",
--   term_mode = "t",
--   command_mode = "c",

-- ########## Normal ##########

k("n", "Q", "<nop>", opts)

-- Buffers
k("n", "<leader>bd", ":bd<CR>", opts )
k("n", "<leader>bl", ":bnext<CR>", opts )
k("n", "<leader>bh", ":bprevious<CR>", opts )
k("n", "<leader>bs", ":new<CR>", opts )
k("n", "<leader>bv", ":vnew<CR>", opts )

-- Edit/reload config
k("n", "<leader>ce", ":e $MYVIMRC<CR>", opts)
k("n", "<leader>cr", ":so $MYVIMRC<CR>", opts)

-- Find file
k("n", "<leader>.", ":find ", { noremap = true } )

-- Line navigation
k("n", "l", "<Space>", opts)
k("n", "h", "<Backspace>", opts)
k("n", "j", "gj", opts)
k("n", "k", "gk", opts)
k("n", "H", "^", opts)
k("n", "L", "$", opts)

-- Toggle search highlighting
k("n", "<leader>*", ":set hlsearch!<CR>", opts)

-- Spell check
k("n", "<leader>sp", ":setlocal spell!<CR>", opts)

-- Split navigation
k("n", "<C-h>", "<C-w>h", opts)
k("n", "<C-j>", "<C-w>j", opts)
k("n", "<C-k>", "<C-w>k", opts)
k("n", "<C-l>", "<C-w>l", opts)

-- Split resizing
k("n", "<C-Up>", ":res +2<CR>", opts)
k("n", "<C-Down>", ":res -2<CR>", opts)
k("n", "<C-Left>", ":vert res +2<CR>", opts)
k("n", "<C-Right>", ":vert res -2<CR>", opts)

-- Write and exit
k("n", "<C-q>", ":x<CR>", opts)
k("n", "<C-s>", ":up<CR>", opts)

-- Goyo and Limelight
k("n", "<leader>g", ":Goyo<CR>", opts)

-- ########## Visual ##########

-- Search for selection
k("v", "*", "\"zy:let @/=@z<C-r>n<CR>", opts)

-- ########## Visual Block ##########

-- Move selected block up/down
k("x", "J", ":m '>+1<CR>gv-gv", opts)
k("x", "K", ":m '<-2<CR>gv-gv", opts)

-- Indent selected block
k("x", ">", ">gv", opts)
k("x", "<", "<gv", opts)

------------------
-- AUTOCOMMANDS --
------------------

-- Define autocommands with Lua APIs
local augroup = vim.api.nvim_create_augroup   -- Create/get autocommand group
local autocmd = vim.api.nvim_create_autocmd   -- Create autocommand

-- Enable spell checker for certain file types
autocmd({ "BufRead", "BufNewFile" }, {
	pattern = { "*.txt", "*.md", "*.tex" },
	command = "setlocal spell"
})

-- Highlight on yank
augroup('YankHighlight', { clear = true })
autocmd('TextYankPost', {
  group = 'YankHighlight',
  callback = function()
    vim.highlight.on_yank({ higroup = 'IncSearch', timeout = '1000' })
  end
})

-- Remove whitespace on save
autocmd('BufWritePre', {
  pattern = "",
  command = ":%s/\\s\\+$//e"
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

-- Toggle relative line numbers when changing modes
autocmd({ "BufEnter", "InsertLeave", "FocusGained", "WinEnter" }, {
	command = [[if &nu && mode() != "i" | set rnu | endif]]
})
autocmd({ "BufLeave", "InsertEnter", "FocusLost", "WinLeave" }, {
	command = [[if &nu | set nornu | endif]]
})

