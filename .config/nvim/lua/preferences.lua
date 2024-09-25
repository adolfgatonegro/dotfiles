-------------------------------------------------------------------------------
-- No need to re-type the same thing over and over
-------------------------------------------------------------------------------
local opt = vim.opt

-------------------------------------------------------------------------------
-- Set basic options
-------------------------------------------------------------------------------
local options = {
	autochdir = true,
	background = "dark",
	backup = true,
	clipboard = "unnamedplus",
	cmdheight = 0,
	completeopt = "menuone,longest,preview",
	confirm = true,
	expandtab = false,
	hidden = true,
	hlsearch = true,
	ignorecase = true,
	incsearch = true,
	laststatus= 3,
	linebreak = true,
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
