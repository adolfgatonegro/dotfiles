local options = {
	backup = false,									-- creates a backup file
	background = "dark",
	clipboard = "unnamedplus",						-- allows neovim to access the system clipboard
	cmdheight = 1,									-- more space in the neovim command line for displaying messages
	completeopt = { "menuone", "noselect" },		-- mostly just for cmp
	conceallevel = 0,								-- so that `` is visible in markdown files
	confirm = true,									-- asks to save changes instead of erroring out
	cursorline = true,								-- highlight the current line
	cursorlineopt = "screenline",					-- highlight line method
	expandtab = false,								-- convert tabs to spaces
	fileencoding = "utf-8",							-- the encoding written to a file
	guifont = "JetBrainsMono Nerd Font Mono:h9",	-- the font used in graphical neovim applications
	hlsearch = true,								-- highlight all matches on previous search pattern
	ignorecase = true,								-- ignore case in search patterns
	mouse = "a",									-- allow the mouse to be used in neovim
	number = true,									-- set numbered lines
	numberwidth = 4,								-- set number column width to 2 {default 4}
	pumheight = 10,									-- pop up menu height
	relativenumber = false,							-- set relative numbered lines
	scrolloff = 100,									-- number of lines to keep above/below cursor
	shiftwidth = 4,									-- the number of spaces inserted for each indentation
	showmode = false,								-- we don't need to see things like -- INSERT -- anymore
	showtabline = 2,								-- always show tabs
	sidescrolloff = 100,								-- number of characters to keep after cursor
	signcolumn = "yes",								-- always show the sign column, otherwise it would shift the text each time
	smartcase = true,								-- smart case
	smartindent = true,								-- make indenting smarter again
	softtabstop = 4,
	splitbelow = true,								-- force all horizontal splits to go below current window
	splitright = true,								-- force all vertical splits to go to the right of current window
	swapfile = false,								-- creates a swapfile
	tabstop = 4,									-- insert 4 spaces for a tab
	termguicolors = true,						-- set term gui colors (most terminals support this)
	timeoutlen = 1000,								-- time to wait for a mapped sequence to complete (in milliseconds)
	undofile = true,								-- enable persistent undo
	updatetime = 300,								-- faster completion (4000ms default)
	wrap = false,									-- display lines as one long line
	writebackup = false,							-- if a file is being edited by another program (or was written to file while editing with another program), it is not allowed to be edited
}

vim.opt.shortmess:append "c"

for k, v in pairs(options) do
  vim.opt[k] = v
end

vim.cmd [[set iskeyword+=-]]
vim.cmd "set whichwrap+=<,>,[,],h,l"

-- Terminal settings
vim.cmd [[
	augroup term_settings
		autocmd!
		autocmd TermOpen * setlocal nonumber
		autocmd TermOpen * startinsert
	augroup end
]]

-- Return to last edit position when opening a file
vim.cmd [[
	augroup resume_edit_position
		autocmd!
		autocmd BufReadPost *
			\ if line("'\"") > 1 && line("'\"") <= line("$") && &ft !~# 'commit'
			\ | execute "normal! g`\"zvzz"
			\ | endif
	augroup end
]]

vim.cmd [[
	augroup text_file_width
		autocmd!
		autocmd BufNewFile,BufRead *.md,*.MD,*.markdown,*.txt,*.tex setlocal textwidth=80
	augroup end
]]

