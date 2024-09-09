-------------------------------------------------------------------------------
-- Autocommands
-------------------------------------------------------------------------------
local cmd = vim.cmd
local opt = vim.opt

-- Define autocommands with Lua APIs
local augroup = vim.api.nvim_create_augroup	 -- Create/get autocommand group
local autocmd = vim.api.nvim_create_autocmd	 -- Create autocommand

-- Automatically rebalance windows on vim resize
-- https://dev.to/voyeg3r/my-lazy-neovim-config-3h6o
autocmd('VimResized', {
	callback = function()
		cmd('tabdo wincmd =')
	end,
	desc = "Auto resize windows when size changes",
})

-- Set spell for typst
-- TODO: Workaround because ltex doesn't support it yet
autocmd({ "BufRead", "BufNewFile" }, {
	pattern = { "*.typ" },
	command = "setlocal spell"
})

-- Set conceal, markdown and typst
autocmd({ "FileType" }, {
	pattern = { "markdown", "typst", "text" },
	command = "set conceallevel=2"
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

-- Fix dimmed area transparency for zen-mode
autocmd("VimEnter", {
	pattern = "*",
	command = [[hi ZenBg ctermbg=NONE guibg=NONE]]
})
