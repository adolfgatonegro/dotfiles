-------------------------------------------------------------------------------
-- Configure key mappings
-- Based on https://dev.to/voyeg3r/my-lazy-neovim-config-3h6o
-------------------------------------------------------------------------------
local g = vim.g

local map = function(mode, lhs, rhs, opts)
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

-------------------------------------------------------------------------------
-- Normal mode mappings
-------------------------------------------------------------------------------

k("n", "Q", "<Nop>") -- Bye-bye, Ex mode

-- Buffers
k("n", "<leader>bd", ":bd<CR>", { desc = "Delete buffer" } )
k("n", "<leader>bb", ":b<Space>", { desc = "Switch buffer" } )
k("n", "<leader>bp", ":bprevious<CR>", { desc = "Previous buffer" } )
k("n", "<leader>bn", ":bnext<CR>", { desc = "Next buffer" } )
k("n", "<leader>bs", ":new<CR>", { desc = "New horizontal split" } )
k("n", "<leader>bv", ":vnew<CR>", { desc = "New vertical split" } )

-- bufferline.nvim
k("n", "<tab>", ":BufferLineCycleNext<CR>", { desc = "Bufferine: Next buffer" } )
k("n", "<S-tab>", ":BufferLineCyclePrev<CR>", { desc = "Bufferine: Previous buffer" } )

-- Telescope.nvim
k("n", "<leader>ff", ":Telescope find_files<CR>", { desc = "Telescope: Find files" } )
k("n", "<leader>fg", ":Telescope live_grep<CR>", { desc = "Telescope: Live grep" } )
k("n", "<leader>fb", ":Telescope buffers<CR>", { desc = "Telescope: Buffers" } )

-- zen-mode.nvim
k("n", "<leader>z", ":ZenMode<CR>", { desc = "Zen Mode: Toggle" } )

-- Go to last change in current buffer
k("n", "gl", '`.', { desc = "Go to last change in current buffer" })

-- Formatting paragraphs
k("n", "f", 'gqap', { desc = "Format current paragraph" })
k("n", "F", 'vipJ', { desc = "Join all lines in current paragraph" })

-- Line navigation
k("n", "l", "<Space>")
k("n", "h", "<Backspace>")
k({"n", "v"}, "H", "^")
k({"n", "v"}, "L", "$")

-- Go to URL under cursor
k("", "gx", '<Cmd>call jobstart(["xdg-open", expand("<cfile>")], {"detach": v:true})<CR>', { desc = "Go to URL under cursor" })

-- oil.nvim
k("n", "-", "<cmd>Oil<CR>", { desc = "Open parent directory" })

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
k("n", "<leader>th", ":set hlsearch!<CR>", { desc = "Toggle highlight for last search term" } )
k("n", "<leader>tw", ":set wrap!<CR>", { desc = "Toggle line wrapping" } )

-- Compiler
k("n", "<leader>c", ":w! | silent!  !compiler \"%:p\"<CR>", { desc = "Compile document" })
k("n", "<leader>p", ":silent! !opout \"%:p\"<CR>", { desc = "Open compiled document" })

-- Write and exit
k("n", "<C-q>", ":x<CR>", { desc = "Write and exit" } )
k("n", "<C-s>", ":up<CR>", { desc = "Write file" } )

-------------------------------------------------------------------------------
-- Visual mappings
-------------------------------------------------------------------------------

-- Paste replace visual selection without copying it
k("v", "p", '"_dP')

-- Search for selection
k("v", "*", "\"zy:let @/=@z<C-r>n<CR>", { desc = "Search for selection" } )

-------------------------------------------------------------------------------
-- Visual Block mappings
-------------------------------------------------------------------------------

-- Formatting paragraphs
k("x", "f", 'gq', { desc = "Format current paragraph" })

-- Move selected block up/down
k("x", "J", ":m '>+1<CR>gv-gv")
k("x", "K", ":m '<-2<CR>gv-gv")

-- Indent selected block
k("x", ">", ">gv")
k("x", "<", "<gv")