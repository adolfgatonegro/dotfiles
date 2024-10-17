local g = vim.g

local map = function(mode, lhs, rhs, opts)
	local options = { noremap = true, silent = true }
	if opts then
		options = vim.tbl_extend("force", options, opts)
	end
	vim.keymap.set(mode, lhs, rhs, options)
end

local k = map

-- Unbind <space> before defining it as leader
k("", "<Space>", "<Nop>")
-- Define leader and localleader (<space> and \)
g.mapleader = " "
g.maplocalleader = "\\"

-- NORMAL mode mappings
k("n", "Q", "<Nop>") -- Disable Ex mode

-- Buffers
k("n", "<leader>b",  "<Nop>",          { desc = "Buffers" } )
k("n", "<leader>bk", ":bd<CR>",        { desc = "Delete current buffer" } )
k("n", "<leader>bn", ":bnext<CR>",     { desc = "Goto next buffer" } )
k("n", "<leader>bp", ":bprevious<CR>", { desc = "Goto previous buffer" } )

-- Compiler
k("n", "<leader>c",  "<Nop>",                                { desc = "Compiler" } )
k("n", "<leader>cc", ":w! | silent!  !compiler \"%:p\"<CR>", { desc = "Compile document" })
k("n", "<leader>cp", ":silent! !opout \"%:p\"<CR>",          { desc = "Open compiled document" })

-- Find files
k("n", "<leader>.", ":find ", { desc = "Find files"})
k("n", "<leader>f", "<Nop> ", { desc = "Find..."})

-- Line navigation
k("n", "h", "<Backspace>")
k("n", "j", "<Down>", { remap = true })
k("n", "k", "<Up>", { remap = true })
k("n", "l", "<Space>")

-- oil.nvim
k("n", "-", "<cmd>Oil<CR>", { desc = "Open Oil" })

-- Splits - better navigation
k("n", "<leader>w",  "<Nop>",     { desc = "Windows" } )
k("n", "<leader>wh", "<C-w>h",    { desc = "Window left" } )
k("n", "<leader>wj", "<C-w>j",    { desc = "Window down" } )
k("n", "<leader>wk", "<C-w>k",    { desc = "Window up" } )
k("n", "<leader>wl", "<C-w>l",    { desc = "Window right" } )
k("n", "<leader>wc", "<C-w>c",    { desc = "Window close" } )
k("n", "<leader>ws", ":new<CR>",  { desc = "New horizontal split" } )
k("n", "<leader>wv", ":vnew<CR>", { desc = "New vertical split" } )

-- Splits - resize with arrow keys
k("n", "<C-Up>", ":res +2<CR>")
k("n", "<C-Down>", ":res -2<CR>")
k("n", "<C-Left>", ":vert res +2<CR>")
k("n", "<C-Right>", ":vert res -2<CR>")

-- Telescope
k("n", "<leader>bb", "<cmd>Telescope buffers<CR>", { desc = "Telescope: Buffers"})
k("n", "<leader>ff", "<cmd>Telescope find_files<CR>", { desc = "Telescope: Find files"})
k("n", "<leader>fg", "<cmd>Telescope live_grep<CR>", { desc = "Telescope: Live grep"})
k("n", "<leader>fr", "<cmd>Telescope oldfiles<cr>", { desc = "Recent files"})

-- Toggle key bindings
k("n", "<leader>t",  "<Nop>",              { desc = "Toggle" } )
k("n", "<leader>th", ":set hlsearch!<CR>", { desc = "Highlight for last search term" } )
k("n", "<leader>tw", ":set wrap!<CR>",     { desc = "Line wrapping" } )

-- Toggle Stay-Centered (disabled)
-- k({"n", "v"}, "<leader>tc", function()
-- 	local stay = require("stay-centered")
-- 	stay.toggle()
-- end, { desc = 'Toggle centred cursor' })

-- Toggle Zen-Mode
k("n", "<leader>tz", ":ZenMode<CR>", { desc = "Zen-Mode" } )

-- Go to last change in current buffer
k("n", "gl", '`.', { desc = "Go to last change in current buffer" } )
-- Go to URL under cursor
k("", "gx", '<Cmd>call jobstart(["xdg-open", expand("<cfile>")], {"detach": v:true})<CR>', { desc = "Go to URL under cursor" } )
-- Duplicate line and keep cursor in same colum
k("n", "<leader>,", "yymmp`mj", { desc = "Duplicate current line, keep cursor column" } )

-- Write and exit
k("n", "<C-q>", ":x<CR>", { desc = "Write and exit" } )
k("n", "<C-s>", ":up<CR>", { desc = "Write file" } )

-- Visual mappings
-- Paste replace visual selection without copying it
k("v", "p", '"_dP', { desc = "Paste replace without yanking" } )

-- Search for selection
k("v", "*", "\"zy:let @/=@z<C-r>n<CR>", { desc = "Search for selection" } )

-- Visual Block mappings
-- Move selected block up/down
k("x", "<C-j>", ":m '>+1<CR>gv-gv")
k("x", "<C-k>", ":m '<-2<CR>gv-gv")

-- Indent selected block
k("x", ">", ">gv")
k("x", "<", "<gv")
