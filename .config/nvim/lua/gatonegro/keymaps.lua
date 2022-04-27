local opts = { noremap = true, silent = true }

local term_opts = { silent = true }

local expr_opts = { silent = true, expr = true }
-- Shorten function name
local keymap = vim.api.nvim_set_keymap

--Remap leader
keymap("", "`", "<Nop>", opts)
vim.g.mapleader = "`"
vim.g.maplocalleader = "`"

-- Modes
--   normal_mode = "n",
--   insert_mode = "i",
--   visual_mode = "v",
--   visual_block_mode = "x",
--   term_mode = "t",
--   command_mode = "c",

-- Normal --
-- Better window navigation
keymap("n", "<C-h>", "<C-w>h", opts)
keymap("n", "<C-j>", "<C-w>j", opts)
keymap("n", "<C-k>", "<C-w>k", opts)
keymap("n", "<C-l>", "<C-w>l", opts)

-- Resize with arrows
keymap("n", "<C-Up>", ":resize +2<CR>", opts)
keymap("n", "<C-Down>", ":resize -2<CR>", opts)
keymap("n", "<C-Left>", ":vertical resize -2<CR>", opts)
keymap("n", "<C-Right>", ":vertical resize +2<CR>", opts)

-- Navigate tabs
keymap("n", "<C-l>", ":tabnext<CR>", opts)
keymap("n", "<C-h>", ":tabprevious<CR>", opts)
keymap("n", "<leader>t", ":tabnew<CR>", opts)

-- Navigate buffers
keymap("n", "<S-l>", ":bnext<CR>", opts)
keymap("n", "<S-h>", ":bprevious<CR>", opts)

-- Save/exit buffer
keymap("n", "<C-w>", ":update<CR>", opts)
keymap("n", "<leader>q", ":x<CR>", opts)
keymap("n", "<leader>Q", ":qa<CR>", opts)

-- Insert --
-- Press jk fast to enter
keymap("i", "jk", "<ESC>", opts)

-- Visual --
-- Stay in indent mode
keymap("v", "<", "<gv", opts)
keymap("v", ">", ">gv", opts)

-- Move text up and down
keymap("v", "<A-k>", ":m .-2<CR>==", opts)
keymap("v", "<A-j>", ":m .+1<CR>==", opts)
keymap("v", "p", '"_dP', opts)

-- Visual Block --
-- Move text up and down
keymap("x", "J", ":move '>+1<CR>gv-gv", opts)
keymap("x", "K", ":move '<-2<CR>gv-gv", opts)

-- Terminal --
-- Better terminal navigation
keymap("t", "<C-h>", "<C-\\><C-N><C-w>h", term_opts)
keymap("t", "<C-j>", "<C-\\><C-N><C-w>j", term_opts)
keymap("t", "<C-k>", "<C-\\><C-N><C-w>k", term_opts)
keymap("t", "<C-l>", "<C-\\><C-N><C-w>l", term_opts)

-- Vifm --
keymap("n", "<leader>vf", ":Vifm<CR>", opts)
keymap("n", "<leader>vs", ":VsplitVifm<CR>", opts)
keymap("n", "<leader>sp", ":SplitVifm<CR>", opts)
keymap("n", "<leader>dv", ":DiffVifm<CR>", opts)
keymap("n", "<leader>tv", ":TabVifm<CR>", opts)

-- Toggle search highlighting --
keymap("n", "<leader>hl", "(&hls && v:hlsearch ? ':nohls' : ':set hls').\"\n\"", expr_opts)

-- Toggle spellcheck for English and Spanish --
keymap("n", "<F4>", ":setlocal spell! spelllang=es_mx<CR>", opts)
keymap("n", "<F5>", ":setlocal spell! spelllang=en_gb<CR>", opts)

-- Run Python file --
vim.cmd [[
	autocmd FileType python nmap <leader>b :terminal python %<CR>
]]

-- LuaSnip snippet choice navigation
keymap("i", "<C-e>", "<Plug>luasnip-next-choice", {})
keymap("s", "<C-e>", "<Plug>luasnip-next-choice", {})

-- Toggle word wrap
keymap("n", "<C-x>", ":set wrap!<CR>", opts)
