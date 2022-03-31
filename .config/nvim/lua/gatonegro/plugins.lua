local fn = vim.fn

-- Automatically install packer
local install_path = fn.stdpath "data" .. "/site/pack/packer/start/packer.nvim"
if fn.empty(fn.glob(install_path)) > 0 then
	PACKER_BOOTSTRAP = fn.system {
		"git",
		"clone",
		"--depth",
		"1",
		"https://github.com/wbthomason/packer.nvim",
		install_path,
	}
	print "Installing packer close and reopen Neovim..."
	vim.cmd [[packadd packer.nvim]]
end

-- Autocommand that reloads neovim whenever you save the plugins.lua file
vim.cmd [[
  augroup packer_user_config
    autocmd!
    autocmd BufWritePost plugins.lua source <afile> | PackerSync
  augroup end
]]

-- Use a protected call so we don't error out on first use
local status_ok, packer = pcall(require, "packer")
if not status_ok then
  return
end

-- Plugins
return packer.startup(function(use)
	--- Packer
	use "wbthomason/packer.nvim"

	-- Check nvim startup time
	-- use "dstein64/vim-startuptime"

	--- Colour scheme
	use "adolfgatonegro/gatoneon.nvim"

	-- CMP plugins
	use "hrsh7th/nvim-cmp"
	use "hrsh7th/cmp-buffer"
	use "hrsh7th/cmp-path"
	use "hrsh7th/cmp-cmdline"
	use "saadparwaiz1/cmp_luasnip"
	use "hrsh7th/cmp-nvim-lsp"

	--- Snippets
	use "L3MON4D3/LuaSnip"
	use "rafamadriz/friendly-snippets"

	--- LSP
	use "neovim/nvim-lspconfig"
	use "williamboman/nvim-lsp-installer"

	--- TreeSitter
	use {
		"nvim-treesitter/nvim-treesitter"
		-- run = ":TSUpdate",
	}
	use "p00f/nvim-ts-rainbow"

	--- Others
	use {"junegunn/goyo.vim", config = function() vim.cmd[[let g:goyo_width = 84]] end}
	use "jiangmiao/auto-pairs"
	use "tpope/vim-commentary"
	use "ap/vim-css-color"
	use "vifm/vifm.vim"
	use {"itchyny/lightline.vim", config = function() vim.cmd[[let g:lightline = {'colorscheme': 'GatoNeon'}]] end}
	use {"lervag/vimtex", config = function() vim.cmd[[let g:vimtex_view_method = 'zathura']] end}
	use {
	    "vimwiki/vimwiki",
		branch = "dev",
	    config = function()
			vim.g.vimwiki_global_ext = 0
			vim.g.vimwiki_markdown_link_ext = 1
			-- vim.g.vimwiki_listsyms = "✗○◐●✓"
	        vim.g.vimwiki_list = {
	            {
	                path = "~/repos/wiki",
	                syntax = "markdown",
	                ext = ".md",
					auto_toc = 1,
					auto_tags = 1,
					automatic_nested_syntaxes = 1,
	            }
	        }
	    end
	}


  -- Sync Packer after all plugins are loaded
  -- if PACKER_BOOTSTRAP then
  --   require("packer").sync()
  -- end
end)
