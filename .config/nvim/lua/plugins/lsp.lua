return {
	{
		"williamboman/mason.nvim",

		config = function()
			require("mason").setup()
		end
	},

{
	"neovim/nvim-lspconfig",

	config = function()
		local capabilities = require("cmp_nvim_lsp").default_capabilities()
		local lspconfig = require("lspconfig")

		-- lspconfig.typst_lsp.setup{
		-- 	capabilities = capabilities,
		-- 	-- exportPdf = "onSave"
		-- }

		lspconfig.tinymist.setup {
			settings = {
				exportPdf = "onType",
			},
		}

		lspconfig.lua_ls.setup{
			capabilities = capabilities,
			settings = {
				Lua = {
					diagnostics = {
						globals = { "vim" },
					},
				},
			},
		}

		lspconfig.clangd.setup{
			capabilities = capabilities,
		}

		-- ltex gets horribly confused by typst, disable for now
  --
		-- lspconfig.ltex.setup{
		-- 	capabilities = capabilities,
		-- 	filetypes = {
		-- 		"latex",
		-- 		"typst",
		-- 		"typ",
		-- 		"bib",
		-- 		"markdown",
		-- 		"plaintex",
		-- 		"tex"
		-- 	},
		-- 	settings = {
		-- 		ltex = {
		-- 			language = "en-GB",
		-- 			enabled = {
		-- 				"latex",
		-- 				"typst",
		-- 				"typ",
		-- 				"bib",
		-- 				"markdown",
		-- 				"plaintex",
		-- 				"tex"
		-- 			},
		-- 		}
		-- 	}
		-- }
	end
},

{
    "onsails/lspkind.nvim"
  },
}
