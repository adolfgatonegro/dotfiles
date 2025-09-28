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
		-- local lspconfig = require("lspconfig")

		vim.lsp.config('tinymist', {
			settings = {exportPdf = "onSave", },
		})

		vim.lsp.config('lua_ls', {
			capabilities = capabilities,
			settings = {
				Lua = {
					diagnostics = {
						globals = { "vim" },
					},
				},
			},
		})

		vim.lsp.enable('tinymist')
		vim.lsp.enable('lua_ls')
	end
},

{
    "onsails/lspkind.nvim"
  },
}
