local status_ok, _ = pcall(require, "lspconfig")
if not status_ok then
	return
end

require("gatonegro.lsp.lsp-installer")
require("gatonegro.lsp.handlers").setup()
