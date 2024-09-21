local function getWords()
  if vim.bo.filetype == "md" or vim.bo.filetype == "txt" or vim.bo.filetype == "markdown" or vim.bo.filetype == "typst" or vim.bo.filetype == "tex" or vim.bo.filetype == "plaintex" then
      return " " .. tostring(vim.fn.wordcount().words)
  else
    return ""
  end
end

return {
	"nvim-lualine/lualine.nvim",
	dependencies = { 'nvim-tree/nvim-web-devicons' },
	event = "VeryLazy",

	config = function ()
		local lazy_status = require("lazy.status")
		require('lualine').setup({
			options = {
				icons_enabled = true,
				theme = "tokyonight",
				component_separators = '',
				section_separators = '',
			},
			always_divide_middle = true,
			globalstatus = true,
			sections = {
				lualine_a = { "mode" },
				lualine_b = {
					"branch",
					"diff",
					-- '"🖧  " .. tostring(#vim.tbl_keys(vim.lsp.buf_get_clients()))',
					{ "diagnostics", sources = { "nvim_diagnostic" } },
				},
				lualine_c = { "%{PencilMode()}", "filename" },
				lualine_x = {
					{ lazy_status.updates, cond = lazy_status.has_updates },
					"encoding",
					"fileformat",
					"filetype",
				},
				lualine_y = {
					{ getWords },
					'(vim.bo.expandtab and "󱁐" or "󰌒 ") .. " " .. vim.bo.shiftwidth',
				},
				lualine_z = {
					"location",
					"progress",
				},
			},
			inactive_sections = {
				lualine_c = { "filename" },
			},
		})
	end,
}
