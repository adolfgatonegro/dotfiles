local function getWords()
  if vim.bo.filetype == "md" or vim.bo.filetype == "txt" or vim.bo.filetype == "markdown" or vim.bo.filetype == "typst" then
      return tostring(vim.fn.wordcount().words) .. " words"
  else
    return ""
  end
end

return {
	'nvim-lualine/lualine.nvim',

	config = function ()
		require('lualine').setup({
			options = {
				theme = 'tokyonight',
				component_separators = '',
				section_separators = '',
			},
			sections = {
				lualine_c = {"%{PencilMode()}"},
				lualine_x = { "encoding", "fileformat", "filetype", { getWords } },
			},
			inactive_sections = {
				lualine_c = {""},
			},
		})
	end,

	dependencies = { 'nvim-tree/nvim-web-devicons' }
}
