return {
	'stevearc/oil.nvim',
	---@module 'oil'
	---@type oil.SetupOpts
  dependencies = { "nvim-tree/nvim-web-devicons" },

	opts = {
		delete_to_trash = true,
		skip_confirm_for_simple_edits = true,
		columns = {
			"icon",
			"permissions",
			"size",
			"mtime",
		},
		float = {
			padding = 4,
		},
		 keymaps = {
			["q"] = "actions.close",
		},
	},
}
