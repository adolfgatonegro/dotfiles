return {
  'stevearc/oil.nvim',
  ---@module 'oil'
  ---@type oil.SetupOpts

	opts = {
		delete_to_trash = true,
		skip_confirm_for_simple_edits = true,
		float = {
			padding = 4,
		},
		 keymaps = {
			["q"] = "actions.close",
		},
	},

  dependencies = { "nvim-tree/nvim-web-devicons" },
}
