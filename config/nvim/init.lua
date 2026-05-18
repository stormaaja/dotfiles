-- Layout

require "paq" {
  "savq/paq-nvim", -- Let Paq manage itself
  "neovim/nvim-lspconfig",
  { "lervag/vimtex", opt = true }, -- Use braces when passing options
  { 'nvim-treesitter/nvim-treesitter', build = ':TSUpdate' },
  "folke/tokyonight.nvim",
  "airblade/vim-gitgutter",
  { "nvim-telescope/telescope.nvim", version = "*" },
	"nvim-lua/plenary.nvim",
  { "nvim-telescope/telescope-fzf-native.nvim", build = "make" },
  "f-person/git-blame.nvim"
}

vim.o.number = true
vim.o.hlsearch = false
vim.o.signcolumn = 'yes'
vim.o.wrap = true
vim.o.linebreak = true
vim.cmd.colorscheme('tokyonight')
-- Tabs
vim.opt.expandtab = true
vim.o.tabstop = 2
vim.o.shiftwidth = 2
vim.opt.softtabstop = 2
-- Search
vim.o.smartcase = true
vim.o.ignorecase = true
-- Copy to system clipboard
vim.o.clipboard = "unnamedplus"

-- Telescope config
local builtin = require('telescope.builtin')
vim.keymap.set('n', '<leader>ff', builtin.find_files, { desc = 'Telescope find files' })
vim.keymap.set('n', '<leader>fg', builtin.live_grep, { desc = 'Telescope live grep' })
vim.keymap.set('n', '<leader>fb', builtin.buffers, { desc = 'Telescope buffers' })
vim.keymap.set('n', '<leader>fh', builtin.help_tags, { desc = 'Telescope help tags' })
vim.keymap.set('n', '<leader>gd', builtin.lsp_definitions, { desc = 'Telescope find definitions' })
vim.keymap.set('n', '<leader>gr', builtin.lsp_references, { desc = 'Telescope find references' })

