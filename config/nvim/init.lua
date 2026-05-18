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
