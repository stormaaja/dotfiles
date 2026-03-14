-- Layout

require "paq" {
    "savq/paq-nvim", -- Let Paq manage itself
    "neovim/nvim-lspconfig",
    { "lervag/vimtex", opt = true }, -- Use braces when passing options
    { 'nvim-treesitter/nvim-treesitter', build = ':TSUpdate' },
		"folke/tokyonight.nvim",
}

vim.o.number = true
vim.o.hlsearch = false
vim.o.signcolumn = 'yes'
vim.cmd.colorscheme('tokyonight')
-- Tabs
vim.o.tabstop = 2
vim.o.shiftwidth = 2
-- Search
vim.o.smartcase = true
vim.o.ignorecase = true
