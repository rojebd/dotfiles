-- Bootstrap lazy.nvim
local lazypath = vim.fn.stdpath("data") .. "/lazy/lazy.nvim"
if not (vim.uv or vim.loop).fs_stat(lazypath) then
    local lazyrepo = "https://github.com/folke/lazy.nvim.git"
    local out = vim.fn.system({ "git", "clone", "--filter=blob:none", "--branch=stable", lazyrepo, lazypath })
    if vim.v.shell_error ~= 0 then
        vim.api.nvim_echo({
            { "Failed to clone lazy.nvim:\n", "ErrorMsg" },
            { out,                            "WarningMsg" },
            { "\nPress any key to exit..." },
        }, true, {})
        vim.fn.getchar()
        os.exit(1)
    end
end
vim.opt.rtp:prepend(lazypath)

-- setup lazy.nvim
require("lazy").setup({
    spec = {
        -- colorscheme
        {
            "ellisonleao/gruvbox.nvim",
            priority = 1000,
            config = true,
            opts = ...
        },

        -- treesitter
        {
            "nvim-treesitter/nvim-treesitter",
            build = ":TSUpdate"
        },

        -- Fuzzy finder
        {
            'nvim-telescope/telescope.nvim',
            tag = '0.1.8',
            dependencies = { 'nvim-lua/plenary.nvim' }
        },

        -- lualine
        { "nvim-lualine/lualine.nvim" },

        -- harpoon
        { "ThePrimeagen/harpoon" },

        -- indent lines
        { "lukas-reineke/indent-blankline.nvim", main = "ibl" },

        -- lsp
        { "neovim/nvim-lspconfig" },

        -- autopairs
        {
            "windwp/nvim-autopairs",
            event = "InsertEnter"
        },

        -- vim sandwich (surround)
        { "machakann/vim-sandwich" },

        -- undotree
        { "mbbill/undotree" },

        -- lsp status
        { "nvim-lua/lsp-status.nvim" },

        -- diagnostics at top right
        {
            "dgagn/diagflow.nvim",
            event = 'LspAttach',
        },

        -- snippets
        { "L3MON4D3/LuaSnip" },

        -- completion
        {
            "hrsh7th/nvim-cmp",
            dependencies = {
                "hrsh7th/cmp-nvim-lsp",
                "hrsh7th/cmp-buffer",
                "hrsh7th/cmp-path",
                "saadparwaiz1/cmp_luasnip",
            },
        },

        -- signature help
        { "hrsh7th/cmp-nvim-lsp-signature-help" }
    },

    -- configure any other settings here. See the documentation for more details.
    -- colorscheme that will be used when installing plugins.
    install = { colorscheme = { "gruvbox" } },

    -- automatically check for plugin updates
    checker = { enabled = false },

})
