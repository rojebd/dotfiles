require 'nvim-treesitter.configs'.setup {
    -- A list of parser names, or "all" (the listed parsers MUST always be installed)
    -- comment is needed to define custom highlight groups for comments see
    -- below this file for more info
    ensure_installed = { "comment", "lua", "python", "c" },

    -- Install parsers synchronously (only applied to `ensure_installed`)
    sync_install = false,

    -- Automatically install missing parsers when entering buffer
    -- Recommendation: set to false if you don't have `tree-sitter` CLI installed locally
    auto_install = false,

    -- List of parsers to ignore installing (or "all")
    -- ignore_install = { "javascript" },

    ---- If you need to change the installation directory of the parsers (see -> Advanced Setup)
    -- parser_install_dir = "/some/path/to/store/parsers", -- Remember to run vim.opt.runtimepath:append("/some/path/to/store/parsers")!

    indent = {
        enable = true
    },

    highlight = {
        enable = true,
        -- Setting this to true will run `:h syntax` and tree-sitter at the same time.
        -- Set this to `true` if you depend on 'syntax' being enabled (like for indentation).
        -- Using this option may slow down your editor, and you may see some duplicate highlights.
        -- Instead of true it can also be a list of languages
        additional_vim_regex_highlighting = false,
    },
}

-- define custom highlight for hare (highlight ! and ? operators)
--vim.api.nvim_create_autocmd("FileType", {
--    pattern = "hare",
--    callback = function()
--        vim.api.nvim_set_hl(0, "@operator", { link = "GruvboxRed", bold = true })
--    end
--})

-- Define custom highlighting for TODO, FIXME, XXX, NOTE:
-- to all be gruvbox red
vim.api.nvim_set_hl(0, "@comment.error", { link = "GruvboxRed", bold = true })
vim.api.nvim_set_hl(0, "@comment.note", { link = "GruvboxRed", bold = true })
vim.api.nvim_set_hl(0, "@comment.todo", { link = "GruvboxRed", bold = true })
vim.api.nvim_set_hl(0, "@comment.warning", { link = "GruvboxRed", bold = true })
