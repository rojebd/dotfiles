-- Default options:
-- require("gruvbox").setup({
--     terminal_colors = true, -- add neovim terminal colors
--     undercurl = true,
--     underline = true,
--     bold = false,
--     italic = {
--         strings = false,
--         emphasis = false,
--         comments = false,
--         operators = false,
--         folds = false,
--     },
--     strikethrough = true,
--     invert_selection = false,
--     invert_signs = false,
--     invert_tabline = false,
--     invert_intend_guides = false,
--     inverse = true, -- invert background for search, diffs, statuslines and errors
--     contrast = "",  -- can be "hard", "soft" or empty string
--     palette_overrides = {},
--     overrides = {},
--     dim_inactive = false,
--     transparent_mode = false,
-- })

-- Default options
require("modus-themes").setup({
    -- Theme comes in two styles `modus_operandi` and `modus_vivendi`
    -- `auto` will automatically set style based on background set with vim.o.background
    style = "auto",
    variant = "default",              -- Theme comes in four variants `default`, `tinted`, `deuteranopia`, and `tritanopia`
    transparent = false,              -- Transparent background (as supported by the terminal)
    dim_inactive = false,             -- "non-current" windows are dimmed
    hide_inactive_statusline = false, -- Hide statuslines on inactive windows. Works with the standard **StatusLine**, **LuaLine** and **mini.statusline**
    styles = {
        -- Style to be applied to different syntax groups
        -- Value is any valid attr-list value for `:help nvim_set_hl`
        comments = { italic = false },
        keywords = { italic = false },
        functions = { italic = false },
        variables = { italic = false },
    },

    --- You can override specific color groups to use other groups or a hex color
    --- Function will be called with a ColorScheme table
    --- Refer to `extras/lua/modus_operandi.lua` or `extras/lua/modus_vivendi.lua` for the ColorScheme table
    ----@param colors ColorScheme
    -- on_colors = function(colors) end,

    --- You can override specific highlights to use other groups or a hex color
    --- Function will be called with a Highlights and ColorScheme table
    --- Refer to `extras/lua/modus_operandi.lua` or `extras/lua/modus_vivendi.lua` for the Highlights and ColorScheme table
    ----@param highlights Highlights
    ----@param colors ColorScheme
    -- on_highlights = function(highlights, colors) end,
})

vim.opt.background = "dark"
vim.cmd.colorscheme("modus_vivendi")
