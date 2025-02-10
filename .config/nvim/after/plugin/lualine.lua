--local function lsp_status()
--    local val = require("lsp-status").status()
--    return val
--end

require("lualine").setup {
    options = {
        icons_enabled = false,
        -- theme = "gruvbox_dark",
    },
    --sections = { lualine_x = { 'encoding', 'fileformat', 'filetype', lsp_status }, }
    sections = { lualine_x = { 'encoding', 'fileformat', 'filetype' }, }
}
