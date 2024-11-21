local lsp = require("lspconfig")

-- enable inlay_hints
vim.lsp.inlay_hint.enable()

-- on_attach function
local on_attach = function(_, bufnr)
    local opts = { buffer = bufnr, remap = false }

    -- lsp mappings
    vim.keymap.set("n", "gd", function() vim.lsp.buf.definition() end, opts)
    vim.keymap.set("n", "gi", function() vim.lsp.buf.implementation() end, opts)
    vim.keymap.set("n", "rn", function() vim.lsp.buf.rename() end, opts)
    vim.keymap.set("n", "K", function() vim.lsp.buf.hover() end, opts)
    vim.keymap.set("n", "ca", function() vim.lsp.buf.code_action() end, opts)
    vim.keymap.set("n", "gr", function() vim.lsp.buf.references() end, opts)
    vim.keymap.set("n", "ws", function() vim.lsp.buf.workspace_symbol() end, opts)
    vim.keymap.set("i", "<C-h>", function() vim.lsp.buf.signature_help() end, opts)

    -- diagnostics mappings
    vim.keymap.set("n", "[d", function() vim.diagnostic.goto_next() end, opts)
    vim.keymap.set("n", "]d", function() vim.diagnostic.goto_prev() end, opts)
    vim.keymap.set("n", "df", function() vim.diagnostic.open_float() end, opts)
end

-- floating windows have border (example doc window)
--local border = {
--    { "🭽", "FloatBorder" },
--    { "▔", "FloatBorder" },
--    { "🭾", "FloatBorder" },
--    { "▕", "FloatBorder" },
--    { "🭿", "FloatBorder" },
--    { "▁", "FloatBorder" },
--    { "🭼", "FloatBorder" },
--    { "▏", "FloatBorder" },
--}
-- overrides the border globally
--local orig_util_open_floating_preview = vim.lsp.util.open_floating_preview
--function vim.lsp.util.open_floating_preview(contents, syntax, opts, ...)
--    opts = opts or {}
--    opts.border = opts.border or border
--    return orig_util_open_floating_preview(contents, syntax, opts, ...)
--end

-- lua language server (lua)
lsp.lua_ls.setup {
    on_attach = on_attach,
    settings = {
        Lua = {
            diagnostics = {
                -- Get the language server to recognize the `vim` global
                globals = {
                    'vim',
                    'require'
                },
            },
            workspace = {
                -- Make the server aware of Neovim runtime files
                library = vim.api.nvim_get_runtime_file("", true),
            },
            -- Do not send telemetry data containing a randomized but unique identifier
            telemetry = {
                enable = false,
            },
        },
    },
}

-- use basedpyright lsp hover doc function instead of pylsp
-- pylsp (python)
lsp.pylsp.setup {
    on_attach = function(client, bufnr)
        client.server_capabilities.hoverProvider = false
        on_attach(client, bufnr)
    end
}

-- basedpyright (python)
lsp.basedpyright.setup {
    on_attach = on_attach,
    settings = {
        basedpyright = {
            analysis = {
                typeCheckingMode = "basic",

                -- NOTE: We do not need reportUnknownMemberType to false
                -- since typeCheckingMode set to basic does not report it
                -- as an error
                reportUnknownMemberType = false,
            },
        },
    },
}

-- ruff (python)
lsp.ruff.setup {
    on_attach = on_attach,
    init_options = {
        settings = {
            lineLength = 80
        }
    }
}

-- auto format on save
vim.api.nvim_create_autocmd("BufWritePre", {
    pattern = "*",
    -- no need to silence this  since neovim overwrites the output
    -- with the written file output
    callback = function()
        vim.lsp.buf.format()
    end
})

-- basedpyright seems to be good at this I don't know about other LSP's tho
-- maybe in the future only enable this for python?

-- Disable lsp syntax highlighting (I use treesitter instead)
--vim.api.nvim_create_autocmd("LspAttach", {
--    callback = function(args)
--        local client = vim.lsp.get_client_by_id(args.data.client_id)
--        client.server_capabilities.semanticTokensProvider = nil
--    end,
--});
