-- lua language server (lua)
require("lspconfig").lua_ls.setup{
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

-- pylyzer (python)
require("lspconfig").pylyzer.setup{}

-- ruff (python)
require("lspconfig").ruff.setup{
    init_options = {
        settings = {
            lineLength = 80
        }
    }
}

-- auto format on save
vim.api.nvim_create_autocmd("BufWritePre", {
    pattern = "*",
    -- if it fails or finds no lsp server to format buffer just silence it
    command = "silent! lua vim.lsp.buf.format()" 
})

-- Disable lsp syntax highlighting (I use treesitter instead)
vim.api.nvim_create_autocmd("LspAttach", {
    callback = function(args)
        local client = vim.lsp.get_client_by_id(args.data.client_id)
        client.server_capabilities.semanticTokensProvider = nil
    end,
});
