local conform = require("conform")

conform.setup({
  formatters_by_ft = {
    c = { "clang-format" },
  },
  format_on_save = {
      timeout_ms = 500,
      lsp_format = "fallback",
  },
})

conform.formatters.clang_format = {
    prepend_args = { "--style=file" },
}
