-- use system clipboard
vim.opt.clipboard = "unnamedplus"

-- line number and relative line number
vim.opt.nu = true
vim.opt.rnu = true

-- highlight the current line number
vim.opt.cursorline = true
vim.opt.cursorlineopt = "number"

-- Restore terminal cursor to bar
vim.api.nvim_create_autocmd("VimLeave", {
  pattern = "*",
  command = "set guicursor=a:ver25-blinkon0",
})

-- use 4 spaces for indentation
vim.opt.tabstop = 4
vim.opt.softtabstop = 4
vim.opt.shiftwidth = 4
vim.opt.expandtab = true

-- No line wrapping
vim.opt.wrap = false

--keep search terms highlighted but do use incremental search
vim.opt.hlsearch = true 
vim.opt.incsearch = true

-- Stop highlighting search terms after I hit esc
vim.keymap.set("n", "<Esc>", ':noh<Enter>', { silent = true})

-- scrolloff and sidescroll
vim.opt.scrolloff = 8
vim.opt.sidescroll = 1
vim.opt.sidescrolloff = 5

-- set file encoding and the encoding to utf8
vim.opt.encoding = "utf-8"
vim.opt.fileencoding = "utf-8"
