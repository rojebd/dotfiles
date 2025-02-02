-- use system clipboard
vim.opt.clipboard = "unnamedplus"

-- line number and relative line number
vim.opt.nu = true
vim.opt.rnu = true

-- 80 column ruler
vim.opt.colorcolumn = "80"

-- make cursor fat (block)
vim.opt.guicursor = "n-v-c-i:block"

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

-- indentation settings
--vim.opt.autoindent = true
--vim.opt.smartindent = true

-- No line wrapping
vim.opt.wrap = false

--keep search terms highlighted but do use incremental search
-- annoying to keep pressing escape to not highlight the matches again so
-- just disable it
vim.opt.hlsearch = false
vim.opt.incsearch = true

-- Stop highlighting search terms after I hit esc
-- vim.keymap.set("n", "<Esc>", ':noh<Enter>', { silent = true })

-- scrolloff and sidescroll
vim.opt.scrolloff = 8
vim.opt.sidescroll = 1
vim.opt.sidescrolloff = 5

-- set file encoding and the encoding to utf8
vim.opt.encoding = "utf-8"
vim.opt.fileencoding = "utf-8"

-- folding
vim.opt.foldenable = true
vim.opt.foldlevelstart = 99
vim.wo.foldcolumn = "1"
vim.wo.foldmethod = "expr"
vim.wo.foldexpr = "v:lua.vim.treesitter.foldexpr()"

-- undotree
vim.opt.swapfile = false
vim.opt.backup = false
vim.opt.undodir = os.getenv("HOME") .. "/.local/state/nvim/undo"
vim.opt.undofile = true

-- completion menu settings
vim.opt.completeopt = { 'menu', 'menuone', 'noselect' }

-- shortmess
vim.opt.shortmess:append "c"
