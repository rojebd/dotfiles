-- set map leader
vim.g.mapleader = " "
vim.g.maplocalleader = "\\"

-- netrw open easily
vim.keymap.set("n", "<Leader>nr", vim.cmd.Ex)

-- buffer next and previous
vim.keymap.set("n", "<Leader>h", vim.cmd.bprevious)
vim.keymap.set("n", "<Leader>l", vim.cmd.bnext)

-- delete/close current buffer
vim.keymap.set("n", "<Leader>bd", vim.cmd.bdelete)

-- open and close folds
vim.keymap.set("n", "<C-f>", "za", { noremap = true, silent = true })
