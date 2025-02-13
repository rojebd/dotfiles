-- set map leader
vim.g.mapleader = " "
vim.g.maplocalleader = "\\"

-- stolen from theprimeagen
-- https://github.com/ThePrimeagen/init.lua/blob/249f3b14cc517202c80c6babd0f9ec548351ec71/lua/theprimeagen/remap.lua#L22
vim.keymap.set("x", "<leader>p", [["_dP]])

-- netrw open easily
vim.keymap.set("n", "<Leader>nr", vim.cmd.Ex)

-- buffer next and previous
vim.keymap.set("n", "<Leader>h", vim.cmd.bprevious)
vim.keymap.set("n", "<Leader>l", vim.cmd.bnext)

-- delete/close current buffer
vim.keymap.set("n", "<Leader>bd", vim.cmd.bdelete)

-- open and close folds
vim.keymap.set("n", "<C-f>", "za", { noremap = true, silent = true })

-- move selection
vim.keymap.set("v", "J", ":m '>+1<CR>gv=gv")
vim.keymap.set("v", "K", ":m '<-2<CR>gv=gv")

-- dont make J move your cursor
vim.keymap.set("n", "J", "mzJ`z")

-- quickfix and location list navigation
vim.keymap.set("n", "<C-d>", "<cmd>copen<CR>zz")
vim.keymap.set("n", "<C-c>", "<cmd>cclose<CR>zz")
vim.keymap.set("n", "<C-n>", "<cmd>cnext<CR>zz")
vim.keymap.set("n", "<C-p>", "<cmd>cprev<CR>zz")
--vim.keymap.set("n", "<leader>s", "<cmd>lnext<CR>zz")
--vim.keymap.set("n", "<leader>a", "<cmd>lprev<CR>zz")
