local builtin = require("telescope.builtin")

local function find_files()
    builtin.find_files({
        layout_strategy = "vertical",
        layout_config = {
            width = 0.5,
        },
    })
end

local function find_buffers()
    builtin.buffers({
        layout_strategy = "vertical",
        layout_config = {
            width = 0.5,
        },
    })
end

local function find_git_files()
    builtin.git_files({
        layout_strategy = "vertical",
        layout_config = {
            width = 0.5,
        },
    })
end

vim.keymap.set('n', '<leader>ff', find_files, { desc = 'Telescope find files' })
--vim.keymap.set('n', '<leader>fg', builtin.live_grep, { desc = 'Telescope live grep' })
vim.keymap.set('n', '<leader>fb', find_buffers, { desc = 'Telescope buffers' })
vim.keymap.set('n', '<leader>fG', find_git_files, { desc = 'Telescope git find files' })
vim.keymap.set('n', '<leader>fm', builtin.marks, { desc = 'Telescope find marks' })
--vim.keymap.set('n', '<leader>fs', function()
    --builtin.grep_string({ search = vim.fn.input("Grep > ") })
--end)
--vim.keymap.set('n', '<leader>fh', builtin.help_tags, { desc = 'Telescope help tags' })

require("telescope").setup {
    defaults = {
        file_ignore_patterns = { "stuff", "dist", "lib", "__pycache__", "%.o", "%.hi" },
    },
}
