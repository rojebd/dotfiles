-- For C (clang-format)
local function format_and_save_c()
    local filetype = vim.bo.filetype
    if filetype == "c" or filetype == "cpp" then
        vim.cmd('silent !clang-format -i --style=file:/home/roniell/coding/c/scripts-and-stuff/clang-format-global %')
        vim.cmd("checktime")
    end
end

vim.api.nvim_create_autocmd('BufWritePost', {
  pattern = '*.c,*.cpp',
  callback = format_and_save_c,
})
