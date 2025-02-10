function open_quickfix_window()
  local window_id = vim.api.nvim_get_current_win()
  vim.cmd('botright copen')
  vim.api.nvim_set_current_win(window_id)
end

vim.api.nvim_create_user_command('Grep', function(opts)
  local args = opts.args
  -- Run the ripgrep command and capture output as a list
  local result = vim.fn.systemlist("rg --color never --column --line-number --no-heading --ignore --no-require-git --hidden " .. args)

  -- Check if the result is empty
  if #result == 0 then
    vim.api.nvim_err_writeln("No results found.")
    return
  end

  -- Set the quickfix list with the result
  vim.fn.setqflist({}, ' ', { title = 'Grep Results', lines = result })
  
  -- Redraw the screen and open the quickfix window
  vim.cmd('redraw!')
  open_quickfix_window()
end, { nargs = '+' })

-- Set grepformat option
vim.o.grepformat = "%f:%l:%c:%m"
vim.o.grepformat = vim.o.grepformat .. "%-GNo\\ files\\ were\\ searched\\,\\ which\\ means\\ ripgrep\\ probably\\ applied\\ a\\ filter\\ you\\ didn\\'t\\ expect\\.\\ Try\\ running\\ again\\ with\\ --debug."
vim.o.grepformat = vim.o.grepformat .. "%-GNo\\ files\\ were\\ searched\\,\\ which\\ means\\ ripgrep\\ probably\\ applied\\ a\\ filter\\ you\\ didn\\'t\\ expect."
vim.o.grepformat = vim.o.grepformat .. "%-GRunning\\ with\\ --debug\\ will\\ show\\ why\\ files\\ are\\ being\\ skipped."

vim.o.grepprg = "rg --color never --column --line-number --no-heading --ignore --no-require-git --hidden"

if vim.fn.has('unix') == 1 then
  vim.o.grepprg = vim.o.grepprg .. " </dev/null"
end

-- Map <Leader>a to :Grep command in normal mode
vim.api.nvim_set_keymap('n', '<Leader>fg', ':Grep ', { noremap = true, silent = true })
