require("nvim-autopairs").setup {}

local Rule = require('nvim-autopairs.rule')
local npairs = require('nvim-autopairs')
local cond = require('nvim-autopairs.conds')

-- add semicolon for hare code because without the semicolon
-- autoindentation even with treesitter does not work
npairs.add_rules({
    Rule("{", "};", "hare")
})
