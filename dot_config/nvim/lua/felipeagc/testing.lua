vim.pack.add({
    "https://github.com/vim-test/vim-test",
    -- "https://github.com/nvim-lua/plenary.nvim",
    -- "https://github.com/nvim-neotest/nvim-nio",
    -- "https://github.com/nvim-neotest/neotest",
    -- "https://github.com/nvim-neotest/neotest-python",
    -- "https://github.com/jfpedroza/neotest-elixir",
})

vim.g["test#strategy"] = "neovim_sticky"
vim.g["test#neovim#term_position"] = "vert botright 80"

vim.keymap.set("n", "<Leader>tt", ":TestSuite<CR>", { silent = true })
vim.keymap.set("n", "<Leader>tf", ":TestFile<CR>", { silent = true })
vim.keymap.set("n", "<Leader>tm", ":TestNearest<CR>", { silent = true })

-- require("neotest").setup({
--   adapters = {
--     require("neotest-elixir"),
--     require("neotest-python"),
--   },
-- })
--
-- vim.keymap.set("n", "<Leader>tt", function()
--     require("neotest").run.run({suite = true})
-- end, { silent = true })
-- vim.keymap.set("n", "<Leader>tf", ":TestFile<CR>", { silent = true })
-- vim.keymap.set("n", "<Leader>tm", ":TestNearest<CR>", { silent = true })
