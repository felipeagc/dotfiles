vim.pack.add({
    { src = "https://github.com/echasnovski/mini.icons" },
    { src = "https://github.com/nvim-tree/nvim-tree.lua" },
    { src = "https://github.com/nvim-lua/plenary.nvim" },
    { src = "https://github.com/nvim-telescope/telescope.nvim" },
    { src = "https://github.com/stevearc/dressing.nvim" },
})

require('mini.icons').setup()
MiniIcons.mock_nvim_web_devicons()

local actions = require("telescope.actions")
require("telescope").setup({
    defaults = {
        preview = true,
        mappings = {
            i = {
                ["<esc>"] = actions.close,
            },
        },
        layout_strategy = "horizontal",
        layout_config = {
            height = { padding = 4 },
            width = { padding = 4 },
        },
    },
})

require("dressing").setup({
    input = { enabled = false },
    select = {
        enabled = true,
        backend = { "telescope" },
    },
})

require("nvim-tree").setup {
    on_attach = function(bufnr)
        local api = require "nvim-tree.api"

        local function opts(desc)
            return {
                desc = "nvim-tree: " .. desc,
                buffer = bufnr,
                noremap = true,
                silent = true,
                nowait = true,
            }
        end

        -- default mappings
        api.config.mappings.default_on_attach(bufnr)

        -- custom mappings
        vim.keymap.set('n', '-', "<CMD>NvimTreeClose<CR>", opts("Toggle tree"))
        vim.keymap.del('n', '<C-k>', { buffer = bufnr })
    end
}
vim.keymap.set("n", "-", "<CMD>NvimTreeFindFile<CR>", { desc = "Toggle tree" })
