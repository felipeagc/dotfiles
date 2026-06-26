vim.pack.add({
    { src = "https://github.com/nvim-mini/mini.icons", version = "stable" },
    { src = "https://github.com/nvim-mini/mini.pick", version = "stable" },
    { src = "https://github.com/nvim-mini/mini.visits", version = "stable" },
    { src = "https://github.com/nvim-tree/nvim-tree.lua" },
    { src = "https://github.com/stevearc/oil.nvim" },
    { src = "https://github.com/nvim-lua/plenary.nvim" },
    -- { src = "https://github.com/nvim-telescope/telescope.nvim" },
})

require('mini.icons').setup()
MiniIcons.mock_nvim_web_devicons()

require('mini.visits').setup()

local win_config = function()
    local height = math.floor(0.8 * vim.o.lines)
    local width = math.floor(0.8 * vim.o.columns)
    return {
        anchor = 'NW', height = height, width = width,
        row = math.floor(0.5 * (vim.o.lines - height)),
        col = math.floor(0.5 * (vim.o.columns - width)),
    }
end
require('mini.pick').setup({
    mappings = {
        paste = "",
        refine = '<C-r>',
        mark = '<C-x>',
        choose_marked = '<C-q>',
    },
    window = { config = win_config },
})

require("nvim-tree").setup {
    view = {
        width = 50,
    },
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
        vim.keymap.set('n', 'gt', "<CMD>NvimTreeClose<CR>", opts("Toggle tree"))
        vim.keymap.del('n', '<C-k>', { buffer = bufnr })
    end
}
vim.keymap.set("n", "gt", "<CMD>NvimTreeFindFile<CR>", { desc = "Toggle tree" })

require("oil").setup({
    default_file_explorer = true,
    keymaps = {
        ["<C-p>"] = false,
    },
})
vim.keymap.set("n", "-", "<CMD>Oil<CR>", { desc = "Toggle file browser" })
