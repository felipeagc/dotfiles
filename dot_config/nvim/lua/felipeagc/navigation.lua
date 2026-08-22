vim.pack.add({
    { src = "https://github.com/nvim-mini/mini.icons", version = "stable" },
    -- { src = "https://github.com/nvim-mini/mini.pick", version = "stable" },
    { src = "https://github.com/nvim-tree/nvim-tree.lua" },
    { src = "https://github.com/stevearc/oil.nvim" },
    { src = "https://github.com/nvim-lua/plenary.nvim" },
    { src = "https://github.com/ibhagwan/fzf-lua" },
    -- { src = "https://github.com/nvim-telescope/telescope.nvim" },
})

require("mini.icons").setup()
MiniIcons.mock_nvim_web_devicons()

require("fzf-lua").setup({
    winopts = {
        preview = {
            hidden = true,
        },
    },
    actions = {
        files = {
            true,
            ["ctrl-q"] = {
                fn = FzfLua.actions.file_sel_to_qf,
                prefix = "select-all",
            },
        }
    }
})

require("nvim-tree").setup({
    view = {
        width = 50,
    },
    on_attach = function(bufnr)
        local api = require("nvim-tree.api")

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
        vim.keymap.set("n", "gt", "<CMD>NvimTreeClose<CR>", opts("Toggle tree"))
        vim.keymap.del("n", "<C-k>", { buffer = bufnr })
    end,
})
vim.keymap.set("n", "gt", "<CMD>NvimTreeFindFile<CR>", { desc = "Toggle tree" })

require("oil").setup({
    default_file_explorer = true,
    keymaps = {
        ["<C-p>"] = false,
    },
})
vim.keymap.set("n", "-", "<CMD>Oil<CR>", { desc = "Toggle file browser" })
