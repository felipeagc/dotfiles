vim.pack.add({
    "https://github.com/windwp/nvim-ts-autotag",
    "https://github.com/windwp/nvim-autopairs",
    "https://github.com/catgoose/nvim-colorizer.lua",
    "https://github.com/ntpeters/vim-better-whitespace", -- highlight trailing whitespace
})

vim.g.strip_whitespace_on_save = 1
vim.g.strip_whitespace_confirm = 0

require("nvim-ts-autotag").setup()

require("nvim-autopairs").setup({
    disable_filetype = { "TelescopePrompt", "vim" },
})

require("colorizer").setup({
    filetypes = {
        "typescript",
        "typescriptreact",
        "javascript",
        "javascriptreact",
        "css",
        "html",
    },
    user_default_options = {
        names = true,        -- "Name" codes like Blue or blue
        css = true,          -- Enable all CSS features: rgb_fn, hsl_fn, names, RGB, RRGGBB
        mode = "background", -- Set the display mode.
        tailwind = true,     -- Enable tailwind colors
        virtualtext = "■",
        always_update = false,
    },
})
