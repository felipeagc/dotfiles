vim.pack.add({
    { src = "https://github.com/nvim-treesitter/nvim-treesitter" },
    { src = "https://github.com/nvim-treesitter/playground" },
})

require("nvim-treesitter.configs").setup({
    sync_install = false,
    auto_install = true,
    ensure_installed = {
        "bash",
        "c",
        "cpp",
        "css",
        "go",
        "html",
        "javascript",
        "lua",
        "make",
        "markdown",
        "python",
        "rust",
        "tsx",
        "typescript",
        "yaml",
        "zig",
    },
    highlight = {
        enable = true, -- false will disable the whole extension
        disable = {},
        additional_vim_regex_highlighting = false,
    },
    indent = {
        enable = true,
        disable = {
            "c",
            "cpp",
            "haskell",
            "ocaml",
            "python",
            "sql",
        },
    },
})
