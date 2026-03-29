vim.pack.add({
    "https://codeberg.org/mfussenegger/nvim-jdtls",
})
local home = vim.fn.expand("$HOME")
local project_name = vim.fn.fnamemodify(vim.fn.getcwd(), ':p:h:t')
local workspace_dir = home.."/.jdtls-data/" .. project_name

vim.lsp.config("jdtls", {
    cmd = {
        "jdtls",
        "-data", workspace_dir
    },
    root_dir = vim.fs.root(0, {"gradlew", ".git", ".jj", "mvnw"}),
    settings = {
        java = {
            -- Custom eclipse.jdt.ls options go here
        },
    },
})

vim.lsp.enable("jdtls")
