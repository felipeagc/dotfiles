vim.pack.add({
    "https://codeberg.org/mfussenegger/nvim-jdtls",
})
local home = vim.fn.expand("$HOME")
local project_name = vim.fn.fnamemodify(vim.fn.getcwd(), ':p:h:t')
local workspace_dir = home.."/.jdtls-data/" .. project_name

local config = {
    cmd = { "jdtls", "-data", workspace_dir },
    root_dir = vim.fs.root(0, {"gradlew", ".git", ".jj", "mvnw"}),
    settings = {
        java = {
            import = {
                generatesMetadataFilesAtProjectRoot = false,
                gradle = {
                    enabled = true,
                    annotationProcessing = {
                        enabled = true,
                    },
                },
            },
        },
    },
}

vim.lsp.config("jdtls", config)
vim.lsp.enable("jdtls")
