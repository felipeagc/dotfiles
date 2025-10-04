vim.pack.add({
    { src = "https://github.com/stevearc/conform.nvim" }
})

require("conform").setup({
    default_format_opts = {
        lsp_format = "fallback",
    },
    formatters_by_ft = {
        peanuts = { "peanuts_fmt" },
        ledger = { "hledger-fmt" },
        go = { "goimports", lsp_format = "last" },
        javascript = { "prettierd", "prettier", stop_after_first = true },
        javascriptreact = { "prettierd", "prettier", stop_after_first = true },
        typescript = { "prettierd", "prettier", stop_after_first = true },
        typescriptreact = { "prettierd", "prettier", stop_after_first = true },
        elixir = { "mix" },
        eelixir = { "mix" },
        heex = { "mix" },
    },
    formatters = {
        peanuts_fmt = {
            command = "peanuts",
            args = { "fmt", "--stdin" },
            stdin = true,
        },
        ["hledger-fmt"] = {
            command = "hledger-fmt",
            args = { "-", "--no-diff", "--exit-zero-on-changes" },
            stdin = true,
        },
    },
})

vim.keymap.set("n", "<Leader>mf", require("conform").format, { silent = true })
