vim.lsp.config("hledger-lsp", {
    cmd = { "hledger-lsp" },
    filetypes = { "ledger" },
    root_markers = { ".git" }
})
vim.lsp.enable("hledger-lsp")
