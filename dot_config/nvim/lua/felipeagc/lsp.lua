vim.pack.add({
    { src = "https://github.com/neovim/nvim-lspconfig" },
    { src = "https://github.com/mason-org/mason.nvim" },
    { src = "https://github.com/mason-org/mason-lspconfig.nvim" },
})

require("mason").setup({})
require("mason-lspconfig").setup({})

vim.diagnostic.config({
    virtual_text = true,
    signs = false,
    underline = true,
    update_in_insert = false,
    severity_sort = true,
})

local lsp_capabilities = vim.lsp.protocol.make_client_capabilities()
lsp_capabilities = require("blink.cmp").get_lsp_capabilities(lsp_capabilities) -- Add blink.cmp capabilities
lsp_capabilities.workspace.didChangeWatchedFiles.dynamicRegistration = true    -- Enable file watcher support for LSP

vim.lsp.config("*", {
    capabilities = lsp_capabilities,
})
vim.lsp.config("clangd", {
    filetypes = { "c", "cpp", "objc", "objcpp", "cuda" },
})
vim.lsp.enable({ "clangd" })

vim.api.nvim_create_autocmd("LspAttach", {
    group = vim.api.nvim_create_augroup("UserLspConfig", {}),
    callback = function(ev)
        -- Disable semantic highlight for all LSP servers
        local client = assert(vim.lsp.get_client_by_id(ev.data.client_id))
        client.server_capabilities.semanticTokensProvider = nil

        local opts = { remap = false, silent = true, buffer = ev.bufnr }

        -- vim.keymap.set("n", "[d", vim.diagnostic.goto_prev, opts)
        -- vim.keymap.set("n", "]d", vim.diagnostic.goto_next, opts)
        vim.keymap.set("n", "<C-y>", vim.diagnostic.open_float, opts)

        -- Toggle inlay hints
        local filter = { bufnr = ev.bufnr }
        vim.lsp.inlay_hint.enable(false, filter)
        vim.keymap.set("n", "<C-/>", function()
            vim.lsp.inlay_hint.enable(not vim.lsp.inlay_hint.is_enabled(filter), filter)
        end, opts)
    end,
})
