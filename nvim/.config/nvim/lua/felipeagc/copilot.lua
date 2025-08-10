vim.pack.add({
    { src = "https://github.com/zbirenbaum/copilot.lua" }
})

require("copilot").setup({
    panel = { enabled = false },
    filetypes = {
        typescript = true,
        typescriptreact = true,
        javascript = true,
        javascriptreact = true,
        python = true,
        elixir = true,
        heex = true,
        ["*"] = false,
    },
    suggestion = {
        enabled = true,
        auto_trigger = true,
        hide_during_completion = true,
        debounce = 75,
        trigger_on_accept = true,
        keymap = {
            accept = "<C-l>",
            accept_word = false,
            accept_line = false,
            next = false,
            prev = false,
            dismiss = false,
        },
    },
})
