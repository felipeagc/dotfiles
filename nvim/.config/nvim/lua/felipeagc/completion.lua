vim.pack.add({
    { src = "https://github.com/saghen/blink.cmp", version = "v1.6.0" }
})

require('blink.cmp').setup({
    keymap = {
        preset = "enter",
        ["<C-n>"] = { "show", "select_next" },
    },
    sources = {
        default = { "lsp", "path", "snippets" },
    },
    cmdline = {
        enabled = true,
    },
    completion = {
        list = {
            selection = { auto_insert = true },
        },
        trigger = {
            show_on_keyword = true,
            show_on_trigger_character = true,
            show_on_insert_on_trigger_character = true,
            show_on_accept_on_trigger_character = true,
        },
        menu = {
            auto_show = false,
            draw = {
                treesitter = { "lsp" },
                columns = { { "label", "label_description", gap = 1 }, { "kind_icon", "kind" } },
            },
        },
        documentation = {
            auto_show = true,
            auto_show_delay_ms = 500,
        },
    },
})
