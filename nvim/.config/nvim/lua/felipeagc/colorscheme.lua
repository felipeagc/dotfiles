vim.pack.add({
    { src = "https://github.com/f-person/auto-dark-mode.nvim" },
    { src = "https://github.com/folke/tokyonight.nvim" },
    { src = "https://github.com/p00f/alabaster.nvim" },
    { src = "https://github.com/Kaikacy/Lemons.nvim" },
    { src = "https://github.com/sainnhe/gruvbox-material" },
    { src = "https://github.com/bjarneo/ethereal.nvim" },
    { src = "https://github.com/gthelding/monokai-pro.nvim" },
})

require("monokai-pro").setup({
    filter = "ristretto",
    override = function()
        return {
            NonText = { fg = "#948a8b" },
            MiniIconsGrey = { fg = "#948a8b" },
            MiniIconsRed = { fg = "#fd6883" },
            MiniIconsBlue = { fg = "#85dacc" },
            MiniIconsGreen = { fg = "#adda78" },
            MiniIconsYellow = { fg = "#f9cc6c" },
            MiniIconsOrange = { fg = "#f38d70" },
            MiniIconsPurple = { fg = "#a8a9eb" },
            MiniIconsAzure = { fg = "#a8a9eb" },
            MiniIconsCyan = { fg = "#85dacc" }, -- same value as MiniIconsBlue for consistency
        }
    end,
})

require("auto-dark-mode").setup({
    set_dark_mode = function()
        vim.api.nvim_set_option_value("background", "dark", {})

        vim.g.gruvbox_material_foreground = "soft"
        vim.g.gruvbox_material_background = "soft"
        -- vim.cmd.colorscheme("monokai-pro")
        vim.cmd.colorscheme("ethereal")
    end,
    set_light_mode = function()
        vim.api.nvim_set_option_value("background", "light", {})
        vim.cmd.colorscheme("alabaster")
    end,
    update_interval = 3000,
    fallback = "dark",
})
