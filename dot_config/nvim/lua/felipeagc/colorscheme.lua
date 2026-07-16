vim.pack.add({
    { src = "https://github.com/Kaikacy/Lemons.nvim" },
    { src = "https://github.com/sainnhe/gruvbox-material" },
})

vim.api.nvim_set_option_value("background", "dark", {})
vim.g.gruvbox_material_background = "hard"
vim.g.gruvbox_material_transparent_background = true
vim.cmd.colorscheme("gruvbox-material")
