vim.pack.add({
    { src = "https://github.com/Kaikacy/Lemons.nvim" },
    { src = "https://github.com/sainnhe/gruvbox-material" },
    { src = "https://github.com/pjhamera/national-parks-themes" },
})

function _G.statusline_cwd()
  return vim.fn.fnamemodify(vim.fn.getcwd(), ":t")
end

vim.api.nvim_set_option_value("background", "dark", {})
vim.api.nvim_create_autocmd("ColorScheme", {
    callback = function()
        vim.api.nvim_set_hl(0, "Comment", { fg = "#7c9f4b", italic = false })
        vim.api.nvim_set_hl(0, "@comment", { link = "Comment" })
        vim.api.nvim_set_hl(0, "@comment.documentation", { link = "Comment" })
        vim.api.nvim_set_hl(0, "@keyword.return", { link = "Keyword" })

        -- the statusline itself
        vim.api.nvim_set_hl(0, "StatusLine",   { fg = "#dedee4", bg = "#1f1f23" })
        vim.api.nvim_set_hl(0, "StatusLineNC", { fg = "#aeaeb4", bg = "NONE" })
        -- a separate group just for the cwd segment
        vim.api.nvim_set_hl(0, "StatusLineCwd", { fg = "#0f0f13", bg = "#e0a08c" })
    end,
})


local active = table.concat({
  "%#StatusLineCwd# %{v:lua.statusline_cwd()} ",
  "%* %f %m%r",
  "%=",
  "%y  %l:%c  %p%% ",
})

local inactive = table.concat({
  " %{v:lua.statusline_cwd()}  %f %m%r",
  "%=",
  "%y  %l:%c  %p%% ",
})

vim.opt.statusline = active

vim.api.nvim_create_autocmd({ "WinEnter", "BufWinEnter" }, {
  callback = function() vim.wo.statusline = active end,
})

vim.api.nvim_create_autocmd("WinLeave", {
  callback = function() vim.wo.statusline = inactive end,
})

-- vim.g.gruvbox_material_background = "hard"
-- vim.g.gruvbox_material_transparent_background = true
-- vim.cmd.colorscheme("gruvbox-material")


require("parks").setup({
    transparent = true,
})
vim.cmd.colorscheme("parks-black-canyon-of-the-gunnison")
