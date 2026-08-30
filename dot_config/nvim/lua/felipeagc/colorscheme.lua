vim.pack.add({
    { src = "https://github.com/sainnhe/gruvbox-material" },
    { src = "https://github.com/nickkadutskyi/jb.nvim" },
})

local dark_theme = "jb"
local light_theme = "jb"

-- Theme configs {{{
vim.g.gruvbox_material_background = "hard"
vim.g.gruvbox_material_transparent_background = true

require("jb").setup({
    transparent = true,
    -- colorblind = true,
    disable_hl_args = {
        bold = false,
        italic = true,
    },
    integrations = {
        -- ghostty = true,
    }
})
-- }}}

-- Theme overrides {{{
vim.api.nvim_create_autocmd("ColorScheme", {
    callback = function()
        -- vim.api.nvim_set_hl(0, "Comment", { fg = "#7c9f4b", italic = false })
        -- vim.api.nvim_set_hl(0, "@comment", { link = "Comment" })
        -- vim.api.nvim_set_hl(0, "@comment.documentation", { link = "Comment" })
        -- vim.api.nvim_set_hl(0, "@keyword.return", { link = "Keyword" })
        --
        -- -- the statusline itself
        -- vim.api.nvim_set_hl(0, "StatusLine",   { fg = "#dedee4", bg = "#1f1f23" })
        -- vim.api.nvim_set_hl(0, "StatusLineNC", { fg = "#aeaeb4", bg = "NONE" })
        -- -- a separate group just for the cwd segment
        -- vim.api.nvim_set_hl(0, "StatusLineCwd", { fg = "#0f0f13", bg = "#e0a08c" })
    end,
})
-- }}}

-- Statusline {{{
function _G.statusline_cwd()
  return vim.fn.fnamemodify(vim.fn.getcwd(), ":t")
end

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
-- }}}

-- Auto dark/light theme {{{
local function apply_theme()
  vim.cmd.colorscheme(vim.o.background == "dark" and dark_theme or light_theme)
end

vim.api.nvim_create_autocmd("OptionSet", {
  pattern = "background",
  callback = function()
    vim.schedule(apply_theme)
  end,
})
apply_theme()
-- }}}
