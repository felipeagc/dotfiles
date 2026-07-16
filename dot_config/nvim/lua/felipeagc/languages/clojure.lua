vim.pack.add({
    -- { src = "https://github.com/gpanders/nvim-parinfer" },
    { src = "https://github.com/eraserhd/parinfer-rust" },
    -- { src = "https://github.com/HiPhish/rainbow-delimiters.nvim" },
    { src = "https://github.com/clojure-vim/vim-jack-in" },
    { src = "https://github.com/Olical/conjure" },
})

-- require("rainbow-delimiters.setup").setup({
--     whitelist = { "clojure" }
-- })

vim.g["conjure#filetypes"] = { "clojure" }
vim.g["conjure#mapping#enable_defaults"] = false
vim.g["conjure#mapping#prefix"] = "<leader>"
vim.g["conjure#mapping#eval_buf"] = "eb"
vim.g["conjure#mapping#eval_current_form"] = "ee"
vim.g["conjure#mapping#log_vsplit"] = "lv"
vim.g["conjure#mapping#log_toggle"] = "lg"
vim.g["conjure#mapping#log_close_visible"] = "lq"
vim.g["conjure#client#clojure#nrepl#mapping#run_all_tests"] = "ta"
vim.g["conjure#client#clojure#nrepl#mapping#run_current_ns_tests"] = "tn"
vim.g["conjure#client#clojure#nrepl#mapping#run_alternate_ns_tests"] = "tN"
vim.g["conjure#client#clojure#nrepl#mapping#run_current_test"] = "tc"
vim.g["conjure#client#clojure#nrepl#mapping#refresh_all"] = "ra"
vim.g["conjure#client#clojure#nrepl#mapping#last_exception"] = "ve"

vim.g["conjure#client#clojure#nrepl#connection#auto_repl#cmd"] = "clj -M:repl"
vim.g["conjure#client#clojure#nrepl#connection#auto_repl#enabled"] = false

create_augroup("clojure", function()
    vim.cmd("setlocal splitright")
    vim.keymap.set("n", "<C-Return>", "<Space>ee", { buffer = true, remap = true })
    vim.keymap.set("i", "<C-Return>", "<C-o><Space>ee", { buffer = true, remap = true })
    vim.cmd("setlocal iskeyword=@,48-57,_,192-255,-")
end)

vim.api.nvim_create_autocmd('PackChanged', {
  desc = 'Build command for parinfer-rust',
  group = vim.api.nvim_create_augroup('parinfer-rust-updated', { clear = true }),
  callback = function(ev)
    local spec = ev.data.spec
    local kind = ev.data.kind
    if spec
        and spec.name == 'parinfer-rust'
        and (kind == 'install' or kind == 'update') then
      local path = ev.data.path
      vim.schedule(function()
        vim.system({ 'cargo', 'build', '--release' }, { cwd = path })
      end)
    end
  end
})
