vim.pack.add({
    { src = "https://github.com/gpanders/nvim-parinfer" },
    { src = "https://github.com/HiPhish/rainbow-delimiters.nvim" },
    { src = "https://github.com/clojure-vim/vim-jack-in" },
    { src = "https://github.com/Olical/conjure" },
})

require("rainbow-delimiters.setup").setup({
    whitelist = { "clojure" }
})

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

vim.g["conjure#client#clojure#nrepl#connection#auto_repl#cmd"] = "clj -M:repl"
vim.g["conjure#client#clojure#nrepl#connection#auto_repl#enabled"] = false

create_augroup("clojure", function()
    vim.cmd("setlocal splitright")
    vim.keymap.set("n", "<C-Return>", "<Space>ee", { buffer = true, remap = true })
    vim.keymap.set("i", "<C-Return>", "<C-o><Space>ee", { buffer = true, remap = true })
end)
