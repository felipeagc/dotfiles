vim.pack.add({
    "https://github.com/ziglang/zig.vim",
})

vim.g.zig_fmt_autosave = 0
create_augroup("zig", function()
    vim.cmd([[ setlocal cpt-=t ]])
end)
