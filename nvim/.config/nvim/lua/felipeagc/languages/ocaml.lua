create_augroup("ocaml", function()
    vim.cmd([[ setlocal cpt-=t ]])
    vim.opt_local.makeprg = "dune build"
end)
