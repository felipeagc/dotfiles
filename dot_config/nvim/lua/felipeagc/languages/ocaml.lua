create_augroup("ocaml", function()
    vim.cmd([[ setlocal cpt-=t ]])
    vim.opt_local.makeprg = "dune build"
end)

local config = {
    cmd = { "ocamllsp" },
    filetypes = { "ocaml", "menhir", "ocamlinterface", "ocamllex", "reason", "dune" },
    root_dir = vim.fs.root(0, {"dune-project"}),
}

vim.lsp.config("ocamllsp", config)
vim.lsp.enable("ocamllsp")
