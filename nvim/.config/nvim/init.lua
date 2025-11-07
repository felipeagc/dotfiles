-- Basics
require("felipeagc.base")
require("felipeagc.keybinds")

-- Submodules
require("felipeagc.colorscheme")
require("felipeagc.treesitter")
require("felipeagc.navigation")
require("felipeagc.util")
require("felipeagc.git")
require("felipeagc.jj")
require("felipeagc.completion")
require("felipeagc.formatting")
require("felipeagc.testing")
require("felipeagc.lsp")
require("felipeagc.ai")

-- Languages
require("felipeagc.languages.clojure")
require("felipeagc.languages.cpp")
require("felipeagc.languages.elixir")
require("felipeagc.languages.go")
require("felipeagc.languages.javascript")
require("felipeagc.languages.ocaml")
require("felipeagc.languages.odin")
require("felipeagc.languages.rust")
require("felipeagc.languages.zig")

vim.pack.add({
    "https://github.com/NoahTheDuke/vim-just",
    "https://github.com/kaarmu/typst.vim",
})
