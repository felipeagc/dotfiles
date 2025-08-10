-- Basics
require("felipeagc.base")

-- Submodules
require("felipeagc.colorscheme")
require("felipeagc.treesitter")
require("felipeagc.navigation")
require("felipeagc.util")
require("felipeagc.git")
require("felipeagc.completion")
require("felipeagc.formatting")
require("felipeagc.testing")
require("felipeagc.lsp")
require("felipeagc.copilot")

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
    "https://github.com/alaviss/nim.nvim",
    "https://github.com/ziglang/zig.vim",
    "https://github.com/rust-lang/rust.vim",
    "https://github.com/NoahTheDuke/vim-just",
    "https://github.com/elixir-editors/vim-elixir",
    "https://github.com/kaarmu/typst.vim",
    "https://github.com/seblyng/roslyn.nvim",
})
