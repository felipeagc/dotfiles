-- Package manager setup {{{
local lazypath = vim.fn.stdpath("data") .. "/lazy/lazy.nvim"
if not vim.loop.fs_stat(lazypath) then
    vim.fn.system({
        "git",
        "clone",
        "--filter=blob:none",
        "https://github.com/folke/lazy.nvim.git",
        "--branch=stable", -- latest stable release
        lazypath,
    })
end
vim.opt.rtp:prepend(lazypath)

local function create_augroup(filetype, callback)
    if type(filetype) == "table" then
        for _, ft in ipairs(filetype) do
            create_augroup(ft, callback)
        end
    else
        local group = vim.api.nvim_create_augroup(filetype .. "_augroup", {})
        vim.api.nvim_create_autocmd({ "FileType" }, {
            pattern = filetype,
            callback = callback,
            group = group,
        })
    end
end
-- }}}

-- vim.cmd.colorscheme("habamax")
-- vim.api.nvim_set_hl(0, "Normal", { bg = "#151415" })
-- vim.api.nvim_set_hl(0, "vertsplit", { bg = "#000000", fg = "#9e9e9e" })

-- vim.cmd.colorscheme("lunaperche")
-- vim.api.nvim_set_hl(0, "Normal", { bg = "#151415" })
-- vim.api.nvim_set_hl(0, "VertSplit", { bg = "#000000", fg = "#767676" })

require("lazy").setup({
    "equalsraf/neovim-gui-shim",

    "neovim/nvim-lspconfig",
    { "williamboman/mason.nvim", opts = {} },
    { "williamboman/mason-lspconfig.nvim", opts = {} },

    {
        "echasnovski/mini.icons",
        opts = {},
        lazy = true,
        specs = {
            {
                "nvim-tree/nvim-web-devicons",
                enabled = false,
                optional = true,
            },
        },
        init = function()
            package.preload["nvim-web-devicons"] = function()
                require("mini.icons").mock_nvim_web_devicons()
                return package.loaded["nvim-web-devicons"]
            end
        end,
    },

    {
        "saghen/blink.cmp",
        version = "v1.2.0",
        opts = {
            keymap = {
                preset = "enter",
                ["<C-n>"] = { "show", "select_next" },
            },
            sources = {
                default = { "lsp", "path", "snippets" },
            },
            cmdline = {
                enabled = true,
            },
            completion = {
                list = {
                    selection = { auto_insert = true },
                },
                trigger = {
                    show_on_keyword = true,
                    show_on_trigger_character = true,
                    show_on_insert_on_trigger_character = true,
                    show_on_accept_on_trigger_character = true,
                },
                menu = {
                    auto_show = false,
                    draw = {
                        treesitter = { "lsp" },
                        columns = { { "label", "label_description", gap = 1 }, { "kind_icon", "kind" } },
                    },
                },
                documentation = {
                    auto_show = true,
                    auto_show_delay_ms = 500,
                },
            },
        },
        opts_extend = { "sources.default" },
    },

    {
        "stevearc/conform.nvim",
        opts = {
            default_format_opts = {
                lsp_format = "fallback",
            },
            formatters_by_ft = {
                peanuts = { "peanuts_fmt" },
                go = { "goimports", lsp_format = "last" },
            },
            formatters = {
                peanuts_fmt = {
                    command = "peanuts",
                    args = { "fmt", "--stdin" },
                    stdin = true,
                },
            },
        },
    },

    {
        "nvim-treesitter/nvim-treesitter",
        build = ":TSUpdate",
        config = function()
            require("nvim-treesitter.configs").setup({
                sync_install = false,
                auto_install = true,
                ensure_installed = {
                    "bash",
                    "c",
                    "cpp",
                    "css",
                    "go",
                    "html",
                    "javascript",
                    "lua",
                    "make",
                    "markdown",
                    "python",
                    "rust",
                    "tsx",
                    "typescript",
                    "yaml",
                    "zig",
                },
                highlight = {
                    enable = true, -- false will disable the whole extension
                    disable = {},
                    additional_vim_regex_highlighting = false,
                },
                indent = {
                    enable = true,
                    disable = {
                        "c",
                        "cpp",
                        "haskell",
                        "ocaml",
                        "python",
                        "sql",
                    },
                },
            })
        end,
    },
    "nvim-treesitter/playground",
    {
        "windwp/nvim-ts-autotag",
        config = function()
            require("nvim-ts-autotag").setup()
        end,
    },

    "tpope/vim-surround",
    "tpope/vim-endwise",
    "tpope/vim-repeat",
    "tpope/vim-fugitive",
    "tpope/vim-abolish",
    "tpope/vim-unimpaired",
    "tpope/vim-dispatch",
    "tpope/vim-projectionist",
    "ntpeters/vim-better-whitespace", -- highlight trailing whitespace
    {
        "windwp/nvim-autopairs",
        event = "InsertEnter",
        opts = {
            disable_filetype = { "TelescopePrompt", "vim", "clojure" },
        },
    },
    {
        "catgoose/nvim-colorizer.lua",
        opts = {
            filetypes = { "typescript", "typescriptreact", "javascript", "javascriptreact", "css", "html" },
            user_default_options = {
                names = true,        -- "Name" codes like Blue or blue
                css = true,          -- Enable all CSS features: rgb_fn, hsl_fn, names, RGB, RRGGBB
                mode = "background", -- Set the display mode.
                tailwind = true,     -- Enable tailwind colors
                virtualtext = "■",
                always_update = false,
            },
        },
    },

    {
        "stevearc/oil.nvim",
        config = function()
            require("oil").setup({
                default_file_explorer = true,
                keymaps = { ["<C-p>"] = false },
            })
            vim.keymap.set("n", "-", "<CMD>Oil<CR>", { desc = "Open parent directory" })
        end,
    },

    {
        "lewis6991/gitsigns.nvim",
        config = function()
            require("gitsigns").setup()
        end,
    },

    {
        "vim-test/vim-test",
        config = function()
            vim.g["test#strategy"] = "dispatch"
        end,
    },
    { "kdheepak/lazygit.nvim",  dependencies = { "nvim-lua/plenary.nvim" } },

    "editorconfig/editorconfig-vim",


    -- Language support
    { "alaviss/nim.nvim",          ft = { "nim" } },
    { "ziglang/zig.vim",           ft = { "zig" } },
    { "rust-lang/rust.vim",        ft = { "rust" } },
    { "NoahTheDuke/vim-just",      ft = { "just" } },
    { "elixir-editors/vim-elixir", ft = { "elixir" } },
    { "kaarmu/typst.vim",          ft = { "typst" } },
    { "seblyng/roslyn.nvim",       ft = { "cs" } },
    {
        "Olical/conjure",
        ft = { "clojure" },
        lazy = true,
        init = function()
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
        end,
        dependencies = {
            {
                "gpanders/nvim-parinfer",
                lazy = true,
                ft = { "clojure" }
            },
            {
                "HiPhish/rainbow-delimiters.nvim",
                lazy = true,
                ft = { "clojure" },
                submodules = false,
                init = function()
                    vim.g["rainbow_delimiters"] = {
                        whitelist = {'clojure'}
                    }
                end,
            },
            { "clojure-vim/vim-jack-in" }
        },
    },

    "nvim-lua/plenary.nvim",
    {
        "nvim-telescope/telescope.nvim",
        config = function()
            local actions = require("telescope.actions")
            require("telescope").setup({
                defaults = {
                    preview = true,
                    mappings = {
                        i = {
                            ["<esc>"] = actions.close,
                        },
                    },
                    layout_strategy = "horizontal",
                    layout_config = {
                        height = { padding = 4 },
                        width = { padding = 4 },
                    },
                },
            })
        end,
    },

    {
        "stevearc/dressing.nvim",
        opts = {
            input = { enabled = false },
            select = {
                enabled = true,
                backend = { "telescope" },
            },
        },
    },

    -- {
    --     "neanias/everforest-nvim",
    --     opts = {
    --         background = "hard",
    --         disable_italic_comments = true,
    --     },
    --     config = function()
    --         vim.cmd.colorscheme("everforest")
    --     end,
    -- },
    -- {
    --     "felipeagc/fleet-theme-nvim",
    --     dir = "~/Code/lua/fleet-theme-nvim",
    --     config = function() vim.cmd("colorscheme fleet") end
    -- },
    {
        "AlexvZyl/nordic.nvim",
        config = function()
            require('nordic').setup({
                reduced_blue = true,
            })
            vim.cmd.colorscheme("nordic")
        end
    }
})

-- Vim options {{{
vim.o.termguicolors = true

vim.o.exrc = true
vim.o.showcmd = true
vim.o.mouse = "a"

vim.o.scrolloff = 5
vim.o.clipboard = "unnamedplus"
vim.o.completeopt = "menu,menuone,noselect"

vim.o.tabstop = 4
vim.o.softtabstop = 4
vim.o.shiftwidth = 4
vim.o.autoindent = true
vim.o.smartindent = true
vim.o.smarttab = true
vim.o.expandtab = true

vim.o.wrap = true
vim.o.wildignore = vim.o.wildignore
    .. "*.so,*.swp,*.zip,*.o,*.png,*.jpg,*.jpeg,*/target/*,*/build/*,*/node_modules/*,tags,*.glb,*.gltf,*.hdr"
vim.o.hidden = true
vim.o.showmode = false
vim.o.modeline = true
vim.o.modelines = 1

vim.o.undofile = true
vim.o.undodir = vim.fn.stdpath("data") .. "/undodir"

vim.o.linebreak = true

vim.o.showbreak = ">   "
vim.o.shortmess = vim.o.shortmess .. "c"

vim.o.ignorecase = true
vim.o.smartcase = true

vim.o.cinoptions = vim.o.cinoptions .. "L0"
vim.o.cinoptions = vim.o.cinoptions .. "l1"

vim.o.pumheight = 8        -- Completion menu height
vim.o.signcolumn = "yes:1" -- Configure minimum gutter width

vim.wo.number = true
-- vim.wo.cursorline = true
vim.wo.foldmethod = "marker"
-- vim.wo.foldlevel = 0
-- }}}

-- Keybinds {{{
vim.g.mapleader = " "

vim.keymap.set("n", "<C-p>", ":Telescope find_files<CR>", { silent = true })
vim.keymap.set("n", "<C-b>", ":Telescope buffers<CR>", { silent = true })
vim.keymap.set("n", "<Leader>fg", ":Telescope live_grep<CR>", { silent = true })
vim.keymap.set("n", "<Leader>fr", ":Telescope resume<CR>", { silent = true })

vim.keymap.set("n", "<C-j>", "<C-w>w", { remap = false })
vim.keymap.set("n", "<C-k>", "<C-w>W", { remap = false })

vim.keymap.set("n", "<C-e>", ":CNext<CR>", { silent = true })
vim.keymap.set("n", "<C-q>", ":CPrev<CR>", { silent = true })

-- Continuous indentation shift
vim.keymap.set("v", "<", "<gv", { remap = false })
vim.keymap.set("v", ">", ">gv", { remap = false })
vim.keymap.set("v", "<s-lt>", "<gv", { remap = false })

vim.keymap.set("v", "J", ":m '>+1<CR>gv=gv", { remap = false })
vim.keymap.set("v", "K", ":m '<-2<CR>gv=gv", { remap = false })

vim.keymap.set("n", "n", "nzzzv", { remap = false })
vim.keymap.set("n", "N", "Nzzzv", { remap = false })
vim.keymap.set("n", "<C-d>", "<C-d>zz", { remap = false })
vim.keymap.set("n", "<C-u>", "<C-u>zz", { remap = false })

vim.keymap.set("n", "J", "mzJ`z", { remap = false })

vim.keymap.set("n", "<Leader>fed", ":e " .. vim.fn.stdpath("config") .. "/init.lua<CR>", { silent = true })
vim.keymap.set("n", "<Leader>feg", ":e " .. vim.fn.stdpath("config") .. "/ginit.vim<CR>", { silent = true })

vim.keymap.set("n", "<Leader>w/", ":vsp<CR>", { silent = true })
vim.keymap.set("n", "<Leader>w-", ":sp<CR>", { silent = true })
vim.keymap.set("n", "<Leader>wd", ":q<CR>", { silent = true })
vim.keymap.set("n", "<Leader>wb", "<C-w>=", { silent = true })

vim.keymap.set("n", "<Leader>bd", ":bd<CR>", { silent = true })
vim.keymap.set("n", "<Leader>bcc", ":%bd|e#<CR>", { silent = true })

vim.keymap.set("n", "<Leader>gs", ":LazyGitCurrentFile<CR>", { silent = true })

vim.keymap.set("n", "<C-a>", ":A<CR>", { silent = true })

vim.keymap.set("n", "<A-p>", "<nop>", { silent = true })

vim.keymap.set("n", "<Leader>mf", require("conform").format, { silent = true })
vim.keymap.set("n", "<f7>", ":Make<CR>", { silent = true })
vim.keymap.set("n", "<Leader>mb", ":Make<CR>", { silent = true })
vim.keymap.set("n", "<A-r>", ":Make<CR>", { silent = true })

vim.keymap.set("n", "<Leader>tt", ":TestSuite<CR>", { silent = true })
vim.keymap.set("n", "<Leader>tf", ":TestFile<CR>", { silent = true })

-- Disable ex mode binding
vim.cmd([[map Q <Nop>]])
-- }}}

-- LSP {{{
vim.diagnostic.config({
    virtual_text = true,
    signs = false,
    underline = true,
    update_in_insert = false,
    severity_sort = true,
})

local lsp_capabilities = vim.lsp.protocol.make_client_capabilities()
lsp_capabilities = require("blink.cmp").get_lsp_capabilities(lsp_capabilities) -- Add blink.cmp capabilities
lsp_capabilities.workspace.didChangeWatchedFiles.dynamicRegistration = true    -- Enable file watcher support for LSP

vim.lsp.config("*", {
    capabilities = lsp_capabilities,
})
vim.lsp.config("clangd", {
    filetypes = { "c", "cpp", "objc", "objcpp", "cuda" },
})

vim.lsp.enable({
    "clangd",
})

vim.api.nvim_create_autocmd("LspAttach", {
    group = vim.api.nvim_create_augroup("UserLspConfig", {}),
    callback = function(ev)
        -- Disable semantic highlight for all LSP servers
        local client = assert(vim.lsp.get_client_by_id(ev.data.client_id))
        client.server_capabilities.semanticTokensProvider = nil

        local opts = { remap = false, silent = true, buffer = ev.bufnr }

        -- vim.keymap.set("n", "[d", vim.diagnostic.goto_prev, opts)
        -- vim.keymap.set("n", "]d", vim.diagnostic.goto_next, opts)
        vim.keymap.set("n", "<C-y>", vim.diagnostic.open_float, opts)

        -- Toggle inlay hints
        local filter = { bufnr = ev.bufnr }
        vim.lsp.inlay_hint.enable(false, filter)
        vim.keymap.set("n", "<C-/>", function()
            vim.lsp.inlay_hint.enable(not vim.lsp.inlay_hint.is_enabled(filter), filter)
        end, opts)
    end,
})
-- }}}

-- Small quality of life stuff {{{

-- Remap :W to :w
vim.cmd([[
	cnoreabbrev W w
	cnoreabbrev Q q
	cnoreabbrev Wq wq
	cnoreabbrev WQ wq
]])

-- Clear highlights with escape
vim.cmd([[
	nnoremap <silent> <esc> :noh<return><esc>
]])

-- Create non-existing directories before writing buffer
vim.api.nvim_create_autocmd("BufWritePre", {
    callback = function()
        if vim.tbl_contains({ "oil" }, vim.bo.ft) then
            return
        end
        local dir = vim.fn.expand("<afile>:p:h")
        if vim.fn.isdirectory(dir) == 0 then
            vim.fn.mkdir(dir, "p")
        end
    end,
})

-- Open help vertically
vim.cmd([[
    autocmd FileType help wincmd L
    autocmd FileType man wincmd L
]])

--  Fix false positive C bracket error
vim.cmd([[
    let c_no_bracket_error=1
    let c_no_curly_error=1
]])

-- Quickfix helpers
vim.cmd([[
    command! CNext try | cnext | catch | clast | catch | endtry
    command! CPrev try | cprev | catch | cfirst | catch | endtry
]])
-- }}}

-- Indentation {{{
vim.cmd([[
    autocmd FileType c setlocal shiftwidth=4 tabstop=4 expandtab
    " autocmd FileType cpp setlocal shiftwidth=4 tabstop=4 expandtab
    autocmd FileType dart setlocal shiftwidth=2 tabstop=2 expandtab
    autocmd FileType glsl setlocal shiftwidth=4 tabstop=4 expandtab
    autocmd FileType haskell setlocal shiftwidth=2 tabstop=2 expandtab
    autocmd FileType html setlocal shiftwidth=2 tabstop=2 expandtab
    autocmd FileType htmldjango setlocal shiftwidth=2 tabstop=2 expandtab
    autocmd FileType lua setlocal shiftwidth=4 tabstop=4 expandtab
    autocmd FileType ocaml setlocal shiftwidth=2 tabstop=2 expandtab
    autocmd FileType rust setlocal shiftwidth=4 tabstop=4 expandtab
    autocmd FileType sbt setlocal shiftwidth=2 tabstop=2 expandtab
    autocmd FileType scala setlocal shiftwidth=2 tabstop=2 expandtab
    autocmd FileType svelte setlocal shiftwidth=2 tabstop=2 expandtab
    autocmd FileType javascript setlocal shiftwidth=2 tabstop=2 expandtab
    autocmd FileType javascriptreact setlocal shiftwidth=2 tabstop=2 expandtab
    autocmd FileType typescript setlocal shiftwidth=2 tabstop=2 expandtab
    autocmd FileType typescriptreact setlocal shiftwidth=2 tabstop=2 expandtab
    autocmd FileType zig setlocal shiftwidth=4 tabstop=4 expandtab
    autocmd FileType sql setlocal shiftwidth=4 tabstop=4 expandtab
    autocmd FileType terraform setlocal shiftwidth=2 tabstop=2 expandtab
    autocmd FileType capnp setlocal shiftwidth=2 tabstop=2 expandtab
]])
-- }}}

-- C/C++ {{{
vim.g.bazel_make_command = "Make"
vim.g.compiler_gcc_ignore_unmatched_lines = 1
-- vim.g.cpp_no_cpp17 = 1

create_augroup({ "c", "cpp" }, function()
    vim.cmd("setlocal cpt-=t")
    vim.cmd("setlocal commentstring=//\\ %s")

    if vim.fn.filereadable("meson.build") == 1 then
        vim.api.nvim_buf_set_option(0, "makeprg", "ninja -C build")
    end
    if vim.fn.filereadable("CMakeLists.txt") == 1 then
        vim.api.nvim_buf_set_option(0, "makeprg", "cmake --build build")
    end
    if vim.fn.filereadable("build/build.ninja") == 1 then
        vim.api.nvim_buf_set_option(0, "makeprg", "ninja -C build")
    end
    if vim.fn.filereadable("makefile") == 1 or vim.fn.filereadable("Makefile") == 1 then
        vim.api.nvim_buf_set_option(0, "makeprg", "make")
    end
    if vim.fn.filereadable("platformio.ini") == 1 then
        vim.api.nvim_buf_set_option(0, "makeprg", "pio run")
    end
    if vim.fn.filereadable("WORKSPACE") == 1 then
        vim.keymap.set("n", "<f7>", ":Bazel build<CR>", { silent = true, buffer = true })
    end

    vim.keymap.set("n", "<C-a>", ":ClangdSwitchSourceHeader<CR>", { silent = true, buffer = true })
end)
-- }}}

-- Go {{{
create_augroup({ "go", "sql" }, function()
    vim.opt_local.shiftwidth = 4
    vim.opt_local.tabstop = 4
    vim.opt_local.expandtab = false
    vim.cmd([[ setlocal cpt-=t ]])

    if vim.fn.filereadable("makefile") == 1 or vim.fn.filereadable("Makefile") == 1 then
        vim.api.nvim_buf_set_option(0, "makeprg", "make")
    elseif vim.fn.filereadable("justfile") == 1 or vim.fn.filereadable("Justfile") == 1 then
        vim.api.nvim_buf_set_option(0, "makeprg", "just")
    else
        vim.api.nvim_buf_set_option(0, "makeprg", "go build .")
    end
end)

vim.filetype.add({
    extension = {
        templ = "templ",
    },
})
-- }}}

-- Zig {{{
vim.g.zig_fmt_autosave = 0
create_augroup("zig", function()
    vim.cmd([[ setlocal cpt-=t ]])
end)
-- }}}

-- Javascript {{{
create_augroup({ "javascript", "typescript", "typescriptreact" }, function()
    vim.cmd([[ setlocal cpt-=t ]])
end)
-- }}}

-- Ocaml {{{
create_augroup("ocaml", function()
    vim.cmd([[ setlocal cpt-=t ]])
    vim.opt_local.makeprg = "dune build"
end)
-- }}}

-- Odin {{{
create_augroup("odin", function()
    if vim.fn.filereadable("build.sh") == 1 and vim.fn.has("unix") then
        print("hello odin")
        vim.api.nvim_buf_set_option(0, "makeprg", "sh ./build.sh")
    elseif vim.fn.filereadable("build.bat") == 1 and not vim.fn.has("unix") then
        vim.api.nvim_buf_set_option(0, "makeprg", "./build.bat")
    else
        vim.api.nvim_buf_set_option(0, "makeprg", "odin build .")
    end
end)
-- }}}

-- Rust {{{
vim.g.cargo_makeprg_params = "check"
create_augroup("rust", function()
    vim.cmd([[ setlocal cpt-=t ]])
    vim.keymap.set("n", "<Leader>mR", ":CargoReload<CR>", { buffer = true, silent = false })
end)
-- }}}

-- Elixir {{{
create_augroup("elixir", function()
    vim.cmd([[ compiler exunit ]])
end)
-- }}}

-- WGSL {{{
create_augroup("wgsl", function()
    vim.opt_local.commentstring = "//\\ %s"
end)
-- }}}

-- Other filetypes {{{
vim.cmd([[
autocmd BufRead,BufNewFile *.wgsl set filetype=wgsl
autocmd BufRead,BufNewFile *.hlsl set filetype=hlsl
autocmd BufRead,BufNewFile Tiltfile set filetype=starlark
autocmd BufRead,BufNewFile Dockerfile.* set filetype=dockerfile
autocmd BufRead,BufNewFile *.ixx set filetype=cpp
autocmd BufRead,BufNewFile *.mxx set filetype=cpp
autocmd BufRead,BufNewFile *.mpp set filetype=cpp
autocmd BufRead,BufNewFile *.cppm set filetype=cpp
autocmd BufRead,BufNewFile *.slang set filetype=slang
autocmd BufRead,BufNewFile *.tofu set filetype=terraform

autocmd FileType hlsl setlocal commentstring=//\\ %s
autocmd FileType slang setlocal commentstring=//\\ %s
]])
-- }}}
