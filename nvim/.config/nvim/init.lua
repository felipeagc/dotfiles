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

require("lazy").setup({
    "equalsraf/neovim-gui-shim",

    "williamboman/mason.nvim",
    "williamboman/mason-lspconfig.nvim",
    "neovim/nvim-lspconfig",

    "hrsh7th/cmp-nvim-lsp",
    "hrsh7th/nvim-cmp",
    "hrsh7th/cmp-vsnip",
    "hrsh7th/vim-vsnip",

    {
        "nvimtools/none-ls.nvim",
        dependencies = { "nvim-lua/plenary.nvim" },
        config = function()
            local null_ls = require("null-ls")
            null_ls.setup({
                sources = {
                    null_ls.builtins.formatting.biome,
                    null_ls.builtins.formatting.stylua,
                },
            })
        end,
    },

    { "j-hui/fidget.nvim", opts = {} },
    {
        "nvim-pack/nvim-spectre",
        config = function()
            require("spectre").setup({
                highlight = {
                    ui = "String",
                    search = "DiffDelete",
                    replace = "DiffAdd",
                },
            })
        end,
    },

    {
        "folke/which-key.nvim",
        event = "VeryLazy",
        init = function()
            vim.o.timeout = true
            vim.o.timeoutlen = 300
        end,
        opts = {
            -- your configuration comes here
            -- or leave it empty to use the default settings
            -- refer to the configuration section below
        },
    },

    { "nvim-treesitter/nvim-treesitter", build = ":TSUpdate" },
    "nvim-treesitter/playground",
    {
        "HiPhish/rainbow-delimiters.nvim",
        config = function()
            require("rainbow-delimiters.setup").setup({
                whitelist = { "clojure" },
            })
        end,
    },
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
    "tpope/vim-vinegar",
    "tpope/vim-projectionist",
    "tpope/vim-commentary",
    {
        "kristijanhusak/vim-dadbod-ui",
        dependencies = {
            { "tpope/vim-dadbod", lazy = true },
            { "kristijanhusak/vim-dadbod-completion", ft = { "sql", "mysql", "plsql" }, lazy = true },
        },
        cmd = {
            'DBUI',
            'DBUIToggle',
            'DBUIAddConnection',
            'DBUIFindBuffer',
        },
        init = function()
            vim.g.db_ui_disable_mappings = 1
        end,
        config = function()
            vim.cmd[[
                autocmd FileType sql,mysql,plsql lua require('cmp').setup.buffer({ sources = {{ name = 'vim-dadbod-completion' }} })

                autocmd FileType dbui nmap <buffer> o <Plug>(DBUI_SelectLine)
                autocmd FileType dbui nmap <buffer> <CR> <Plug>(DBUI_SelectLine)
                autocmd FileType dbui nmap <buffer> <2-LeftMouse> <Plug>(DBUI_SelectLine)

                autocmd FileType dbui nmap <buffer> S <Plug>(DBUI_SelectLineVsplit)
                autocmd FileType dbui nmap <buffer> R <Plug>(DBUI_Redraw)
                autocmd FileType dbui nmap <buffer> d <Plug>(DBUI_DeleteLine)
                autocmd FileType dbui nmap <buffer> A <Plug>(DBUI_AddConnection)
                autocmd FileType dbui nmap <buffer> H <Plug>(DBUI_ToggleDetails)
                autocmd FileType dbui nmap <buffer> r <Plug>(DBUI_RenameLine)
                autocmd FileType dbui nmap <buffer> q <Plug>(DBUI_Quit)

                autocmd FileType sql nmap <buffer> <Leader>W <Plug>(DBUI_SaveQuery)
                autocmd FileType sql nmap <buffer> <Leader>E <Plug>(DBUI_EditBindParameters)
                autocmd FileType sql nmap <buffer> <Leader>S <Plug>(DBUI_ExecuteQuery)
                autocmd FileType sql vmap <buffer> <Leader>S <Plug>(DBUI_ExecuteQuery)
            ]]
        end,
    },
    "ntpeters/vim-better-whitespace", -- highlight trailing whitespace
    {
        "windwp/nvim-autopairs",
        event = "InsertEnter",
        opts = {
            disable_filetype = { "TelescopePrompt", "vim", "clojure" },
        }, -- this is equalent to setup({}) function
    },

    {
        "lewis6991/gitsigns.nvim",
        config = function()
            require("gitsigns").setup()
        end,
    },

    {
        "zbirenbaum/copilot.lua",
        config = function()
            if vim.fn.executable("node") == nil then
                -- Don't load copilot if node is not installed
                return
            end

            vim.keymap.set("i", "<Tab>", function()
                if require("copilot.suggestion").is_visible() then
                    require("copilot.suggestion").accept()
                else
                    vim.api.nvim_feedkeys(vim.api.nvim_replace_termcodes("<Tab>", true, false, true), "n", false)
                end
            end, { desc = "Super Tab" })

            require("copilot").setup({
                suggestion = {
                    auto_trigger = true,
                    keymap = {
                        accept = "<s-tab>",
                        next = "<M-]>",
                        prev = "<M-[>",
                        dismiss = "<M-d>",
                    },
                    filetypes = {
                        go = true,
                        html = true,
                        htmldjango = true,
                        lua = true,
                        ocaml = true,
                        rust = true,
                        elixir = true,
                        clojure = true,
                        svelte = true,
                        javascript = true,
                        typescript = true,
                        typescriptreact = true,
                        ["*"] = true,
                    },
                },
            })
        end,
    },

    {
        "vim-test/vim-test",
        config = function()
            vim.g["test#strategy"] = "dispatch"
        end,
    },
    { "kdheepak/lazygit.nvim", dependencies = { "nvim-lua/plenary.nvim" } },

    "editorconfig/editorconfig-vim",
    "derekwyatt/vim-fswitch",

    -- Language support
    "plasticboy/vim-markdown",
    "ziglang/zig.vim",
    "rust-lang/rust.vim",
    "felipeagc/dusk.vim",
    "IndianBoy42/tree-sitter-just",
    "elixir-editors/vim-elixir",
    "kaarmu/typst.vim",
    "whonore/Coqtail",
    "PhilT/vim-fsharp",
    "slint-ui/vim-slint",
    {
        "scalameta/nvim-metals",
        ft = { "scala", "sbt", "java" },
        config = function(self, metals_config)
            local nvim_metals_group = vim.api.nvim_create_augroup("nvim-metals", { clear = true })
            vim.api.nvim_create_autocmd("FileType", {
                pattern = self.ft,
                callback = function()
                    require("metals").initialize_or_attach(metals_config)
                end,
                group = nvim_metals_group,
            })
        end,
    },
    {
        "Olical/conjure",
        ft = { "clojure" },
        init = function()
            vim.g["conjure#filetypes"] = { "clojure" }
            vim.g["conjure#mapping#enable_defaults"] = false
            vim.g["conjure#mapping#doc_word"] = false
        end,
        config = function()
            require("conjure.main").main()
            require("conjure.mapping")["on-filetype"]()
        end,
    },
    -- { "eraserhd/parinfer-rust", build = "cargo build --release" },
    { "gpanders/nvim-parinfer" },

    "nvim-lua/plenary.nvim",
    "nvim-telescope/telescope.nvim",

    {
        "stevearc/dressing.nvim",
        config = function()
            require("dressing").setup({
                input = { enabled = false },
                select = {
                    enabled = true,
                    backend = { "telescope" },
                },
            })
        end,
    },

    "someone-stole-my-name/yaml-companion.nvim",

    {
        "felipeagc/fleet-theme-nvim",
        -- dir = "~/code/lua/fleet-theme-nvim",
        -- config = function() vim.cmd("colorscheme fleet") end
    },
    {
        "savq/melange-nvim",
        config = function()
            vim.cmd("colorscheme melange")
        end,
    },
    {
        "ellisonleao/gruvbox.nvim",
        -- config = function()
        --     require("gruvbox").setup({
        --         contrast = "soft", -- can be "hard", "soft" or empty string
        --         overrides = {
        --             SignColumn = { link = "Normal" },
        --             GruvboxGreenSign = { bg = "" },
        --             GruvboxOrangeSign = { bg = "" },
        --             GruvboxPurpleSign = { bg = "" },
        --             GruvboxYellowSign = { bg = "" },
        --             GruvboxRedSign = { bg = "" },
        --             GruvboxBlueSign = { bg = "" },
        --             GruvboxAquaSign = { bg = "" },
        --         }
        --     })
        --     vim.cmd("colorscheme gruvbox")
        -- end
    },
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

vim.o.pumheight = 8 -- Completion menu height
vim.o.signcolumn = "yes:1" -- Configure minimum gutter width

vim.wo.number = false
-- vim.wo.cursorline = true
-- vim.wo.foldmethod = 'marker'
-- vim.wo.foldlevel = 0

vim.cmd([[
if exists('g:nvy')
	set guifont=Cascadia\ Code:h12
endif
]])
-- }}}

-- Keybinds {{{
vim.g.mapleader = " "

vim.keymap.set("n", "<C-p>", ":Telescope find_files<CR>", { silent = true })
vim.keymap.set("n", "<C-b>", ":Telescope buffers<CR>", { silent = true })
vim.keymap.set("n", "<Leader>fg", ":Telescope live_grep<CR>", { silent = true })
vim.keymap.set("n", "<Leader>fv", vim.cmd.Ex, { silent = true })

vim.keymap.set("n", "<leader>S", '<cmd>lua require("spectre").toggle()<CR>', {
    desc = "Toggle Spectre",
})

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

-- vim.keymap.set("n", "<Leader>gs", ":vertical Git<CR>", { silent = true })
vim.keymap.set("n", "<Leader>gs", ":LazyGitCurrentFile<CR>", { silent = true })

vim.keymap.set("n", "<C-a>", ":FSHere<CR>", { silent = true })

vim.keymap.set("n", "<A-p>", "<nop>", { silent = true })

vim.keymap.set("n", "<f7>", ":Make<CR>", { silent = true })
vim.keymap.set("n", "<Leader>mb", ":Make<CR>", { silent = true })
vim.keymap.set("n", "<A-r>", ":Make<CR>", { silent = true })

vim.keymap.set("n", "<Leader>tt", ":TestSuite<CR>", { silent = true })
vim.keymap.set("n", "<Leader>tf", ":TestFile<CR>", { silent = true })

-- Disable ex mode binding
vim.cmd([[map Q <Nop>]])
-- }}}

-- Telescope {{{
local actions = require("telescope.actions")
require("telescope").setup({
    defaults = {
        preview = true,
        mappings = {
            i = {
                ["<esc>"] = actions.close,
            },
        },
    },
})
-- }}}

-- LSP {{{
vim.diagnostic.config({
    virtual_text = false,
    signs = false,
    underline = true,
    update_in_insert = false,
    severity_sort = true,
})

require("mason").setup()
require("mason-lspconfig").setup()

local lspconfig = require("lspconfig")
local lsp_configs = require("lspconfig.configs")

if not lsp_configs.lexical then
    local lexical_config = {
        filetypes = { "elixir", "eelixir", "heex" },
        cmd = { vim.fn.expand("$HOME/Code/elixir/lexical/_build/dev/package/lexical/bin/start_lexical.sh") },
        settings = {},
    }
    lsp_configs.lexical = {
        default_config = {
            filetypes = lexical_config.filetypes,
            cmd = lexical_config.cmd,
            root_dir = function(fname)
                return lspconfig.util.root_pattern("mix.exs", ".git")(fname) or vim.loop.os_homedir()
            end,
            -- optional settings
            settings = lexical_config.settings,
        },
    }
end

local function format_buffer(bufnr)
    local bufnr = bufnr or 0
    vim.lsp.buf.format({
        async = true,
        filter = function(client)
            if #vim.lsp.get_active_clients({ bufnr = bufnr, name = "null-ls" }) >= 1 then
                -- If null-ls is active, don't format with other clients
                return client.name == "null-ls"
            end
            return true
        end,
    })
end

vim.api.nvim_create_autocmd("LspAttach", {
    group = vim.api.nvim_create_augroup("UserLspConfig", {}),
    callback = function(ev)
        -- buf_set_option('omnifunc', 'v:lua.vim.lsp.omnifunc')

        local opts = { remap = false, silent = true, buffer = ev.bufnr }

        vim.keymap.set("n", "K", function()
            vim.lsp.buf.hover()
        end, opts)
        vim.keymap.set("n", "<c-]>", function()
            vim.lsp.buf.definition()
        end, opts)
        vim.keymap.set("n", "<leader>mf", format_buffer, opts)
        vim.keymap.set("n", "<leader>mr", function()
            vim.lsp.buf.rename()
        end, opts)
        vim.keymap.set("n", "<leader>mR", ":LspRestart<CR>", opts)
        vim.keymap.set("n", "<M-CR>", function()
            vim.lsp.buf.code_action()
        end, opts)
        vim.keymap.set("i", "<M-CR>", function()
            vim.lsp.buf.code_action()
        end, opts)
        vim.keymap.set("n", "<leader>mc", ":Copilot toggle<CR>", opts)
        vim.keymap.set("n", "[d", function()
            vim.diagnostic.goto_prev()
        end, opts)
        vim.keymap.set("n", "]d", function()
            vim.diagnostic.goto_next()
        end, opts)
        vim.keymap.set("n", "<C-y>", function()
            vim.diagnostic.open_float()
        end, opts)
        vim.keymap.set("i", "<C-h>", function()
            vim.lsp.buf.signature_help()
        end, opts)

        local client = vim.lsp.get_client_by_id(ev.data.client_id)
        local active_clients = vim.lsp.get_active_clients()
        if client.name == "denols" then
            for _, client_ in pairs(active_clients) do
                -- stop tsserver if denols is already active
                if client_.name == "tsserver" then
                    client_.stop()
                end
            end
        elseif client.name == "tsserver" then
            for _, client_ in pairs(active_clients) do
                -- prevent tsserver from starting if denols is already active
                if client_.name == "denols" then
                    client.stop()
                end
            end
        end

        -- workaround to hl semanticTokens
        -- https://github.com/golang/go/issues/54531#issuecomment-1464982242
        if client.name == "gopls" and not client.server_capabilities.semanticTokensProvider then
            local semantic = client.config.capabilities.textDocument.semanticTokens
            client.server_capabilities.semanticTokensProvider = {
                full = true,
                legend = { tokenModifiers = semantic.tokenModifiers, tokenTypes = semantic.tokenTypes },
                range = true,
            }
        end

        -- Disable semantic highlighting
        client.server_capabilities.semanticTokensProvider = nil
    end,
})

local servers = {
    ["templ"] = {},
    ["lexical"] = {},
    ["emmet_language_server"] = {},
    ["ansiblels"] = {},
    ["csharp_ls"] = {},
    ["clojure_lsp"] = {},
    ["dartls"] = {},
    ["ocamllsp"] = {},
    ["svelte"] = {},
    ["tailwindcss"] = {},
    ["wgsl_analyzer"] = {},
    ["zls"] = {},
    ["jdtls"] = {},
    ["fsautocomplete"] = {},
    ["slint_lsp"] = {},
    ["denols"] = {
        root_dir = lspconfig.util.root_pattern("deno.json", "deno.jsonc"),
        init_options = { lint = true },
    },
    ["tsserver"] = {
        root_dir = lspconfig.util.root_pattern("package.json"),
        init_options = { lint = true },
    },
    ["rust_analyzer"] = {
        settings = {
            ["rust-analyzer"] = {
                completion = { fullFunctionSignatures = { enable = true } },
                cachePriming = { enable = false },
            },
        },
    },
    ["gopls"] = {
        settings = {
            gopls = {
                semanticTokens = true,
            },
        },
    },
    ["clangd"] = {
        filetypes = { "c", "cpp", "objc", "objcpp", "cuda" },
    },
}

local capabilities = vim.lsp.protocol.make_client_capabilities()
-- Enable file watcher support for LSP
capabilities.workspace.didChangeWatchedFiles.dynamicRegistration = true
for lsp, settings in pairs(servers) do
    settings.capabilities = capabilities
    lspconfig[lsp].setup(settings)
end
-- }}}

-- Nvim-cmp {{{
local cmp = require("cmp")

cmp.setup({
    snippet = {
        expand = function(args)
            vim.fn["vsnip#anonymous"](args.body)
        end,
    },
    window = {},
    mapping = cmp.mapping.preset.insert({
        ["<C-b>"] = cmp.mapping.scroll_docs(-4),
        ["<C-f>"] = cmp.mapping.scroll_docs(4),
        ["<C-Space>"] = cmp.mapping.complete(),
        ["<C-e>"] = cmp.mapping.abort(),
        ["<CR>"] = cmp.mapping.confirm({ select = true }), -- Accept currently selected item. Set `select` to `false` to only confirm explicitly selected items.
    }),
    sources = cmp.config.sources({
        { name = "nvim_lsp" },
        { name = "vsnip" },
    }),
})
-- }}}

-- YAML companion {{{
require("telescope").load_extension("yaml_schema")
local yaml_cfg = require("yaml-companion").setup({
    builtin_matchers = {
        kubernetes = { enabled = false },
        cloud_init = { enabled = false },
    },
})
require("lspconfig")["yamlls"].setup(yaml_cfg)
-- }}}

-- Color scheme {{{
vim.cmd([[set background=dark]])
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
vim.cmd([[
    function! Mkdir()
        let dir = expand('%:p:h')

        if !isdirectory(dir)
            call mkdir(dir, 'p')
            echo 'Created non-existing directory: '.dir
        endif
    endfunction

    augroup on_buffer_write
        autocmd BufWritePre * call Mkdir()
    augroup END
]])

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
    autocmd FileType cpp setlocal shiftwidth=4 tabstop=4 expandtab
    autocmd FileType lua setlocal shiftwidth=4 tabstop=4 expandtab
    autocmd FileType glsl setlocal shiftwidth=4 tabstop=4 expandtab
    autocmd FileType haskell setlocal shiftwidth=2 tabstop=2 expandtab
    autocmd FileType ocaml setlocal shiftwidth=2 tabstop=2 expandtab
    autocmd FileType dart setlocal shiftwidth=2 tabstop=2 expandtab
    autocmd FileType zig setlocal shiftwidth=4 tabstop=4 expandtab
    autocmd FileType javascript setlocal shiftwidth=2 tabstop=2 expandtab
    autocmd FileType typescript setlocal shiftwidth=2 tabstop=2 expandtab
    autocmd FileType typescriptreact setlocal shiftwidth=2 tabstop=2 expandtab
    autocmd FileType rust setlocal shiftwidth=4 tabstop=4 expandtab
    autocmd FileType svelte setlocal shiftwidth=2 tabstop=2 expandtab
    autocmd FileType html setlocal shiftwidth=2 tabstop=2 expandtab
    autocmd FileType htmldjango setlocal shiftwidth=2 tabstop=2 expandtab
    autocmd FileType scala setlocal shiftwidth=2 tabstop=2 expandtab
    autocmd FileType sbt setlocal shiftwidth=2 tabstop=2 expandtab
]])
-- }}}

-- Completion {{{
-- vim.cmd [[inoremap <silent> <C-n> <C-x><C-o>]]
-- }}}

-- Treesitter {{{
require("nvim-treesitter.parsers").get_parser_configs().fsharp = {
    install_info = {
        url = "https://github.com/Nsidorenco/tree-sitter-fsharp",
        branch = "develop",
        files = { "src/scanner.cc", "src/parser.c" },
        generate_requires_npm = true,
        requires_generate_from_grammar = true,
    },
    filetype = "fsharp",
}

require("nvim-treesitter.configs").setup({
    -- Install parsers synchronously (only applied to `ensure_installed`)
    sync_install = false,

    -- Automatically install missing parsers when entering buffer
    -- Recommendation: set to false if you don't have `tree-sitter` CLI installed locally
    auto_install = true,

    ensure_installed = {
        "bash",
        "c",
        "c_sharp",
        "clojure",
        "cmake",
        "cpp",
        "css",
        "dart",
        "elixir",
        "fsharp",
        "glsl",
        "go",
        "graphql",
        "haskell",
        "heex",
        "hlsl",
        "html",
        "java",
        "javascript",
        "latex",
        "lua",
        "make",
        "markdown",
        "ocaml",
        "python",
        "rust",
        "slint",
        "svelte",
        "scala",
        "templ",
        "tsx",
        "typescript",
        "wgsl",
        "yaml",
        "zig",
    },
    -- ignore_install = { "javascript" }, -- List of parsers to ignore installing

    highlight = {
        enable = true, -- false will disable the whole extension
        disable = {
            -- "cpp",
        }, -- list of language that will be disabled
        -- Setting this to true will run `:h syntax` and tree-sitter at the same time.
        -- Set this to `true` if you depend on 'syntax' being enabled (like for indentation).
        -- Using this option may slow down your editor, and you may see some duplicate highlights.
        -- Instead of true it can also be a list of languages
        additional_vim_regex_highlighting = false,
    },
    indent = {
        enable = true,
        disable = {
            "c",
            "cpp",
            "haskell",
            "latex",
            "ocaml",
            "python",
            "slint",
            -- "html",
            -- "htmldjango",
        },
    },
    context_commentstring = {
        enable = true,
        disable = { "wgsl" },
    },
    incremental_selection = {
        enable = true,
        keymaps = {
            init_selection = "<A-n>", -- set to `false` to disable one of the mappings
            node_incremental = "<A-n>",
            -- scope_incremental = "grc",
            node_decremental = "<A-p>",
        },
    },
})
-- }}}

-- Markdown {{{
vim.g.vim_markdown_folding_disabled = 1
-- }}}

-- C/C++ {{{
vim.g.bazel_make_command = "Make"
vim.g.compiler_gcc_ignore_unmatched_lines = 1
-- vim.g.cpp_no_cpp17 = 1

create_augroup({ "c", "cpp" }, function()
    vim.cmd([[ setlocal cpt-=t ]])

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

-- Rust {{{
vim.g.cargo_makeprg_params = "check"
create_augroup("rust", function()
    vim.cmd([[ setlocal cpt-=t ]])
    vim.keymap.set("n", "<Leader>mR", ":CargoReload<CR>", { buffer = true, silent = false })
end)
-- }}}

-- Dart {{{
vim.g.dart_style_guide = "2"

function flutter_hot_reload()
    vim.cmd([[silent execute '!kill -SIGUSR1 $(pgrep -f "[f]lutter_tool.*run")']])
end

create_augroup("dart", function()
    vim.cmd([[ setlocal cpt-=t ]])
    vim.keymap.set("n", "<f7>", ":lua flutter_hot_reload()<CR>", { buffer = true, silent = true })
end)
-- }}}

-- Latex {{{
function open_pdf_preview()
    local buffer_path = vim.fn.expand("%")
    local pdf_path = string.gsub(buffer_path, "%.[^/]+$", ".pdf")
    print(pdf_path)
    vim.cmd("silent !zathura " .. pdf_path .. " & disown")
end

create_augroup("tex", function()
    vim.opt_local.makeprg = "latexmk"
    vim.keymap.set("n", "<Leader>mp", ":lua open_pdf_preview()<CR>", { buffer = true, silent = true })
end)
-- }}}

-- Clojure {{{
create_augroup("clojure", function()
    vim.keymap.set("n", "<C-Return>", ":ConjureEvalCurrentForm<CR>", { buffer = true, silent = true })
    vim.keymap.set("i", "<C-Return>", "<C-o>:ConjureEvalCurrentForm<CR>", { buffer = true, silent = true })
    vim.keymap.set("n", "<f7>", ":ConjureEvalBuf<CR>", { buffer = true, silent = true, remap = false })
    vim.keymap.set("i", "<f7>", ":ConjureEvalBuf<CR>", { buffer = true, silent = true, remap = false })
    vim.keymap.set("n", "<leader>mb", ":ConjureEvalBuf<CR>", { buffer = true, silent = true, remap = false })
end)
-- }}}

-- Elixir {{{
create_augroup("elixir", function()
    vim.cmd([[ compiler exunit ]])
end)
-- }}}

-- Swift {{{
create_augroup("swift", function()
    vim.opt_local.makeprg = "xcodebuild"
end)
-- }}}

-- WGSL {{{
create_augroup("wgsl", function()
    vim.opt_local.commentstring = "//\\ %s"
end)
-- }}}

-- Coq {{{
vim.g.coqtail_nomap = 1

create_augroup("coq", function()
    vim.keymap.set("n", "<Leader>n", ":CoqNext<CR>", { buffer = true, silent = true })
    vim.keymap.set("n", "<Leader>p", ":CoqUndo<CR>", { buffer = true, silent = true })
    vim.keymap.set("n", "<Leader>l", ":CoqToLine<CR>", { buffer = true, silent = true })
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
autocmd BufRead,BufNewFile *.slang set filetype=hlsl
]])
-- }}}
