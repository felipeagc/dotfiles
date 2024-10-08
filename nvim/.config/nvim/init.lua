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

    {
        "hrsh7th/nvim-cmp",
        dependencies = {
            "hrsh7th/cmp-nvim-lsp",
            "hrsh7th/cmp-vsnip",
            "hrsh7th/vim-vsnip",
            "onsails/lspkind.nvim",
        },
        config = function()
            local cmp = require("cmp")
            local lspkind = require("lspkind")
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
                    ["<CR>"] = cmp.mapping.confirm({ select = true }),
                }),
                sources = cmp.config.sources({
                    { name = "nvim_lsp" },
                    { name = "vsnip" },
                }),
                formatting = {
                    format = lspkind.cmp_format({
                        mode = "symbol", -- show only symbol annotations
                        maxwidth = 50, -- prevent the popup from showing more than provided characters (e.g 50 will not show more than 50 characters)
                        -- can also be a function to dynamically calculate max width such as
                        -- maxwidth = function() return math.floor(0.45 * vim.o.columns) end,
                        ellipsis_char = "...", -- when popup menu exceed maxwidth, the truncated part would show ellipsis_char instead (must define maxwidth first)
                        show_labelDetails = true, -- show labelDetails in menu. Disabled by default

                        -- The function below will be called before any actual modifications from lspkind
                        -- so that you can provide more controls on popup customization. (See [#30](https://github.com/onsails/lspkind-nvim/pull/30))
                        before = function(entry, vim_item)
                            return vim_item
                        end,
                    }),
                },
            })
        end,
    },

    {
        "nvimtools/none-ls.nvim",
        dependencies = { "nvim-lua/plenary.nvim" },
        config = function()
            local null_ls = require("null-ls")
            null_ls.setup({
                sources = {
                    null_ls.builtins.formatting.stylua,
                    null_ls.builtins.formatting.sqlfluff.with({ extra_args = { "--dialect", "postgres" } }),
                    null_ls.builtins.diagnostics.swiftlint,
                    null_ls.builtins.diagnostics.sqlfluff.with({ extra_args = { "--dialect", "postgres" } }),
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
    },

    {
        "nvim-treesitter/nvim-treesitter",
        build = ":TSUpdate",
        config = function()
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
                    "glsl",
                    "go",
                    "graphql",
                    "haskell",
                    "heex",
                    "hlsl",
                    "html",
                    "http",
                    "java",
                    "javascript",
                    "latex",
                    "ledger",
                    "lua",
                    "make",
                    "markdown",
                    "ocaml",
                    "python",
                    "rust",
                    "scala",
                    "svelte",
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
                        "sql",
                        -- "html",
                        -- "htmldjango",
                    },
                },
                context_commentstring = {
                    enable = true,
                    disable = { "wgsl", "cpp", "c", "hlsl", "slang" },
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
        end,
    },
    { "nvim-treesitter/nvim-treesitter-context" },
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
    {
        "jellydn/hurl.nvim",
        dependencies = {
            "MunifTanjim/nui.nvim",
            "nvim-lua/plenary.nvim",
            "nvim-treesitter/nvim-treesitter",
        },
        ft = "hurl",
        opts = {
            debug = false,
            show_notification = true,
            auto_close = false,
            mode = "split",
            -- Default formatter
            formatters = {
                json = { "jq" }, -- Make sure you have install jq in your system, e.g: brew install jq
            },
        },
        keys = {
            -- Run API request
            -- { "<leader>A", "<cmd>HurlRunner<CR>", desc = "Run All requests" },
            { "<C-Return>", "<cmd>HurlRunnerAt<CR>", desc = "Run Api request" },
            -- { "<leader>te", "<cmd>HurlRunnerToEntry<CR>", desc = "Run Api request to entry" },
            -- { "<leader>tm", "<cmd>HurlToggleMode<CR>", desc = "Hurl Toggle Mode" },
            -- { "<leader>tv", "<cmd>HurlVerbose<CR>", desc = "Run Api in verbose mode" },
            -- Run Hurl request in visual mode
            -- { "<leader>h", ":HurlRunner<CR>", desc = "Hurl Runner", mode = "v" },
        },
    },

    "tpope/vim-surround",
    "tpope/vim-endwise",
    "tpope/vim-repeat",
    "tpope/vim-fugitive",
    "tpope/vim-abolish",
    "tpope/vim-unimpaired",
    "tpope/vim-dispatch",
    "tpope/vim-projectionist",
    "tpope/vim-commentary",
    {
        "kristijanhusak/vim-dadbod-ui",
        dependencies = {
            { "tpope/vim-dadbod", lazy = true },
            { "kristijanhusak/vim-dadbod-completion", ft = { "sql", "mysql", "plsql" }, lazy = true },
        },
        cmd = {
            "DBUI",
            "DBUIToggle",
            "DBUIAddConnection",
            "DBUIFindBuffer",
        },
        init = function()
            vim.g.db_ui_disable_mappings = 1
        end,
        config = function()
            vim.cmd([[
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
            ]])
        end,
    },
    "ntpeters/vim-better-whitespace", -- highlight trailing whitespace
    {
        "windwp/nvim-autopairs",
        event = "InsertEnter",
        opts = {
            disable_filetype = { "TelescopePrompt", "vim", "clojure" },
        },
    },
    {
        "NvChad/nvim-colorizer.lua",
        opts = {
            filetypes = { "typescript", "typescriptreact", "javascript", "javascriptreact", "css", "html" },
            user_default_options = {
                names = true, -- "Name" codes like Blue or blue
                css = true, -- Enable all CSS features: rgb_fn, hsl_fn, names, RGB, RRGGBB
                mode = "background", -- Set the display mode.
                tailwind = true, -- Enable tailwind colors
                virtualtext = "■",
                always_update = false,
            },
        },
    },

    {
        "stevearc/oil.nvim",
        dependencies = { "nvim-tree/nvim-web-devicons" },
        config = function()
            vim.keymap.set("n", "-", "<CMD>Oil<CR>", { desc = "Open parent directory" })
            require("oil").setup({
                default_file_explorer = true,
            })
        end,
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
                        javascriptreact = true,
                        typescript = true,
                        typescriptreact = true,
                        markdown = false,
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

    -- Language support
    { "alaviss/nim.nvim", ft = { "nim" } },
    { "ziglang/zig.vim", ft = { "zig" } },
    { "rust-lang/rust.vim", ft = { "rust" } },
    { "NoahTheDuke/vim-just", ft = { "just" } },
    { "elixir-editors/vim-elixir", ft = { "elixir" } },
    { "kaarmu/typst.vim", ft = { "typst" } },
    { "whonore/Coqtail", ft = { "coq" } },
    { "dcharbon/vim-flatbuffers", ft = { "fbs" } },
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
                        height = { padding = 0 },
                        width = { padding = 0 },
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

    {
        "someone-stole-my-name/yaml-companion.nvim",
        dependencies = { "nvim-telescope/telescope.nvim" },
        config = function()
            require("telescope").load_extension("yaml_schema")
            local yaml_cfg = require("yaml-companion").setup({
                builtin_matchers = {
                    kubernetes = { enabled = false },
                    cloud_init = { enabled = false },
                },
            })
            require("lspconfig")["yamlls"].setup(yaml_cfg)
        end,
    },

    -- {
    --     "neanias/everforest-nvim",
    --     version = false,
    --     lazy = false,
    --     priority = 1000,
    --     config = function()
    --         require("everforest").setup({
    --             transparent_background_level = 1,
    --             background = "hard",
    --         })
    --         vim.cmd.colorscheme("everforest")
    --     end,
    -- },
    {
        "catppuccin/nvim",
        name = "catppuccin",
        version = false,
        lazy = false,
        priority = 1000,
        config = function()
            require("catppuccin").setup({
                flavour = "mocha",
                -- transparent_background = true,
            })
            vim.cmd.colorscheme("catppuccin")
        end,
    },
    {
        "nvim-lualine/lualine.nvim",
        priority = 1000,
        dependencies = { "nvim-tree/nvim-web-devicons" },
        config = function()
            require("lualine").setup({
                theme = "catppuccin",
                options = {
                    component_separators = { left = "", right = "" },
                    section_separators = { left = "", right = "" },
                },
            })
        end,
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
vim.wo.foldmethod = "marker"
-- vim.wo.foldlevel = 0
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

vim.keymap.set("n", "<C-a>", ":A<CR>", { silent = true })

vim.keymap.set("n", "<A-p>", "<nop>", { silent = true })

vim.keymap.set("n", "<f7>", ":Make<CR>", { silent = true })
vim.keymap.set("n", "<Leader>mb", ":Make<CR>", { silent = true })
vim.keymap.set("n", "<A-r>", ":Make<CR>", { silent = true })

vim.keymap.set("n", "<Leader>tt", ":TestSuite<CR>", { silent = true })
vim.keymap.set("n", "<Leader>tf", ":TestFile<CR>", { silent = true })

vim.keymap.set("n", "<Leader>en", vim.diagnostic.goto_next, { silent = true })
vim.keymap.set("n", "<Leader>ep", vim.diagnostic.goto_prev, { silent = true })

-- Disable ex mode binding
vim.cmd([[map Q <Nop>]])
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

local function format_buffer(bufnr)
    local bufnr = bufnr or 0
    vim.lsp.buf.format({
        async = true,
        filter = function(client)
            if #vim.lsp.get_active_clients({ bufnr = bufnr, name = "null-ls" }) >= 1 then
                -- If null-ls is active, don't format with other clients
                return client.name == "null-ls"
            end
            if #vim.lsp.get_active_clients({ bufnr = bufnr, name = "biome" }) >= 1 then
                -- If null-ls is active, don't format with other clients
                return client.name == "biome"
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

        -- Toggle inlay hints
        local filter = { bufnr = ev.bufnr }
        vim.lsp.inlay_hint.enable(false, filter)
        vim.keymap.set("n", "<C-/>", function()
            vim.lsp.inlay_hint.enable(not vim.lsp.inlay_hint.is_enabled(filter), filter)
        end, opts)

        local active_clients = vim.lsp.get_active_clients()
        if client.name == "denols" then
            for _, client_ in pairs(active_clients) do
                -- stop tsserver if denols is already active
                if client_.name == "ts_ls" then
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
    -- ["nim_langserver"] = {},
    ["templ"] = {},
    -- ["lexical"] = { cmd = { "lexical" } },
    ["emmet_language_server"] = {},
    ["ansiblels"] = {},
    -- ["csharp_ls"] = {},
    ["clojure_lsp"] = {},
    ["svelte"] = {},
    ["tailwindcss"] = {
        filetypes = { "html", "typescriptreact", "javascriptreact" },
    },
    ["biome"] = {},
    ["zls"] = {},
    ["jdtls"] = {},
    ["denols"] = {
        root_dir = lspconfig.util.root_pattern("deno.json", "deno.jsonc"),
        init_options = { lint = true },
    },
    ["ts_ls"] = {
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
    ["slangd"] = {
        filetypes = { "slang" },
    },
    ["pyright"] = {},
}

local capabilities = vim.lsp.protocol.make_client_capabilities()
-- Enable file watcher support for LSP
capabilities.workspace.didChangeWatchedFiles.dynamicRegistration = true
for lsp, settings in pairs(servers) do
    settings.capabilities = capabilities
    lspconfig[lsp].setup(settings)
end
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
    autocmd FileType cpp setlocal shiftwidth=4 tabstop=4 expandtab
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
    autocmd FileType sql setlocal shiftwidth=4 tabstop=4 expandtab
    autocmd FileType svelte setlocal shiftwidth=2 tabstop=2 expandtab
    autocmd FileType javascript setlocal shiftwidth=2 tabstop=2 expandtab
    autocmd FileType javascriptreact setlocal shiftwidth=2 tabstop=2 expandtab
    autocmd FileType typescript setlocal shiftwidth=2 tabstop=2 expandtab
    autocmd FileType typescriptreact setlocal shiftwidth=2 tabstop=2 expandtab
    autocmd FileType zig setlocal shiftwidth=4 tabstop=4 expandtab
    autocmd FileType sql setlocal shiftwidth=4 tabstop=4 expandtab
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

-- Rust {{{
vim.g.cargo_makeprg_params = "check"
create_augroup("rust", function()
    vim.cmd([[ setlocal cpt-=t ]])
    vim.keymap.set("n", "<Leader>mR", ":CargoReload<CR>", { buffer = true, silent = false })
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
autocmd BufRead,BufNewFile *.slang set filetype=slang
]])

vim.cmd("autocmd FileType hlsl setlocal commentstring=//\\ %s")
vim.cmd("autocmd FileType slang setlocal commentstring=//\\ %s")
-- }}}
