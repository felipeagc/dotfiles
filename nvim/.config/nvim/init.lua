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

local function create_augroup(groupname, filetype, commands)
    local group = vim.api.nvim_create_augroup(groupname, {})
    for _, command in ipairs(commands) do
        vim.api.nvim_create_autocmd({ "FileType" }, {
            pattern = filetype,
            command = command,
            group = group,
        })
    end
end

-- }}}

require("lazy").setup({
    'equalsraf/neovim-gui-shim',

    'neovim/nvim-lspconfig',
    'hrsh7th/nvim-cmp',
    'hrsh7th/cmp-nvim-lsp',
    'hrsh7th/cmp-buffer',
    'hrsh7th/cmp-path',
    'hrsh7th/cmp-cmdline',
    'hrsh7th/cmp-vsnip',
    'hrsh7th/vim-vsnip',

    { 'nvim-treesitter/nvim-treesitter', build = ":TSUpdate" },
    'nvim-treesitter/playground',
    'JoosepAlviste/nvim-ts-context-commentstring',
    'numToStr/Comment.nvim',

    'mfussenegger/nvim-dap',
    'rcarriga/nvim-dap-ui',
    'leoluz/nvim-dap-go',

    'tpope/vim-surround',
    'tpope/vim-endwise',
    'tpope/vim-repeat',
    'tpope/vim-fugitive',
    'tpope/vim-abolish',
    'tpope/vim-unimpaired',
    'tpope/vim-dispatch',
    'tpope/vim-vinegar',
    'tpope/vim-projectionist',

    {
        'lewis6991/gitsigns.nvim',
        config = function() require('gitsigns').setup() end
    },

    {
        'zbirenbaum/copilot.lua',
        config = function()
            require("copilot").setup({
                suggestion = {
                    auto_trigger = false,
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
                        javascript = true,
                        lua = true,
                        ocaml = true,
                        rust = true,
                        svelte = true,
                        typescript = true,
                        typescriptreact = true,
                        elixir = true,
                        ["*"] = false,
                    },
                }
            })
        end
    },

    {
        'vim-test/vim-test',
        config = function() vim.g["test#strategy"] = "dispatch" end
    },
    { 'kdheepak/lazygit.nvim', dependencies = { 'nvim-lua/plenary.nvim' } },

    'editorconfig/editorconfig-vim',
    'derekwyatt/vim-fswitch',

    'plasticboy/vim-markdown',
    'ziglang/zig.vim',
    'rust-lang/rust.vim',
    'LnL7/vim-nix',
    'felipeagc/dusk.vim',
    'IndianBoy42/tree-sitter-just',
    'elixir-editors/vim-elixir',
    'whonore/Coqtail',

    'nvim-lua/plenary.nvim',
    'nvim-telescope/telescope.nvim',
    'nvim-telescope/telescope-dap.nvim',

    {
        'stevearc/dressing.nvim',
        config = function()
            require('dressing').setup({
                input = { enabled = false },
                select = {
                    enabled = true,
                    backend = { "telescope" },
                },
            })
        end
    },

    -- For visualizing colors
    {
        'NvChad/nvim-colorizer.lua',
        config = function()
            require('colorizer').setup({
                user_default_options = {
                    tailwind = true,
                }
            })
        end
    },
    {
        'roobert/tailwindcss-colorizer-cmp.nvim',
        -- optionally, override the default options:
        config = function()
            require('tailwindcss-colorizer-cmp').setup({
                color_square_width = 2,
            })
        end
    },

    -- {
    --     "felipeagc/fleet-theme-nvim",
    --     -- dir = "~/code/lua/fleet-theme-nvim",
    --     -- config = function() vim.cmd("colorscheme fleet") end
    -- },
    {
        "ellisonleao/gruvbox.nvim",
        config = function()
            require("gruvbox").setup({
                contrast = "hard", -- can be "hard", "soft" or empty string
                overrides = {
                    SignColumn = { link = "Normal" },
                    GruvboxGreenSign = { bg = "" },
                    GruvboxOrangeSign = { bg = "" },
                    GruvboxPurpleSign = { bg = "" },
                    GruvboxYellowSign = { bg = "" },
                    GruvboxRedSign = { bg = "" },
                    GruvboxBlueSign = { bg = "" },
                    GruvboxAquaSign = { bg = "" },
                }
            })
            vim.cmd("colorscheme gruvbox")
        end
    },
    -- {
    --     'rebelot/kanagawa.nvim',
    --     config = function()
    --         require('kanagawa').setup({
    --             compile = false,
    --             undercurl = true, -- enable undercurls
    --             commentStyle = { italic = false },
    --             functionStyle = {},
    --             keywordStyle = { italic = false },
    --             statementStyle = { bold = true },
    --             typeStyle = {},
    --             transparent = false,   -- do not set background color
    --             dimInactive = false,   -- dim inactive window `:h hl-NormalNC`
    --             terminalColors = true, -- define vim.g.terminal_color_{0,17}
    --             colors = {},
    --             overrides = function(colors) return {} end,
    --         })
    --     end
    -- },
})

-- Vim options {{{
vim.o.termguicolors = true

vim.o.exrc = true
vim.o.showcmd = true
vim.o.mouse = 'a'

vim.o.scrolloff = 5
vim.o.clipboard = 'unnamedplus'
vim.o.completeopt = 'menu,menuone,noselect'

vim.o.tabstop = 4
vim.o.softtabstop = 4
vim.o.shiftwidth = 4
vim.o.autoindent = true
vim.o.smartindent = true
vim.o.smarttab = true
vim.o.expandtab = true

vim.o.wrap = true
vim.o.wildignore = vim.o.wildignore ..
    '*.so,*.swp,*.zip,*.o,*.png,*.jpg,*.jpeg,*/target/*,*/build/*,*/node_modules/*,tags,*.glb,*.gltf,*.hdr'
vim.o.hidden = true
vim.o.showmode = false
vim.o.modeline = true
vim.o.modelines = 1

vim.o.undofile = true
vim.o.undodir = vim.fn.stdpath('data') .. '/undodir'

vim.o.linebreak = true

vim.o.showbreak = '>   '
vim.o.shortmess = vim.o.shortmess .. 'c'

vim.o.ignorecase = true
vim.o.smartcase = true

vim.o.cinoptions = vim.o.cinoptions .. 'L0'
vim.o.cinoptions = vim.o.cinoptions .. 'l1'

vim.wo.number = false
-- vim.wo.cursorline = true
-- vim.wo.foldmethod = 'marker'
-- vim.wo.foldlevel = 0
vim.cmd[[
if exists('g:nvy')
	set guifont=Cascadia\ Code:h12
endif
]]
-- }}}

-- Keybinds {{{
vim.g.mapleader = " "

vim.keymap.set("n", "<C-p>", ":Telescope find_files<CR>", { silent = true })
vim.keymap.set("n", "<C-b>", ":Telescope buffers<CR>", { silent = true })
vim.keymap.set("n", "<Leader>fg", ":Telescope live_grep<CR>", { silent = true })
vim.keymap.set("n", "<Leader>fv", vim.cmd.Ex, { silent = true })

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

vim.keymap.set("n", "<Leader>do", ":lua require('dapui').toggle()<CR>", { silent = true })
vim.keymap.set("n", "<Leader>dc", ":lua require('dap').continue()<CR>", { silent = true })
vim.keymap.set("n", "<Leader>db", ":lua require('dap').toggle_breakpoint()<CR>", { silent = true })
vim.keymap.set("n", "<Leader>ds", ":lua require('dap').step_into()<CR>", { silent = true })
vim.keymap.set("n", "<Leader>dn", ":lua require('dap').step_over()<CR>", { silent = true })
vim.keymap.set("n", "<Leader>df", ":lua require('dap').step_out()<CR>", { silent = true })
vim.keymap.set("n", "<Leader>dq", ":lua require('dap').terminate()<CR>", { silent = true })

-- Disable ex mode binding
vim.cmd [[map Q <Nop>]]
-- }}}

-- Telescope {{{
local actions = require("telescope.actions")
require('telescope').setup({
    defaults = {
        preview = true,
        -- layout_strategy = 'bottom_pane',
        -- border = true,
        -- borderchars = {
        --     results = { "─", "", "", "", "", "", "", "" },
        --     prompt = { "", "", "", "", "", "", "", "" },
        -- },
        -- layout_config = {
        --     -- vertical = { width = 0.5 }
        --     vertical = { width = 0.2 },
        --     horizontal = { height = 0.2 },
        --     bottom_pane = {
        --         height = 8,
        --     },
        --     prompt_position = 'bottom',
        --     -- other layout configuration here
        -- },
        mappings = {
            i = {
                ["<esc>"] = actions.close
            },
        },
    },
    -- pickers = {
    --     find_files = { theme = "ivy" },
    --     buffers = { theme = "ivy" },
    --     live_grep = { theme = "ivy" },
    -- },
})
require('telescope').load_extension('dap')
-- }}}

-- LSP {{{
vim.diagnostic.config {
    virtual_text = {
        severity = {
            -- min = vim.diagnostic.severity.ERROR,
        },
    },
    signs = false,
    underline = true,
    update_in_insert = false,
    severity_sort = true,
}

local lspconfig = require("lspconfig")

vim.api.nvim_create_autocmd('LspAttach', {
    group = vim.api.nvim_create_augroup('UserLspConfig', {}),
    callback = function(ev)
        -- buf_set_option('omnifunc', 'v:lua.vim.lsp.omnifunc')

        local opts = { remap = false, silent = true, buffer = ev.bufnr }

        vim.keymap.set('n', 'K', function() vim.lsp.buf.hover() end, opts)
        vim.keymap.set('n', '<c-]>', function() vim.lsp.buf.definition() end, opts)
        vim.keymap.set('n', '<leader>mf', function() vim.lsp.buf.format({ async = true }) end, opts)
        vim.keymap.set('n', '<leader>mr', function() vim.lsp.buf.rename() end, opts)
        vim.keymap.set('n', '<leader>mR', ":LspRestart<CR>", opts)
        vim.keymap.set('n', '<leader>mi', function() vim.lsp.buf.code_action() end, opts)
        vim.keymap.set('n', '<leader>mc', ":Copilot toggle<CR>", opts)
        vim.keymap.set('n', '[d', function() vim.diagnostic.goto_prev() end, opts)
        vim.keymap.set('n', ']d', function() vim.diagnostic.goto_next() end, opts)
        vim.keymap.set('n', '<C-y>', function() vim.diagnostic.open_float() end, opts)
        vim.keymap.set('i', '<C-h>', function() vim.lsp.buf.signature_help() end, opts)

        local client = vim.lsp.get_client_by_id(ev.data.client_id)
        local active_clients = vim.lsp.get_active_clients()
        if client.name == 'denols' then
            for _, client_ in pairs(active_clients) do
                -- stop tsserver if denols is already active
                if client_.name == 'tsserver' then
                    client_.stop()
                end
            end
        elseif client.name == 'tsserver' then
            for _, client_ in pairs(active_clients) do
                -- prevent tsserver from starting if denols is already active
                if client_.name == 'denols' then
                    client.stop()
                end
            end
        end

        -- workaround to hl semanticTokens
        -- https://github.com/golang/go/issues/54531#issuecomment-1464982242
        if client.name == 'gopls' and not client.server_capabilities.semanticTokensProvider then
            local semantic = client.config.capabilities.textDocument.semanticTokens
            client.server_capabilities.semanticTokensProvider = {
                full = true,
                legend = { tokenModifiers = semantic.tokenModifiers, tokenTypes = semantic.tokenTypes },
                range = true,
            }
        end

        -- Disable semantic highlighting
        client.server_capabilities.semanticTokensProvider = nil
    end
})

local capabilities = require('cmp_nvim_lsp').default_capabilities()
capabilities.textDocument.completion.completionItem.snippetSupport = false

local servers = {
    "ansiblels",
    "csharp_ls",
    "clojure_lsp",
    "dartls",
    "eslint",
    "hls",
    "metals",
    "ocamllsp",
    "slint_lsp",
    "svelte",
    "tailwindcss",
    "wgsl_analyzer",
    "zls",
}
for _, lsp in ipairs(servers) do
    lspconfig[lsp].setup {
        capabilities = capabilities,
    }
end

lspconfig.denols.setup {
    capabilities = capabilities,
    root_dir = lspconfig.util.root_pattern("deno.json", "deno.jsonc"),
    init_options = { lint = true },
}

lspconfig.tsserver.setup {
    capabilities = capabilities,
    init_options = { lint = true },
    root_dir = lspconfig.util.root_pattern("package.json"),
}

lspconfig.rust_analyzer.setup {
    capabilities = capabilities,
    settings = {
        ["rust-analyzer"] = {
            cargo = { buildScripts = { enable = true } },
        },
    },
}

lspconfig.gopls.setup {
    capabilities = capabilities,
    settings = {
        gopls = {
            semanticTokens = true,
        },
    },
}

lspconfig.clangd.setup {
    capabilities = capabilities,
    filetypes = { "c", "cpp", "objc", "objcpp", "cuda" },
}

lspconfig.elixirls.setup {
    capabilities = capabilities,
    cmd = { "elixir-ls" },
    settings = {
        -- elixirLS = { dialyzerEnabled = false },
    },
}

-- lspconfig.lua_ls.setup {
--     capabilities = capabilities,
--     settings = {
--         Lua = {
--             runtime = { version = 'LuaJIT' },
--             diagnostics = { globals = { 'vim' } },
--             workspace = { library = vim.api.nvim_get_runtime_file("", true) },
--             telemetry = { enable = false },
--         },
--     },
-- }

-- Lexical LSP server config
-- local configs = require("lspconfig.configs")
-- local lexical_config = {
--     filetypes = { "elixir", "eelixir", },
--     cmd = { vim.env.HOME.."/Code/elixir/lexical/_build/dev/package/lexical/bin/start_lexical.sh" },
--     settings = {},
-- }
-- if not configs.lexical then
--     configs.lexical = {
--         default_config = {
--             filetypes = lexical_config.filetypes,
--             cmd = lexical_config.cmd,
--             root_dir = function(fname)
--                 return lspconfig.util.root_pattern("mix.exs", ".git")(fname) or vim.loop.os_homedir()
--             end,
--             -- optional settings
--             settings = lexical_config.settings,
--         },
--     }
-- end
--
-- lspconfig.lexical.setup({})
-- }}}

-- DAP {{{
local dap = require("dap")
dap.adapters.lldb = {
    type = 'executable',
    command = '/usr/bin/lldb-vscode', -- adjust as needed, must be absolute path
    name = 'lldb'
}

dap.configurations.cpp = {
    {
        name = 'Launch',
        type = 'lldb',
        request = 'launch',
        program = function()
            return vim.fn.input('Path to executable: ', vim.fn.getcwd() .. '/', 'file')
        end,
        cwd = '${workspaceFolder}',
        stopOnEntry = false,
        args = function()
            local s = vim.fn.input('Arguments: ', '')
            local words = {}
            for word in s:gmatch("%S+") do table.insert(words, word) end
            return words
        end,
    },
}

dap.configurations.c = dap.configurations.cpp
dap.configurations.rust = dap.configurations.cpp

require('dap-go').setup()
require('dapui').setup({
    icons = { expanded = "▾", collapsed = "▸" },
    mappings = {
        -- Use a table to apply multiple mappings
        expand = { "<CR>", "<2-LeftMouse>" },
        open = "o",
        remove = "d",
        edit = "e",
        repl = "r",
        toggle = "t",
    },
    -- Expand lines larger than the window
    -- Requires >= 0.7
    expand_lines = vim.fn.has("nvim-0.7"),
    layouts = {
        {
            -- You can change the order of elements in the sidebar
            elements = {
                -- Provide as ID strings or tables with "id" and "size" keys
                {
                    id = "scopes",
                    size = 0.25, -- Can be float or integer > 1
                },
                { id = "breakpoints", size = 0.25 },
                { id = "stacks",      size = 0.25 },
                { id = "watches",     size = 00.25 },
            },
            size = 40,
            position = "left", -- Can be "left", "right", "top", "bottom"
        },
        {
            elements = { "repl" },
            size = 10,
            position = "bottom", -- Can be "left", "right", "top", "bottom"
        },
    },
    floating = {
        max_height = nil,  -- These can be integers or a float between 0 and 1.
        max_width = nil,   -- Floats will be treated as percentage of your screen.
        border = "single", -- Border style. Can be "single", "double" or "rounded"
        mappings = {
            close = { "q", "<Esc>" },
        },
    },
    windows = { indent = 1 },
    render = {
        max_type_length = nil, -- Can be integer or nil.
    }
})
-- }}}

-- Visual packages {{{
-- require("gitsigns").setup{}
-- }}}

-- Color scheme {{{
vim.cmd [[set background=dark]]
-- }}}

-- Small quality of life stuff {{{

-- Remap :W to :w
vim.cmd [[
	cnoreabbrev W w
	cnoreabbrev Q q
	cnoreabbrev Wq wq
	cnoreabbrev WQ wq
]]

-- Clear highlights with escape
vim.cmd [[
	nnoremap <silent> <esc> :noh<return><esc>
]]

-- Create non-existing directories before writing buffer
vim.cmd [[
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
]]

-- Open help vertically
vim.cmd [[
    autocmd FileType help wincmd L
    autocmd FileType man wincmd L
]]

--  Fix false positive C bracket error
vim.cmd [[
    let c_no_bracket_error=1
    let c_no_curly_error=1
]]

-- Quickfix helpers
vim.cmd [[
    command! CNext try | cnext | catch | clast | catch | endtry
    command! CPrev try | cprev | catch | cfirst | catch | endtry
]]
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

local cmp = require("cmp")
cmp.setup {
    preselect = cmp.PreselectMode.None,
    view = {
        entries = "custom" -- can be "custom", "wildmenu" or "native"
    },
    snippet = {
        expand = function(args) vim.fn["vsnip#anonymous"](args.body) end,
    },
    completion = {
        autocomplete = false,
        completeopt = 'menu,menuone,noselect',
    },
    formatting = {
        format = function(entry, vim_item)
            vim_item.menu = ({
                buffer = "[Buffer]",
                nvim_lsp = "[LSP]",
                luasnip = "[LuaSnip]",
                nvim_lua = "[Lua]",
                latex_symbols = "[LaTeX]",
            })[entry.source.name]
            return vim_item
        end,
    },
    mapping = cmp.mapping.preset.insert({
        ['<C-n>'] = cmp.mapping({
            c = function()
                if cmp.visible() then
                    cmp.select_next_item({ behavior = cmp.SelectBehavior.Select })
                else
                    cmp.complete()
                end
            end,
            i = function()
                if cmp.visible() then
                    cmp.select_next_item({ behavior = cmp.SelectBehavior.Select })
                else
                    cmp.complete()
                end
            end
        }),
        ['<C-p>'] = cmp.mapping({
            c = function()
                if cmp.visible() then
                    cmp.select_prev_item({ behavior = cmp.SelectBehavior.Select })
                else
                    cmp.complete()
                end
            end,
            i = function()
                if cmp.visible() then
                    cmp.select_prev_item({ behavior = cmp.SelectBehavior.Select })
                else
                    cmp.complete()
                end
            end
        }),
        ['<CR>'] = cmp.mapping({
            i = cmp.mapping.confirm({ behavior = cmp.ConfirmBehavior.Replace, select = false }),
            c = function(fallback)
                if cmp.visible() then
                    cmp.confirm({ behavior = cmp.ConfirmBehavior.Replace, select = false })
                else
                    fallback()
                end
            end
        }),
    }),
    sources = cmp.config.sources(
        { { name = 'path' } },
        { { name = 'nvim_lsp' } },
        { { name = 'vsnip' } },
        { { name = 'buffer' } }
    ),
}

cmp.setup.cmdline('/', {
    mapping = cmp.mapping.preset.cmdline(),
    sources = {
        { name = 'buffer' }
    }
})

cmp.setup.cmdline(':', {
    mapping = cmp.mapping.preset.cmdline(),
    sources = cmp.config.sources({
        { name = 'path' }
    }, {
        {
            name = 'cmdline',
            option = {
                ignore_cmds = { 'Man', '!' }
            }
        }
    })
})
-- }}}

-- Treesitter {{{
local parsers = require("nvim-treesitter.parsers")

require("nvim-treesitter.parsers").get_parser_configs().just = {
  install_info = {
    url = "https://github.com/IndianBoy42/tree-sitter-just", -- local path or git repo
    files = { "src/parser.c", "src/scanner.cc" },
    branch = "main",
    use_makefile = true -- this may be necessary on MacOS (try if you see compiler errors)
  },
  maintainers = { "@IndianBoy42" },
}

require("nvim-treesitter.configs").setup {
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
        "tsx",
        "typescript",
        "wgsl",
        "yaml",
        "zig",
    },
    -- ignore_install = { "javascript" }, -- List of parsers to ignore installing

    highlight = {
        enable = true, -- false will disable the whole extension
        disable = {},  -- list of language that will be disabled
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
            "html",
            "htmldjango",
        },
    },
    context_commentstring = {
        enable = true
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
}

require('Comment').setup {
    pre_hook = require('ts_context_commentstring.integrations.comment_nvim').create_pre_hook(),
}
-- }}}

-- Markdown {{{
vim.g.vim_markdown_folding_disabled = 1
-- }}}

-- C/C++ {{{
vim.g.bazel_make_command = "Make"

function set_c_makeprg()
    if vim.fn.filereadable('meson.build') == 1 then
        vim.api.nvim_buf_set_option(0, 'makeprg', 'ninja -C build')
    end
    if vim.fn.filereadable('CMakeLists.txt') == 1 then
        vim.api.nvim_buf_set_option(0, 'makeprg', 'cmake --build build')
    end
    if vim.fn.filereadable('build/build.ninja') == 1 then
        vim.api.nvim_buf_set_option(0, 'makeprg', 'ninja -C build')
    end
    if vim.fn.filereadable('makefile') == 1 or vim.fn.filereadable('Makefile') == 1 then
        vim.api.nvim_buf_set_option(0, 'makeprg', 'make')
    end
    if vim.fn.filereadable('platformio.ini') == 1 then
        vim.api.nvim_buf_set_option(0, 'makeprg', 'pio run')
    end
    if vim.fn.filereadable('WORKSPACE') == 1 then
        vim.keymap.set("n", "<f7>", ":Bazel build<CR>", { silent = true, buffer = true })
    end
end

vim.g.compiler_gcc_ignore_unmatched_lines = 1
-- vim.g.cpp_no_cpp17 = 1

create_augroup("cbindings", "c", {
    "setlocal cpt-=t",
    "lua set_c_makeprg()",
})
create_augroup("cppbindings", "cpp", {
    "setlocal cpt-=t",
    "lua set_c_makeprg()",
})
-- }}}

-- Go {{{
function set_go_makeprg()
    if vim.fn.filereadable('makefile') == 1 or vim.fn.filereadable('Makefile') == 1 then
        vim.api.nvim_buf_set_option(0, 'makeprg', 'make')
    elseif vim.fn.filereadable('justfile') == 1 or vim.fn.filereadable('Justfile') == 1 then
        vim.api.nvim_buf_set_option(0, 'makeprg', 'just')
    else
        vim.api.nvim_buf_set_option(0, 'makeprg', 'go build .')
    end
end

create_augroup("gobindings", "go", {
    "setlocal shiftwidth=4 tabstop=4 noexpandtab",
    "lua set_go_makeprg()",
    "setlocal cpt-=t",
})
-- }}}

-- Zig {{{
vim.g.zig_fmt_autosave = 0
create_augroup("zigbindings", "zig", { "setlocal cpt-=t" })
-- }}}

-- Javascript {{{
create_augroup("javascriptbindings", "javascript", { "setlocal cpt-=t" })
create_augroup("typescriptbindings", "typescript", { "setlocal cpt-=t" })
create_augroup("typescriptreactbindings", "typescriptreact", { "setlocal cpt-=t" })
-- }}}

-- Ocaml {{{
create_augroup("ocamlbindings", "ocaml", {
    "setlocal cpt-=t",
    "setlocal makeprg=dune\\ build"
})
-- }}}

-- Rust {{{
vim.g.cargo_makeprg_params = "check"
create_augroup("rustbindings", "rust", { "setlocal cpt-=t" })
-- }}}

-- Dart {{{
vim.g.dart_style_guide = "2"

function flutter_hot_reload()
    vim.cmd [[silent execute '!kill -SIGUSR1 $(pgrep -f "[f]lutter_tool.*run")']]
end

create_augroup("dartbindings", "dart", {
    "setlocal cpt-=t",
    "nmap <silent> <buffer> <F7> :lua flutter_hot_reload()<CR>",
})
-- }}}

-- Latex {{{
function open_pdf_preview()
    local buffer_path = vim.fn.expand('%')
    local pdf_path = string.gsub(buffer_path, "%.[^/]+$", ".pdf")
    print(pdf_path)
    vim.cmd("silent !zathura " .. pdf_path .. " & disown")
end

create_augroup("latexbindings", "tex", {
    "setlocal makeprg=latexmk",
    "nmap <silent> <buffer> <Leader>mp :lua open_pdf_preview()<CR>",
})
-- }}}

-- Clojure {{{
-- vim.g["conjure#filetypes"] = { "clojure" }
-- create_augroup("clojurebindings", "clojure", {
--     "nmap <silent> <buffer> <M-e> :ConjureEvalCurrentForm<CR>",
--     "nnoremap <silent> <buffer> <f7> :ConjureEvalBuf<CR>",
--     "inoremap <silent> <buffer> <f7> :ConjureEvalBuf<CR>",
-- })
-- }}}

-- Elixir {{{
require("felipe_elixir").setup()
create_augroup("elixirbindings", "elixir", {
    "compiler exunit"
})
-- }}}

-- Swift {{{
create_augroup("swiftbindings", "swift", {
    "setlocal makeprg=xcodebuild"
})
-- }}}

-- Coq {{{
vim.api.nvim_create_autocmd({ "FileType" }, {
    pattern = "coq",
    group = vim.api.nvim_create_augroup('CoqBindings', {}),
    callback = function(ev)
        vim.keymap.set("n", "<Leader>j", ":CoqNext<CR>", { silent = false })
        vim.keymap.set("n", "<Leader>k", ":CoqUndo<CR>", { silent = false })
        vim.keymap.set("n", "<Leader>c", ":CoqToLine<CR>", { silent = false })
    end,
})
-- }}}

-- Other filetypes {{{
vim.cmd [[
autocmd BufRead,BufNewFile *.wgsl set filetype=wgsl
autocmd BufRead,BufNewFile *.slint set filetype=slint
autocmd BufRead,BufNewFile Tiltfile set filetype=starlark 
autocmd BufRead,BufNewFile Dockerfile.* set filetype=dockerfile
]]
-- }}}
