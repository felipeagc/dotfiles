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

function create_augroup(groupname, filetype, commands)
    local group = vim.api.nvim_create_augroup(groupname, {})
    for _, command in ipairs(commands) do
        vim.api.nvim_create_autocmd({"FileType"}, {
            pattern = filetype,
            command = command,
            group = group,
        })
    end
end
-- }}}

require("lazy").setup({
    'neovim/nvim-lspconfig',
    'hrsh7th/nvim-cmp',
    'hrsh7th/cmp-nvim-lsp',
    'hrsh7th/cmp-buffer',
    'hrsh7th/cmp-path',
    'hrsh7th/cmp-cmdline',
    'hrsh7th/cmp-vsnip',
    'hrsh7th/vim-vsnip',

    'nvim-treesitter/nvim-treesitter',
    'nvim-treesitter/playground',
    'p00f/nvim-ts-rainbow',
    'JoosepAlviste/nvim-ts-context-commentstring',
    'numToStr/Comment.nvim',
    'princejoogie/tailwind-highlight.nvim',

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
        'zbirenbaum/copilot.lua',
        config = function() 
            require("copilot").setup({
                suggestion = { 
                    auto_trigger = true,
                    keymap = {
                        accept = "<M-l>",
                        next = "<M-]>",
                        prev = "<M-[>",
                        dismiss = "<M-h>",
                    },
                    filetypes = {
                        go = true,
                        html = true,
                        javascript = true,
                        lua = true,
                        ocaml = true,
                        rust = true,
                        typescript = true,
                        typescriptreact = true,
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

    'editorconfig/editorconfig-vim',
    'derekwyatt/vim-fswitch',

    'plasticboy/vim-markdown',
    -- 'beyondmarc/hlsl.vim',
    'ziglang/zig.vim',
    'rust-lang/rust.vim',
    'dart-lang/dart-vim-plugin',
    -- 'NoahTheDuke/vim-just',
    -- 'lakshayg/vim-bazel',
    'LnL7/vim-nix',
    -- 'Olical/conjure',
    'clojure-vim/vim-jack-in',
    'elixir-editors/vim-elixir',
    'isobit/vim-caddyfile',
    'felipeagc/dusk.vim',
    
    'nvim-lua/plenary.nvim',
    'nvim-telescope/telescope.nvim',
    'nvim-telescope/telescope-dap.nvim',

    -- "lukas-reineke/indent-blankline.nvim",

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
    {
        "folke/which-key.nvim",
        config = function()
            vim.o.timeout = true
            vim.o.timeoutlen = 300
            local wk = require("which-key")
            wk.setup {}
            wk.register({
                ["<leader>f"] = { name = "+files" },
                ["<leader>fe"] = { name = "+edit configs" },
                ["<leader>fv"] = { name = "+netrw" },
                ["<leader>g"] = { name = "+git" },
                ["<leader>w"] = { name = "+window" },
                ["<leader>b"] = { name = "+buffer" },
                ["<leader>d"] = { name = "+dap" },
            })
        end,
    },
    { "folke/zen-mode.nvim", config = function() require("zen-mode").setup {} end },

    -- 'rktjmp/lush.nvim',
    -- 'lifepillar/vim-gruvbox8',
    -- { 
    --     'rose-pine/neovim',
    --     as = 'rose-pine',
    --     config = function()
    --         require('rose-pine').setup {
    --             disable_italics = true,
    --         } 
    --         vim.cmd('colorscheme rose-pine')
    --     end
    -- },
    -- {
    --     'ramojus/mellifluous.nvim',
    --     dependencies = { 'rktjmp/lush.nvim' },
    --     config = function()
    --         require'mellifluous'.setup({ --[[...]] }) -- optional, see configuration section.
    --         vim.cmd('colorscheme mellifluous')
    --     end,
    -- },
    -- {
    --     'folke/tokyonight.nvim',
    --     config = function()
    --         vim.cmd("colorscheme tokyonight-moon")
    --     end,
    -- },
    { 'nyoom-engineering/oxocarbon.nvim' },
    { 
        'rebelot/kanagawa.nvim',
        config = function()
            require('kanagawa').setup({
                compile = false,
                undercurl = true,           -- enable undercurls
                commentStyle = { italic = false },
                functionStyle = {},
                keywordStyle = { italic = false },
                statementStyle = { bold = true },
                typeStyle = {},
                transparent = false,        -- do not set background color
                dimInactive = false,        -- dim inactive window `:h hl-NormalNC`
                terminalColors = true,      -- define vim.g.terminal_color_{0,17}
                colors = {},
                overrides = function(colors) return {} end,
            })
        end
    },
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
vim.o.wildignore = vim.o.wildignore .. '*.so,*.swp,*.zip,*.o,*.png,*.jpg,*.jpeg,*/target/*,*/build/*,*/node_modules/*,tags,*.glb,*.gltf,*.hdr'
vim.o.hidden = true
vim.o.showmode = false
vim.o.modeline = true
vim.o.modelines = 1

vim.o.undofile = true
vim.o.undodir = vim.fn.stdpath('data')..'/undodir'

vim.o.linebreak = true

vim.o.showbreak = '>   '
vim.o.shortmess = vim.o.shortmess .. 'c'

vim.o.ignorecase = true
vim.o.smartcase = true

vim.o.cinoptions = vim.o.cinoptions .. 'L0'
vim.o.cinoptions = vim.o.cinoptions .. 'l1'

vim.wo.number = false
vim.wo.cursorline = true
-- vim.wo.foldmethod = 'marker'
-- vim.wo.foldlevel = 0
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

vim.keymap.set("n", "<Leader>gs", ":vertical Git<CR>", { silent = true })

vim.keymap.set("n", "<C-a>", ":FSHere<CR>", { silent = true })

vim.keymap.set("n", "<A-p>", "<nop>", { silent = true })

vim.keymap.set("n", "<f7>", ":Make<CR>", { silent = true })

vim.keymap.set("n", "<Leader>do", ":lua require('dapui').toggle()<CR>", { silent = true })
vim.keymap.set("n", "<Leader>dc", ":lua require('dap').continue()<CR>", { silent = true })
vim.keymap.set("n", "<Leader>db", ":lua require('dap').toggle_breakpoint()<CR>", { silent = true })
vim.keymap.set("n", "<Leader>ds", ":lua require('dap').step_into()<CR>", { silent = true })
vim.keymap.set("n", "<Leader>dn", ":lua require('dap').step_over()<CR>", { silent = true })
vim.keymap.set("n", "<Leader>df", ":lua require('dap').step_out()<CR>", { silent = true })
vim.keymap.set("n", "<Leader>dq", ":lua require('dap').terminate()<CR>", { silent = true })

vim.keymap.set("n", "<Leader>zz", ":ZenMode<CR>", { silent = true })

-- Disable ex mode binding
vim.cmd[[map Q <Nop>]]
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

local function on_lsp_attach(client, bufnr)
    -- buf_set_option('omnifunc', 'v:lua.vim.lsp.omnifunc')

    local opts = { remap=false, silent=true, buffer=bufnr }

    vim.keymap.set('n', 'K', function() vim.lsp.buf.hover() end, opts)
    vim.keymap.set('n', '<c-]>', function() vim.lsp.buf.definition() end, opts)
    vim.keymap.set('n', '<leader>mf', function() vim.lsp.buf.format({async = true}) end, opts)
    vim.keymap.set('n', '<leader>mr', function() vim.lsp.buf.rename() end, opts)
    vim.keymap.set('n', '<leader>mR', ":LspRestart<CR>", opts)
    vim.keymap.set('n', '<leader>mi', function() vim.lsp.buf.code_action() end, opts)
    vim.keymap.set('n', '[d', function() vim.diagnostic.goto_prev() end, opts)
    vim.keymap.set('n', ']d', function() vim.diagnostic.goto_next() end, opts)
    vim.keymap.set('n', '<C-y>', function() vim.diagnostic.open_float() end, opts)
    vim.keymap.set('i', '<C-h>', function() vim.lsp.buf.signature_help() end, opts)

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

    if client.name == 'tailwindcss' then
        require('tailwind-highlight').setup(client, bufnr, {
            single_column = false,
            mode = 'background',
            debounce = 200,
        })
    end
end

local capabilities = require('cmp_nvim_lsp').default_capabilities()
capabilities.textDocument.completion.completionItem.snippetSupport = false

local servers = {
    "ansiblels",
    "clangd",
    "clojure_lsp",
    "dartls",
    "eslint",
    "gopls",
    "hls",
    "kotlin_language_server",
    "metals",
    "ocamllsp",
    "svelte",
    "tailwindcss",
    "zls",
}
for _, lsp in ipairs(servers) do
    lspconfig[lsp].setup{
        capabilities = capabilities,
        on_attach = on_lsp_attach,
    }
end

lspconfig.elixirls.setup {
    capabilities = capabilities,
    on_attach = on_lsp_attach,
    init_options = { lint = true },
    cmd = { "/usr/bin/elixir-ls" },
}

lspconfig.denols.setup {
    capabilities = capabilities,
    on_attach = on_lsp_attach,
    root_dir = lspconfig.util.root_pattern("deno.json", "deno.jsonc"),
    init_options = { lint = true },
}

lspconfig.tsserver.setup {
    capabilities = capabilities,
    on_attach = on_lsp_attach,
    init_options = { lint = true },
    root_dir = lspconfig.util.root_pattern("package.json"),
}

lspconfig.rust_analyzer.setup {
    capabilities = capabilities,
    on_attach = on_lsp_attach,
    settings = {
        ["rust-analyzer"] = {
            ["cargo"] = {
                ["buildScripts"] = {
                    ["enable"] = true,
                },
            },
        },
    },
}
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
require("dapui").setup({
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
                { id = "stacks", size = 0.25 },
                { id = "watches", size = 00.25 },
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
        max_height = nil, -- These can be integers or a float between 0 and 1.
        max_width = nil, -- Floats will be treated as percentage of your screen.
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
vim.cmd.colorscheme "kanagawa"
-- vim.g.gruvbox_italics = 0
-- vim.g.gruvbox_transp_bg = 0
-- vim.cmd[[colorscheme gruvbox8_hard]]
-- vim.cmd[[colorscheme felipe]]
-- vim.cmd[[colorscheme kanagawa]]
-- vim.g.zenbones = { darkness = 'warm' }
-- vim.cmd[[colorscheme zenbones]]
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
    autocmd FileType javascript setlocal shiftwidth=4 tabstop=4 expandtab
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
local rainbow_enabled_list = {"clojure", "fennel", "commonlisp", "query"}
local parsers = require("nvim-treesitter.parsers")
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
        "heex",
        "glsl",
        "go",
        "graphql",
        "haskell",
        "hlsl",
        "html",
        "htmldjango",
        "java",
        "javascript",
        "kotlin",
        "latex",
        "lua",
        "make",
        "markdown",
        "ocaml",
        "python",
        "rust",
        "svelte",
        "tsx",
        "typescript",
        "wgsl",
        "yaml",
        "zig",
    },
    -- ignore_install = { "javascript" }, -- List of parsers to ignore installing

    highlight = {
        enable = true,              -- false will disable the whole extension
        disable = { },  -- list of language that will be disabled
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
            "elixir",
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
    rainbow = {
        enable = true,
        disable = vim.tbl_filter(
            function(p)
                local disable = true
                for _, lang in pairs(rainbow_enabled_list) do
                    if p==lang then disable = false end
                end
                return disable
            end,
            parsers.available_parsers()
        ),
        extended_mode = true, -- Also highlight non-bracket delimiters like html tags, boolean or table: lang -> boolean
        max_file_lines = 2000, -- Do not enable for files with more than n lines, int
    }
}

require('Comment').setup{
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
vim.g["conjure#filetypes"] = {"clojure"}
create_augroup("clojurebindings", "clojure", {
    "nmap <silent> <buffer> <M-e> :ConjureEvalCurrentForm<CR>",
    "nnoremap <silent> <buffer> <f7> :ConjureEvalBuf<CR>",
    "inoremap <silent> <buffer> <f7> :ConjureEvalBuf<CR>",
})
-- }}}

-- WGSL {{{
vim.cmd[[
augroup wgsl_ft
  au!
  autocmd BufNewFile,BufRead *.wgsl   set filetype=wgsl
augroup END
]]
-- }}}

-- Elixir {{{
require("felipe_elixir").setup()
create_augroup("elixirbindings", "elixir", {
    "compiler exunit"
})
-- }}}
