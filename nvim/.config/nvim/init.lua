-- Package manager setup {{{
local install_path = vim.fn.stdpath('data')..'/site/pack/packer/opt/packer.nvim'

if vim.fn.empty(vim.fn.glob(install_path)) > 0 then
    vim.cmd('!git clone https://github.com/wbthomason/packer.nvim '..install_path)
    vim.cmd('packadd packer.nvim')
end

vim.cmd('packadd packer.nvim')

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

require('packer').startup(function()
    -- Packer can manage itself as an optional plugin
    use {'wbthomason/packer.nvim', opt = true}

    use 'neovim/nvim-lspconfig'
    use 'ray-x/lsp_signature.nvim'
    use {
        'nvim-treesitter/nvim-treesitter',
        run = ':TSUpdate'
    }
    use 'mfussenegger/nvim-dap'
    use 'rcarriga/nvim-dap-ui'
    use 'leoluz/nvim-dap-go'

    use 'tpope/vim-surround'
    use 'tpope/vim-commentary'
    use 'tpope/vim-endwise'
    use 'tpope/vim-repeat'
    use 'tpope/vim-fugitive'
    use 'tpope/vim-abolish'
    use 'tpope/vim-unimpaired'
    use 'tpope/vim-dispatch'

    use 'editorconfig/editorconfig-vim'
    use 'derekwyatt/vim-fswitch'

    use 'plasticboy/vim-markdown'
    use 'beyondmarc/hlsl.vim'
    use 'ziglang/zig.vim'
    use 'rust-lang/rust.vim'
    use 'dart-lang/dart-vim-plugin'
    use 'NoahTheDuke/vim-just'
    use 'lakshayg/vim-bazel'
    use 'LnL7/vim-nix'
    use '~/tmp/dusk.vim'
    use '~/tmp/lang.vim'

    use 'nvim-lua/plenary.nvim'
    use 'nvim-telescope/telescope.nvim'
    use 'nvim-telescope/telescope-dap.nvim'

    use 'lukas-reineke/indent-blankline.nvim'

    use 'rktjmp/lush.nvim'
end)

-- Vim options {{{
vim.o.termguicolors = true

vim.o.exrc = true
vim.o.showcmd = true
vim.o.mouse = 'a'

vim.o.scrolloff = 5
vim.o.clipboard = 'unnamedplus'
vim.o.completeopt = 'menuone,noselect'

vim.o.tabstop = 4
vim.o.softtabstop = 4
vim.o.shiftwidth = 4
vim.o.autoindent = true
vim.o.smartindent = true
vim.o.smarttab = true

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
vim.wo.foldmethod = 'marker'
vim.wo.foldlevel = 0
-- }}}

-- Keybinds {{{
vim.g.mapleader = " "

vim.keymap.set("n", "<C-p>", ":Telescope find_files<CR>", { silent = true })
vim.keymap.set("n", "<C-b>", ":Telescope buffers<CR>", { silent = true })
vim.keymap.set("n", "<Leader>fg", ":Telescope live_grep<CR>", { silent = true })

vim.keymap.set("n", "<C-j>", "<C-w>w", { noremap = true })
vim.keymap.set("n", "<C-k>", "<C-w>W", { noremap = true })

vim.keymap.set("n", "<C-e>", ":CNext<CR>", { silent = true })
vim.keymap.set("n", "<C-q>", ":CPrev<CR>", { silent = true })

-- Continuous indentation shift
vim.keymap.set("v", "<", "<gv", { noremap = true })
vim.keymap.set("v", ">", ">gv", { noremap = true })
vim.keymap.set("v", "<s-lt>", "<gv", { noremap = true })

vim.keymap.set("n", "<Leader>fed", ":e " .. vim.fn.stdpath("config") .. "/init.lua<CR>", { silent = true })
vim.keymap.set("n", "<Leader>feg", ":e " .. vim.fn.stdpath("config") .. "/ginit.vim<CR>", { silent = true })

vim.keymap.set("n", "<Leader>w/", ":vsp<CR>", { silent = true })
vim.keymap.set("n", "<Leader>w-", ":sp<CR>", { silent = true })
vim.keymap.set("n", "<Leader>wd", ":q<CR>", { silent = true })
vim.keymap.set("n", "<Leader>wb", "<C-w>=", { silent = true })

vim.keymap.set("n", "<Leader>bd", ":bd<CR>", { silent = true })
vim.keymap.set("n", "<Leader>bcc", ":%bd|e#<CR>", { silent = true })

vim.keymap.set("n", "<Leader>gs", ":vertical Git<CR>", { silent = true })
-- vim.keymap.set("n", "<Leader>gs", ":LazyGit<CR>", { silent = true })

vim.keymap.set("n", "<C-a>", ":FSHere<CR>", { silent = true })

vim.keymap.set("n", "<f7>", ":Make<CR>", { silent = true })

vim.keymap.set("n", "<Leader>do", ":lua require('dapui').toggle()<CR>", { silent = true })
vim.keymap.set("n", "<Leader>dc", ":lua require('dap').continue()<CR>", { silent = true })
vim.keymap.set("n", "<Leader>db", ":lua require('dap').toggle_breakpoint()<CR>", { silent = true })
vim.keymap.set("n", "<Leader>ds", ":lua require('dap').step_into()<CR>", { silent = true })
vim.keymap.set("n", "<Leader>dn", ":lua require('dap').step_over()<CR>", { silent = true })
vim.keymap.set("n", "<Leader>df", ":lua require('dap').step_out()<CR>", { silent = true })
vim.keymap.set("n", "<Leader>dq", ":lua require('dap').terminate()<CR>", { silent = true })

-- Disable ex mode binding
vim.cmd[[map Q <Nop>]]
-- }}}

-- Package configuration {{{
require("lsp_signature").setup({
    bind = true, 
    hint_enable = false,
    floating_window = true,
    hint_prefix = "",
    handler_opts = {
        border = "none"   -- double, single, shadow, none
    },
})

vim.diagnostic.config({
    virtual_text = true,
    signs = false,
    underline = true,
    update_in_insert = false,
    severity_sort = true,
})

local lspconfig = require("lspconfig")

local function on_lsp_attach(client, bufnr)
    local function buf_set_keymap(...) vim.api.nvim_buf_set_keymap(bufnr, ...) end
    local function buf_set_option(...) vim.api.nvim_buf_set_option(bufnr, ...) end

    buf_set_option('omnifunc', 'v:lua.vim.lsp.omnifunc')

    local opts = { noremap=true, silent=true }

    buf_set_keymap('n', 'K', '<cmd>lua vim.lsp.buf.hover()<CR>', opts)
    buf_set_keymap('n', '<c-]>', '<cmd>lua vim.lsp.buf.definition()<CR>', opts)
    buf_set_keymap('n', '<Leader>mf', '<cmd>lua vim.lsp.buf.formatting()<CR>', opts)
    buf_set_keymap('n', '<Leader>mr', '<cmd>lua vim.lsp.buf.rename()<CR>', opts)
    buf_set_keymap('n', '<Leader>mi', '<cmd>lua vim.lsp.buf.code_action()<CR>', opts)
    buf_set_keymap('n', '<Leader>me', ':Telescope diagnostics<CR>', opts)
end

local servers = { "clangd", "gopls", "zls", "tsserver", "ocamllsp", "dartls", "hls", "kotlin_language_server" }
for _, lsp in ipairs(servers) do
    lspconfig[lsp].setup{
        on_attach = on_lsp_attach,
    }
end

lspconfig['rust_analyzer'].setup{
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

-- require("gitsigns").setup{}

local actions = require("telescope.actions")
require('telescope').setup({
    defaults = {
        preview = false,
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

require("indent_blankline").setup {
    -- for example, context is off by default, use this to turn it on
    show_current_context = true,
    show_current_context_start = true,
}
-- }}}

-- Color scheme {{{
vim.cmd [[set background=dark]]
vim.cmd[[colorscheme felipe]]
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
    autocmd FileType typescript setlocal shiftwidth=4 tabstop=4 expandtab
    autocmd FileType typescriptreact setlocal shiftwidth=4 tabstop=4 expandtab
    autocmd FileType rust setlocal shiftwidth=4 tabstop=4 expandtab
]])
-- }}}

-- Completion {{{
vim.cmd [[inoremap <silent> <C-n> <C-x><C-o>]]
-- }}}

-- Treesitter {{{
    require("nvim-treesitter.configs").setup {
        ensure_installed = {
            "c",
            "cpp",
            "lua",
            "python",
            "javascript",
            "typescript",
            "go",
            "rust",
            "zig",
            "ocaml",
            "haskell",
            "dart",
            "latex",
            "dockerfile",
            "css",
            "html",
            "java",
            "cmake",
            "bash",
            "wgsl",
            "glsl",
        }, -- one of "all", "maintained" (parsers with maintainers), or a list of languages
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
        context_commentstring = {
            enable = true
        }
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
create_augroup("ocamlbindings", "ocaml", { "setlocal cpt-=t" })
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
