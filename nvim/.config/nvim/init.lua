-- Package manager setup {{{
local install_path = vim.fn.stdpath('data')..'/site/pack/packer/opt/packer.nvim'

if vim.fn.empty(vim.fn.glob(install_path)) > 0 then
    vim.cmd('!git clone https://github.com/wbthomason/packer.nvim '..install_path)
    vim.cmd('packadd packer.nvim')
end

vim.cmd('packadd packer.nvim')

function nvim_create_augroups(definitions)
    for group_name, definition in pairs(definitions) do
        vim.api.nvim_command('augroup '..group_name)
        vim.api.nvim_command('autocmd!')
        for _, def in ipairs(definition) do
            -- if type(def) == 'table' and type(def[#def]) == 'function' then
            -- 	def[#def] = lua_callback(def[#def])
            -- end
            local command = table.concat(vim.tbl_flatten{'autocmd', def}, ' ')
            vim.api.nvim_command(command)
        end
        vim.api.nvim_command('augroup END')
    end
end
-- }}}

require('packer').startup(function()
    -- Packer can manage itself as an optional plugin
    use {'wbthomason/packer.nvim', opt = true}

    use 'junegunn/fzf.vim'
    -- use { 'lewis6991/gitsigns.nvim', requires = { 'nvim-lua/plenary.nvim' } }

    use 'neovim/nvim-lspconfig'
    use 'ray-x/lsp_signature.nvim'
    use {
        'nvim-treesitter/nvim-treesitter',
        run = ':TSUpdate'
    }

    use 'tpope/vim-surround'
    use 'tpope/vim-commentary'
    use 'tpope/vim-endwise'
    use 'tpope/vim-repeat'
    use 'tpope/vim-fugitive'
    use 'tpope/vim-abolish'
    use 'tpope/vim-unimpaired'
    use 'tpope/vim-dispatch'

    use 'kdheepak/lazygit.nvim'

    use 'editorconfig/editorconfig-vim'
    use 'derekwyatt/vim-fswitch'
    -- use 'ludovicchabant/vim-gutentags'

    -- use 'bfrg/vim-cpp-modern'
    use 'maxmellon/vim-jsx-pretty'
    use 'yuezk/vim-js'
    use 'plasticboy/vim-markdown'
    use 'tikhomirov/vim-glsl'
    use 'beyondmarc/hlsl.vim'
    use 'ziglang/zig.vim'
    use 'DingDean/wgsl.vim'
    use 'peterhoeg/vim-qml'
    use 'rust-lang/rust.vim'
    use 'dart-lang/dart-vim-plugin'
    use 'ledger/vim-ledger'
    use 'tomlion/vim-solidity'
    use 'NoahTheDuke/vim-just'
    use 'nathangrigg/vim-beancount'
    use '~/tmp/dusk.vim'
    use '~/tmp/lang.vim'

    -- use { 'embark-theme/vim', as = 'embark' }
    use 'folke/lsp-colors.nvim'
    use 'folke/tokyonight.nvim'
    use 'ishan9299/nvim-solarized-lua'
    use 'lifepillar/vim-solarized8'
    use 'jnurmine/Zenburn'
    use 'rktjmp/lush.nvim'
    use 'metalelf0/jellybeans-nvim'
    use 'mcchrish/zenbones.nvim'

    -- use 'sainnhe/sonokai'
    -- use 'ayu-theme/ayu-vim'
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
    severity_sort = false,
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
end

local servers = { "clangd", "gopls", "zls", "tsserver", "ocamllsp", "rust_analyzer", "dartls", "hls" }
for _, lsp in ipairs(servers) do
    lspconfig[lsp].setup{
        on_attach = on_lsp_attach,
    }
end

-- require("gitsigns").setup{}

vim.cmd[[
let $FZF_DEFAULT_OPTS='--color=gutter:-1 --layout=reverse'

if executable('ag')
	let $FZF_DEFAULT_COMMAND = 'ag -g ""'
endif

let g:fzf_preview_window = ''
]]
-- }}}

vim.cmd [[set background=dark]]

-- Other color schemes {{{
-- vim.cmd('colorscheme embark')
-- vim.cmd('autocmd ColorScheme * hi! StatusLine guibg=#3E3859')
-- vim.cmd('autocmd ColorScheme * hi! StatusLineNC guibg=#100E23 guifg=#6B697E')

-- vim.g.sonokai_style = 'shusia'
-- vim.cmd('colorscheme sonokai')

-- vim.g.ayucolor = "mirage"
-- vim.cmd('colorscheme ayu')

-- vim.cmd[[colorscheme solarized-high]]
-- vim.cmd[[colorscheme solarized8_high]]

-- vim.g.zenburn_high_Contrast = 1
-- vim.cmd[[colorscheme zenburn]]

-- vim.cmd[[colorscheme jellybeans-nvim]]
vim.cmd[[colorscheme kanagawabones]]
-- vim.cmd[[colorscheme zenbones]]
-- vim.cmd[[
-- 	autocmd ColorScheme * hi! GitSignsAdd guibg=#333333 guifg=#d2ebbe ctermbg=none
-- 	autocmd ColorScheme * hi! GitSignsChange guibg=#333333 guifg=#dad085 ctermbg=none
-- 	autocmd ColorScheme * hi! GitSignsDelete guibg=#333333 guifg=#f0a0c0 ctermbg=none
-- ]]
-- }}}

-- Keybinds {{{
vim.g.mapleader = ' '

vim.api.nvim_set_keymap('n', '<C-p>', ':Files<CR>', { silent = true })
vim.api.nvim_set_keymap('n', '<C-b>', ':Buffers<CR>', { silent = true })
vim.api.nvim_set_keymap('n', '<Leader>fg', ':Ag<CR>', { silent = true })

vim.api.nvim_set_keymap('n', '<C-j>', '<C-w>w', { noremap = true })
vim.api.nvim_set_keymap('n', '<C-k>', '<C-w>W', { noremap = true })

vim.api.nvim_set_keymap('n', '<C-e>', ':CNext<CR>', { silent = true })
vim.api.nvim_set_keymap('n', '<C-q>', ':CPrev<CR>', { silent = true })

-- Continuous indentation shift
vim.api.nvim_set_keymap('v', '<', '<gv', { noremap = true })
vim.api.nvim_set_keymap('v', '>', '>gv', { noremap = true })
vim.api.nvim_set_keymap('v', '<s-lt>', '<gv', { noremap = true })

vim.api.nvim_set_keymap('n', '<Leader>fed', ':e ' .. vim.fn.stdpath('config') .. '/init.lua<CR>', { silent = true })

vim.api.nvim_set_keymap('n', '<Leader>w/', ':vsp<CR>', { silent = true })
vim.api.nvim_set_keymap('n', '<Leader>w-', ':sp<CR>', { silent = true })
vim.api.nvim_set_keymap('n', '<Leader>wd', ':q<CR>', { silent = true })
vim.api.nvim_set_keymap('n', '<Leader>wb', '<C-w>=', { silent = true })

vim.api.nvim_set_keymap('n', '<Leader>bd', ':bd<CR>', { silent = true })
vim.api.nvim_set_keymap('n', '<Leader>bcc', ':%bd|e#<CR>', { silent = true })

-- vim.api.nvim_set_keymap('n', '<Leader>gs', ':vertical Git<CR>', { silent = true })
vim.api.nvim_set_keymap('n', '<Leader>gs', ':LazyGit<CR>', { silent = true })

vim.api.nvim_set_keymap('n', '<C-a>', ':FSHere<CR>', { silent = true })

vim.api.nvim_set_keymap('n', '<f7>', ':Make<CR>', { silent = true })

-- Disable ex mode binding
vim.cmd[[map Q <Nop>]]
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
  ensure_installed = "maintained", -- one of "all", "maintained" (parsers with maintainers), or a list of languages
  -- ignore_install = { "javascript" }, -- List of parsers to ignore installing
  highlight = {
    enable = true,              -- false will disable the whole extension
    disable = { "c", "cpp", "python" },  -- list of language that will be disabled
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
end

vim.g.compiler_gcc_ignore_unmatched_lines = 1
-- vim.g.cpp_no_cpp17 = 1

nvim_create_augroups({
    cbindingslua = {
        {"Filetype", "c", "setlocal cpt-=t"},
        {"Filetype", "c", "nmap <silent> <buffer> <F7>       :Make<CR>"},
        {"Filetype", "c", "lua set_c_makeprg()"},
    },
    cppbindingslua = {
        {"Filetype", "cpp", "setlocal cpt-=t"},
        {"Filetype", "cpp", "nmap <silent> <buffer> <F7>       :Make<CR>"},
        {"Filetype", "cpp", "lua set_c_makeprg()"},
    },
})
-- }}}

-- Go {{{
function goimports(timeoutms)
    local context = { source = { organizeImports = true } }
    vim.validate { context = { context, "t", true } }

    local params = vim.lsp.util.make_range_params()
    params.context = context

    -- See the implementation of the textDocument/codeAction callback
    -- (lua/vim/lsp/handler.lua) for how to do this properly.
    local result = vim.lsp.buf_request_sync(0, "textDocument/codeAction", params, timeout_ms)
    if not result or next(result) == nil then return end
    if not result[1] then return end
    local actions = result[1].result
    if not actions then return end
    local action = actions[1]

    -- textDocument/codeAction can return either Command[] or CodeAction[]. If it
    -- is a CodeAction, it can have either an edit, a command or both. Edits
    -- should be executed first.
    if action.edit or type(action.command) == "table" then
        if action.edit then
            vim.lsp.util.apply_workspace_edit(action.edit)
        end
        if type(action.command) == "table" then
            vim.lsp.buf.execute_command(action.command)
        end
    else
        vim.lsp.buf.execute_command(action)
    end
end

nvim_create_augroups({
    gobindingslua = {
        {"Filetype", "go", "setlocal shiftwidth=4 tabstop=4 noexpandtab"},
        {"Filetype", "go", "setlocal cpt-=t"},
        {"Filetype", "go", "nmap <silent> <buffer> <F7>       :Make<CR>"},
        -- {"BufWritePre", "*.go", "lua vim.lsp.buf.formatting_sync(nil, 1000)"},
        -- {"BufWritePre", "*.go", "lua goimports(1000)"},
    },
})
-- }}}

-- Zig {{{
vim.g.zig_fmt_autosave = 0

nvim_create_augroups({
    zigbindingslua = {
        {"Filetype", "zig", "setlocal cpt-=t"},
        {"Filetype", "zig", "nmap <silent> <buffer> <F7>       :Make<CR>"},
    },
})
-- }}}

-- Javascript {{{
nvim_create_augroups({
    javascriptbindingslua = {
        {"Filetype", "javascript", "setlocal cpt-=t"},
        {"Filetype", "javascript", "nmap <silent> <buffer> <F7>       :Make<CR>"},
    },
    typescriptbindingslua = {
        {"Filetype", "typescript", "setlocal cpt-=t"},
        {"Filetype", "typescript", "nmap <silent> <buffer> <F7>       :Make<CR>"},
    },
    typescriptreactbindingslua = {
        {"Filetype", "typescriptreact", "setlocal cpt-=t"},
        {"Filetype", "typescriptreact", "nmap <silent> <buffer> <F7>       :Make<CR>"},
    },
})
-- }}}

-- Ocaml {{{
nvim_create_augroups({
    ocamlbindingslua = {
        {"Filetype", "ocaml", "setlocal cpt-=t"},
    },
})
-- }}}

-- Rust {{{
vim.g.cargo_makeprg_params = "check"

nvim_create_augroups({
    rustbindingslua = {
        {"Filetype", "rust", "setlocal cpt-=t"},
        {"Filetype", "rust", "nmap <silent> <buffer> <F7>       :Make<CR>"},
    },
})
-- }}}

-- Dart {{{
vim.g.dart_style_guide = "2"

function flutter_hot_reload()
    vim.cmd [[silent execute '!kill -SIGUSR1 $(pgrep -f "[f]lutter_tool.*run")']]
end

nvim_create_augroups({
    dartbindingslua = {
        {"Filetype", "dart", "setlocal cpt-=t"},
        {"Filetype", "dart", "nmap <silent> <buffer> <F7> :lua flutter_hot_reload()<CR>"},
    },
})
-- }}}
