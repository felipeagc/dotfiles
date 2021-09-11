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
    use { 'lewis6991/gitsigns.nvim', requires = { 'nvim-lua/plenary.nvim' } }

    use 'neovim/nvim-lspconfig'
    use {
        "hrsh7th/nvim-cmp",
        requires = {
            'hrsh7th/cmp-buffer',
            'hrsh7th/cmp-nvim-lsp',
            'hrsh7th/cmp-path'
        }
    }
    use 'ray-x/lsp_signature.nvim'

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
    -- use 'ludovicchabant/vim-gutentags'

    -- use 'bfrg/vim-cpp-modern'
    use 'maxmellon/vim-jsx-pretty'
    use 'yuezk/vim-js'
    use 'plasticboy/vim-markdown'
    use 'tikhomirov/vim-glsl'
    use 'beyondmarc/hlsl.vim'
    use 'ziglang/zig.vim'
    use 'zah/nim.vim'
    use 'DingDean/wgsl.vim'
    use 'peterhoeg/vim-qml'
    use 'rust-lang/rust.vim'
    use 'dart-lang/dart-vim-plugin'
    use 'ledger/vim-ledger'
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
    -- use 'sainnhe/sonokai'
    -- use 'ayu-theme/ayu-vim'
end)

-- Vim options {{{
vim.o.termguicolors = true

vim.o.exrc = true
vim.o.showcmd = true
vim.o.mouse = 'a'

vim.o.scrolloff = 10
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
local cmp = require("cmp")
cmp.setup({
    snippet = {},
    mapping = {
        ['<C-p>'] = cmp.mapping.select_prev_item(),
        ['<C-n>'] = cmp.mapping.complete(),
    },
    sources = {
        { name = 'nvim_lsp' },
        { name = 'buffer' },
        { name = 'path' },
    },
    completion = {
        autocomplete = false,
    },
})

require("lsp_signature").setup({
    bind = true, 
    hint_enable = false,
    floating_window = true,
    hint_prefix = "",
    handler_opts = {
        border = "none"   -- double, single, shadow, none
    },
})

local capabilities = vim.lsp.protocol.make_client_capabilities()
capabilities = require("cmp_nvim_lsp").update_capabilities(capabilities)

local lspconfig = require("lspconfig")
lspconfig.clangd.setup{ capabilities = capabilities, }
lspconfig.gopls.setup{ capabilities = capabilities, }
-- lspconfig.zls.setup{ capabilities = capabilities, }
lspconfig.tsserver.setup{ capabilities = capabilities, }
lspconfig.ocamllsp.setup{ capabilities = capabilities, }
lspconfig.rust_analyzer.setup{ capabilities = capabilities, }
lspconfig.nimls.setup{ capabilities = capabilities, }
lspconfig.dartls.setup{ capabilities = capabilities, }

require("gitsigns").setup({})

vim.cmd[[
let $FZF_DEFAULT_OPTS='--color=gutter:-1 --layout=reverse'

if executable('ag')
	let $FZF_DEFAULT_COMMAND = 'ag -g ""'
endif

let g:fzf_preview_window = ''
]]
-- }}}

vim.cmd[[set background=dark]]

-- Tokyonight color scheme {{{
vim.g.tokyonight_style = "night"
-- vim.g.tokyonight_style = "storm"
-- vim.g.tokyonight_style = "day"
-- vim.cmd[[colorscheme tokyonight]]
-- }}}

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

vim.g.zenburn_high_Contrast = 1
-- vim.cmd[[colorscheme zenburn]]

vim.cmd[[colorscheme jellybeans-nvim]]
vim.cmd[[
	autocmd ColorScheme * hi! DiffAdd guibg=#333333 guifg=#d2ebbe ctermbg=none
	autocmd ColorScheme * hi! DiffChange guibg=#333333 guifg=#dad085 ctermbg=none
	autocmd ColorScheme * hi! DiffDelete guibg=#333333 guifg=#f0a0c0 ctermbg=none
]]
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

vim.api.nvim_set_keymap('n', '<Leader>gs', ':vertical Git<CR>', { silent = true })

vim.api.nvim_set_keymap('n', '<C-a>', ':FSHere<CR>', { silent = true })

vim.api.nvim_set_keymap('n', '<f7>', ':Make<CR>', { silent = true })
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
    autocmd FileType lua setlocal shiftwidth=4 tabstop=4 expandtab
    autocmd FileType glsl setlocal shiftwidth=4 tabstop=4 expandtab
    autocmd FileType cpp setlocal shiftwidth=4 tabstop=4 expandtab
    autocmd FileType c setlocal shiftwidth=4 tabstop=4 expandtab
    autocmd FileType haskell setlocal shiftwidth=2 tabstop=2 expandtab
    autocmd FileType dart setlocal shiftwidth=2 tabstop=2 expandtab
]])
-- }}}

-- Completion {{{
-- vim.api.nvim_set_keymap('i', '<CR>', "compe#confirm(lexima#expand('<LT>CR>', 'i'))", { silent = true, expr = true })
-- vim.api.nvim_set_keymap('i', '<C-n>', 'compe#complete()', { silent = true, expr = true })
-- vim.api.nvim_set_keymap('i', '<C-p>', 'compe#complete()', { silent = true, expr = true })
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
end

vim.g.compiler_gcc_ignore_unmatched_lines = 1
-- vim.g.cpp_no_cpp17 = 1

nvim_create_augroups({
    cbindingslua = {
        {"Filetype", "c", "setlocal omnifunc=v:lua.vim.lsp.omnifunc"},
        {"Filetype", "c", "setlocal cpt-=t"},
        {"Filetype", "c", "setlocal shiftwidth=4 tabstop=4 expandtab"},
        {"Filetype", "c", "nmap <silent> <buffer> K          <cmd>lua vim.lsp.buf.hover()<CR>"},
        {"Filetype", "c", "nmap <silent> <buffer> <c-]>      <cmd>lua vim.lsp.buf.definition()<CR>"},
        {"Filetype", "c", "nmap <silent> <buffer> <Leader>mf <cmd>lua vim.lsp.buf.formatting_sync(nil, 1000)<CR>"},
        {"Filetype", "c", "nmap <silent> <buffer> <Leader>mr <cmd>lua vim.lsp.buf.rename()<CR>"},
        {"Filetype", "c", "nmap <silent> <buffer> <F7>       :Make<CR>"},
        {"Filetype", "c", "lua set_c_makeprg()"},
    },
    cppbindingslua = {
        {"Filetype", "cpp", "setlocal omnifunc=v:lua.vim.lsp.omnifunc"},
        {"Filetype", "cpp", "setlocal cpt-=t"},
        {"Filetype", "cpp", "setlocal shiftwidth=4 tabstop=4 expandtab"},
        {"Filetype", "cpp", "nmap <silent> <buffer> K          <cmd>lua vim.lsp.buf.hover()<CR>"},
        {"Filetype", "cpp", "nmap <silent> <buffer> <c-]>      <cmd>lua vim.lsp.buf.definition()<CR>"},
        {"Filetype", "cpp", "nmap <silent> <buffer> <Leader>mf <cmd>lua vim.lsp.buf.formatting_sync(nil, 1000)<CR>"},
        {"Filetype", "cpp", "nmap <silent> <buffer> <Leader>mr <cmd>lua vim.lsp.buf.rename()<CR>"},
        {"Filetype", "cpp", "nmap <silent> <buffer> <F7>       :Make<CR>"},
        {"Filetype", "cpp", "lua set_c_makeprg()"},
    },
})
-- }}}

-- Go {{{
nvim_create_augroups({
    gobindingslua = {
        {"Filetype", "go", "setlocal shiftwidth=4 tabstop=4 noexpandtab"},
        {"Filetype", "go", "setlocal omnifunc=v:lua.vim.lsp.omnifunc"},
        {"Filetype", "go", "setlocal cpt-=t"},
        {"Filetype", "go", "nmap <silent> <buffer> K          <cmd>lua vim.lsp.buf.hover()<CR>"},
        {"Filetype", "go", "nmap <silent> <buffer> <c-]>      <cmd>lua vim.lsp.buf.definition()<CR>"},
        {"Filetype", "go", "nmap <silent> <buffer> <Leader>mf <cmd>lua vim.lsp.buf.formatting_sync(nil, 1000)<CR>"},
        {"Filetype", "go", "nmap <silent> <buffer> <Leader>mr <cmd>lua vim.lsp.buf.rename()<CR>"},
        {"Filetype", "go", "nmap <silent> <buffer> <F7>       :Make<CR>"},
    },
})
-- }}}

-- Zig {{{
vim.g.zig_fmt_autosave = 0

nvim_create_augroups({
    zigbindingslua = {
        {"Filetype", "zig", "setlocal shiftwidth=4 tabstop=4 noexpandtab"},
        {"Filetype", "zig", "setlocal omnifunc=v:lua.vim.lsp.omnifunc"},
        {"Filetype", "zig", "setlocal cpt-=t"},
        {"Filetype", "zig", "nmap <silent> <buffer> K          <cmd>lua vim.lsp.buf.hover()<CR>"},
        {"Filetype", "zig", "nmap <silent> <buffer> <c-]>      <cmd>lua vim.lsp.buf.definition()<CR>"},
        {"Filetype", "zig", "nmap <silent> <buffer> <Leader>mf <cmd>lua vim.lsp.buf.formatting_sync(nil, 1000)<CR>"},
        {"Filetype", "zig", "nmap <silent> <buffer> <Leader>mr <cmd>lua vim.lsp.buf.rename()<CR>"},
        {"Filetype", "zig", "nmap <silent> <buffer> <F7>       :Make<CR>"},
    },
})
-- }}}

-- Javascript {{{
nvim_create_augroups({
    javascriptbindingslua = {
        {"Filetype", "javascript", "setlocal shiftwidth=2 tabstop=2 expandtab"},
        {"Filetype", "javascript", "setlocal omnifunc=v:lua.vim.lsp.omnifunc"},
        {"Filetype", "javascript", "setlocal cpt-=t"},
        {"Filetype", "javascript", "nmap <silent> <buffer> K          <cmd>lua vim.lsp.buf.hover()<CR>"},
        {"Filetype", "javascript", "nmap <silent> <buffer> <c-]>      <cmd>lua vim.lsp.buf.definition()<CR>"},
        {"Filetype", "javascript", "nmap <silent> <buffer> <Leader>mf <cmd>lua vim.lsp.buf.formatting_sync(nil, 1000)<CR>"},
        {"Filetype", "javascript", "nmap <silent> <buffer> <Leader>mr <cmd>lua vim.lsp.buf.rename()<CR>"},
        {"Filetype", "javascript", "nmap <silent> <buffer> <F7>       :Make<CR>"},
    },
    typescriptbindingslua = {
        {"Filetype", "typescript", "setlocal shiftwidth=2 tabstop=2 expandtab"},
        {"Filetype", "typescript", "setlocal omnifunc=v:lua.vim.lsp.omnifunc"},
        {"Filetype", "typescript", "setlocal cpt-=t"},
        {"Filetype", "typescript", "nmap <silent> <buffer> K          <cmd>lua vim.lsp.buf.hover()<CR>"},
        {"Filetype", "typescript", "nmap <silent> <buffer> <c-]>      <cmd>lua vim.lsp.buf.definition()<CR>"},
        {"Filetype", "typescript", "nmap <silent> <buffer> <Leader>mf <cmd>lua vim.lsp.buf.formatting_sync(nil, 1000)<CR>"},
        {"Filetype", "typescript", "nmap <silent> <buffer> <Leader>mr <cmd>lua vim.lsp.buf.rename()<CR>"},
        {"Filetype", "typescript", "nmap <silent> <buffer> <F7>       :Make<CR>"},
    },
    typescriptreactbindingslua = {
        {"Filetype", "typescriptreact", "setlocal shiftwidth=2 tabstop=2 expandtab"},
        {"Filetype", "typescriptreact", "setlocal omnifunc=v:lua.vim.lsp.omnifunc"},
        {"Filetype", "typescriptreact", "setlocal cpt-=t"},
        {"Filetype", "typescriptreact", "nmap <silent> <buffer> K          <cmd>lua vim.lsp.buf.hover()<CR>"},
        {"Filetype", "typescriptreact", "nmap <silent> <buffer> <c-]>      <cmd>lua vim.lsp.buf.definition()<CR>"},
        {"Filetype", "typescriptreact", "nmap <silent> <buffer> <Leader>mf <cmd>lua vim.lsp.buf.formatting_sync(nil, 1000)<CR>"},
        {"Filetype", "typescriptreact", "nmap <silent> <buffer> <Leader>mr <cmd>lua vim.lsp.buf.rename()<CR>"},
        {"Filetype", "typescriptreact", "nmap <silent> <buffer> <F7>       :Make<CR>"},
    },
})
-- }}}

-- Ocaml {{{
nvim_create_augroups({
    ocamlbindingslua = {
        {"Filetype", "ocaml", "setlocal shiftwidth=2 tabstop=2 expandtab"},
        {"Filetype", "ocaml", "setlocal omnifunc=v:lua.vim.lsp.omnifunc"},
        {"Filetype", "ocaml", "setlocal cpt-=t"},
        {"Filetype", "ocaml", "nmap <silent> <buffer> K          <cmd>lua vim.lsp.buf.hover()<CR>"},
        {"Filetype", "ocaml", "nmap <silent> <buffer> <c-]>      <cmd>lua vim.lsp.buf.definition()<CR>"},
        {"Filetype", "ocaml", "nmap <silent> <buffer> <Leader>mf <cmd>lua vim.lsp.buf.formatting_sync(nil, 1000)<CR>"},
        {"Filetype", "ocaml", "nmap <silent> <buffer> <Leader>mr <cmd>lua vim.lsp.buf.rename()<CR>"},
    },
})
-- }}}

-- Rust {{{
vim.g.cargo_makeprg_params = "check"

nvim_create_augroups({
    rustbindingslua = {
        {"Filetype", "rust", "setlocal shiftwidth=4 tabstop=4 expandtab"},
        {"Filetype", "rust", "setlocal omnifunc=v:lua.vim.lsp.omnifunc"},
        {"Filetype", "rust", "setlocal cpt-=t"},
        {"Filetype", "rust", "nmap <silent> <buffer> K          <cmd>lua vim.lsp.buf.hover()<CR>"},
        {"Filetype", "rust", "nmap <silent> <buffer> <c-]>      <cmd>lua vim.lsp.buf.definition()<CR>"},
        {"Filetype", "rust", "nmap <silent> <buffer> <Leader>mf <cmd>lua vim.lsp.buf.formatting_sync(nil, 1000)<CR>"},
        {"Filetype", "rust", "nmap <silent> <buffer> <Leader>mr <cmd>lua vim.lsp.buf.rename()<CR>"},
        {"Filetype", "rust", "nmap <silent> <buffer> <F7>       :Make<CR>"},
    },
})
-- }}}

-- Nim {{{
nvim_create_augroups({
    nimtbindingslua = {
        {"Filetype", "nim", "setlocal shiftwidth=2 tabstop=2 expandtab"},
        {"Filetype", "nim", "setlocal omnifunc=v:lua.vim.lsp.omnifunc"},
        {"Filetype", "nim", "setlocal cpt-=t"},
        {"Filetype", "nim", "nmap <silent> <buffer> K          <cmd>lua vim.lsp.buf.hover()<CR>"},
        {"Filetype", "nim", "nmap <silent> <buffer> <c-]>      <cmd>lua vim.lsp.buf.definition()<CR>"},
        {"Filetype", "nim", "nmap <silent> <buffer> <Leader>mf <cmd>lua vim.lsp.buf.formatting_sync(nil, 1000)<CR>"},
        {"Filetype", "nim", "nmap <silent> <buffer> <Leader>mr <cmd>lua vim.lsp.buf.rename()<CR>"},
        {"Filetype", "nim", "nmap <silent> <buffer> <F7>       :Make<CR>"},
        {"Filetype", "nim", "setlocal makeprg=nimble\\ --noColor\\ build"},
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
        {"Filetype", "dart", "setlocal omnifunc=v:lua.vim.lsp.omnifunc"},
        {"Filetype", "dart", "setlocal cpt-=t"},
        {"Filetype", "dart", "nmap <silent> <buffer> K          <cmd>lua vim.lsp.buf.hover()<CR>"},
        {"Filetype", "dart", "nmap <silent> <buffer> <c-]>      <cmd>lua vim.lsp.buf.definition()<CR>"},
        {"Filetype", "dart", "nmap <silent> <buffer> <Leader>mf <cmd>lua vim.lsp.buf.formatting_sync(nil, 1000)<CR>"},
        {"Filetype", "dart", "nmap <silent> <buffer> <Leader>mr <cmd>lua vim.lsp.buf.rename()<CR>"},
        {"Filetype", "dart", "nmap <silent> <buffer> <F7> :lua flutter_hot_reload()<CR>"},
    },
})
-- }}}
