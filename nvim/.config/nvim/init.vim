if &compatible
	set nocompatible
endif

" Plugins
call plug#begin('~/.local/share/nvim/plugged')

" Completion
Plug 'roxma/nvim-completion-manager'
" Plug 'Shougo/deoplete.nvim', { 'do': ':UpdateRemotePlugins' }
Plug 'Shougo/echodoc.vim'
" Plug 'ervandew/supertab'

" Error checking
Plug 'w0rp/ale'

" Fuzzy finders
Plug 'junegunn/fzf', { 'do': './install --bin'}
Plug 'junegunn/fzf.vim'

" Language Server Protocol
Plug 'autozimu/LanguageClient-neovim', { 'do': ':UpdateRemotePlugins' }

" Git
Plug 'tpope/vim-fugitive'

" Tags
Plug 'ludovicchabant/vim-gutentags'

" Org mode
Plug 'jceb/vim-orgmode'

" HTML
Plug 'mattn/emmet-vim'

" Haskell
Plug 'neovimhaskell/haskell-vim'

" Meson
Plug 'stfl/meson.vim'

" Rust
Plug 'rust-lang/rust.vim'

" Utilities
Plug 'tpope/vim-surround'
Plug 'tpope/vim-commentary'
Plug 'jiangmiao/auto-pairs'
Plug 'hecal3/vim-leader-guide'
Plug 'derekwyatt/vim-fswitch'

" Themes
Plug 'KeitaNakamura/neodark.vim'
Plug 'dylanaraps/wal.vim'
Plug 'rakr/vim-one'

" Other
Plug 'metakirby5/codi.vim'
Plug 'junegunn/goyo.vim'
Plug 'sheerun/vim-polyglot'
Plug 'wellle/targets.vim'

call plug#end()

" Settings
set mouse=a
set noshowcmd
" set number
set clipboard=unnamedplus
set completeopt-=preview
set tabstop=4
set shiftwidth=4
set cursorline
set wildignore+=*.so,*.swp,*.zip,*.o,*.png,*.jpg,*/target/*,*/build/*,*/node_modules/*
set noswapfile
set hidden
" set completeopt+=noselect
set noshowmode
set undofile
set undodir=~/.vim/undodir
set wrap
set linebreak
" note trailing space at end of next line
set showbreak=>\ \ \
set shortmess+=c

" if (has("nvim"))
" 	let $NVIM_TUI_ENABLE_TRUE_COLOR=1
" endif
" if (has("termguicolors"))
" 	set termguicolors
" endif

" Color scheme settings
set background=dark
colorscheme one

" Gutentags cache
let g:gutentags_cache_dir = '~/.local/share/gutentags'

" Echodoc configuration
let g:echodoc_enable_at_startup=1

" Make FZF use ripgrep
let $FZF_DEFAULT_COMMAND = 'rg --files --no-ignore --hidden --follow --glob "!.git/*"'

" ALE
let g:ale_linters = {
\   'rust': ['rls'],
\}
let g:ale_sign_column_always = 1

" Language Client configuration
let g:LanguageClient_serverCommands = {
    \ 'rust': ['rustup', 'run', 'nightly', 'rls'],
    \ }

let g:LanguageClient_autoStart = 1

nnoremap <silent> K :call LanguageClient_textDocument_hover()<CR>
nnoremap <silent> gd :call LanguageClient_textDocument_definition()<CR>
nnoremap <silent> <F2> :call LanguageClient_textDocument_rename()<CR>

" Use ctrl-[hjkl] to select the active split
nmap <silent> <c-k> :wincmd k<CR>
nmap <silent> <c-j> :wincmd j<CR>
nmap <silent> <c-h> :wincmd h<CR>
nmap <silent> <c-l> :wincmd l<CR>

" Beginnings and ends of lines
nnoremap H ^
nnoremap L $

" Continuous indentation shift
vnoremap < <gv
vnoremap > >gv

" Fix filetype indentations
autocmd Filetype crystal setlocal expandtab|setlocal shiftwidth=2|setlocal softtabstop=2
autocmd Filetype haskell setlocal expandtab|setlocal shiftwidth=2|setlocal softtabstop=2
autocmd Filetype cabal setlocal expandtab|setlocal shiftwidth=2|setlocal softtabstop=2
autocmd Filetype lua setlocal expandtab|setlocal shiftwidth=2|setlocal softtabstop=2
autocmd Filetype javascript setlocal expandtab|setlocal shiftwidth=2|setlocal softtabstop=2
autocmd Filetype typescript setlocal expandtab|setlocal shiftwidth=2|setlocal softtabstop=2

" Remove trailing whitespace on save
function! <SID>StripTrailingWhitespaces()
    let l = line(".")
    let c = col(".")
    %s/\s\+$//e
    call cursor(l, c)
endfun
autocmd BufWritePre * :call <SID>StripTrailingWhitespaces()

" Replace words with cn
nnoremap cn *``cgn

" Remap exit terminal to ESC
tnoremap <Esc> <C-\><C-n>

" Use tab for completion manager
inoremap <expr> <Tab> pumvisible() ? "\<C-n>" : "\<Tab>"
inoremap <expr> <S-Tab> pumvisible() ? "\<C-p>" : "\<S-Tab>"

" Leader menu

let mapleader = " "

let g:all_key_map = {}

let g:lmap = {}

let g:lmap.f = { 'name' : 'Find' }
let g:lmap.f.e = { 'name' : 'Config' }

nmap <silent> <leader>ff :Files<CR>
let g:lmap.f.f = ['', 'File']
nmap <silent> <leader>ft :Tags<CR>
let g:lmap.f.t = ['', 'Tags']
nmap <silent> <leader>fed :e $MYVIMRC<CR>
let g:lmap.f.e.d = ['', 'Edit']
nmap <silent> <leader>fer :so $MYVIMRC<CR>
let g:lmap.f.e.r = ['', 'Reload config']

let g:lmap.b = { 'name' : 'Buffer' }

nmap <silent> <leader>bn :bnext<CR>
let g:lmap.b.n = ['', 'Next']
nmap <silent> <leader>bp :bprevious<CR>
let g:lmap.b.p = ['', 'Previous']
nmap <silent> <leader>bb :Buffers<CR>
let g:lmap.b.b = ['', 'Find']
nmap <silent> <leader>bd :bdelete<CR>
let g:lmap.b.d = ['', 'Delete']

let g:lmap.e = { 'name' : 'Error' }

nmap <silent> <leader>en <Plug>(ale_next_wrap)
let g:lmap.e.n = ['', 'Next']
nmap <silent> <leader>ep <Plug>(ale_previous_wrap)
let g:lmap.e.p = ['', 'Previous']

let g:lmap.w = { 'name' : 'Window' }

nmap <silent> <leader>w/ :vsplit<CR>
let g:lmap.w['/'] = ['', 'Split vertically']
nmap <silent> <leader>w- :split<CR>
let g:lmap.w['-'] = ['', 'Split horizontally']

call leaderGuide#register_prefix_descriptions(" ", "g:lmap")

let g:all_key_map['<Leader>'] = g:lmap
let g:all_key_map['<Leader>']['name'] = '<Leader>'
nmap <silent> <Leader> :<c-u>LeaderGuide '<Leader>'<CR>
vmap <silent> <Leader> :<c-u>LeaderGuideVisual '<Leader>'<CR>

