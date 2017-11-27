if &compatible
	set nocompatible
endif

if empty(glob('~/.local/share/nvim/site/autoload/plug.vim'))
  silent !curl -fLo ~/.local/share/nvim/site/autoload/plug.vim --create-dirs
    \ https://raw.githubusercontent.com/junegunn/vim-plug/master/plug.vim
  autocmd VimEnter * PlugInstall --sync | source $MYVIMRC
endif

" Plugins
call plug#begin('~/.local/share/nvim/plugged')

" Completion
Plug 'roxma/nvim-completion-manager'
Plug 'Shougo/echodoc.vim'

" Error checking
Plug 'w0rp/ale'

" Fuzzy finders
Plug 'junegunn/fzf', { 'do': './install --bin'}
Plug 'junegunn/fzf.vim'

" Language Server Protocol
Plug 'autozimu/LanguageClient-neovim', { 'do': ':UpdateRemotePlugins' }

" Git
Plug 'tpope/vim-fugitive'

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
Plug 'derekwyatt/vim-fswitch'

" Themes
Plug 'joshdick/onedark.vim'

" Other
Plug 'metakirby5/codi.vim'
Plug 'sheerun/vim-polyglot'

call plug#end()

" Settings
" TODO: describe each of these, keep them to a minimum
set mouse=a
set noshowcmd
set number
set relativenumber
set scrolloff=10
set clipboard=unnamedplus
set completeopt-=preview
set tabstop=4
set shiftwidth=4
" set cursorline
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

" Color scheme settings
let g:onedark_termcolors = 16
colorscheme onedark

" Echodoc configuration
let g:echodoc_enable_at_startup=1

" Make FZF use ripgrep
let $FZF_DEFAULT_COMMAND = 'rg --files --hidden --follow --glob "!.git/*"'

" ALE
let g:ale_linters = {
\   'rust': ['rls'],
\   'python': ['flake8'],
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

" Remap :W to :w
cnoreabbrev W w
cnoreabbrev Q q
cnoreabbrev Wq wq
cnoreabbrev WQ wq

" Remap exit terminal to ESC
tnoremap <Esc> <C-\><C-n>

" Use tab for completion manager
inoremap <expr> <Tab> pumvisible() ? "\<C-n>" : "\<Tab>"
inoremap <expr> <S-Tab> pumvisible() ? "\<C-p>" : "\<S-Tab>"

" Esc to exit FZF
autocmd FileType fzf tnoremap <buffer> <Esc> <Esc>

" Leader keybinds
let mapleader = " "

nmap <silent> <leader>ff :Files<CR>
nmap <silent> <leader>ft :Tags<CR>
nmap <silent> <leader>fed :e $MYVIMRC<CR>
nmap <silent> <leader>fer :so $MYVIMRC<CR>

nmap <silent> <leader>bn :bnext<CR>
nmap <silent> <leader>bp :bprevious<CR>
nmap <silent> <leader>bb :Buffers<CR>
nmap <silent> <leader>bd :bdelete<CR>

nmap <silent> <leader>en <Plug>(ale_next_wrap)
nmap <silent> <leader>ep <Plug>(ale_previous_wrap)

nmap <silent> <leader>w/ :vsplit<CR>
nmap <silent> <leader>w- :split<CR>
