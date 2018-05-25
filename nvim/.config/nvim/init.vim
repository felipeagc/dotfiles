if &compatible
  set nocompatible
endif

" Automatically install plugin manager {{{
if empty(glob('~/.local/share/nvim/site/autoload/plug.vim'))
  silent !curl -fLo ~/.local/share/nvim/site/autoload/plug.vim --create-dirs
    \ https://raw.githubusercontent.com/junegunn/vim-plug/master/plug.vim
  autocmd VimEnter * PlugInstall --sync | source $MYVIMRC
endif
" }}}

" Plugins {{{
call plug#begin('~/.local/share/nvim/plugged')

" Completion / linting
Plug 'w0rp/ale'
Plug 'Shougo/deoplete.nvim', { 'do': ':UpdateRemotePlugins' }

" C/C++
Plug 'rhysd/vim-clang-format'

" Meson
Plug 'stfl/meson.vim'

" Polyglot
Plug 'sheerun/vim-polyglot'

" Snippets
Plug 'SirVer/ultisnips'
Plug 'honza/vim-snippets'
Plug 'mattn/emmet-vim'

" Writing
Plug 'junegunn/goyo.vim'

" UI
Plug 'equalsraf/neovim-gui-shim'
Plug 'itchyny/lightline.vim'

" Wrappers
Plug 'tpope/vim-fugitive'

" Search
Plug 'junegunn/fzf', { 'do': './install --bin'}
Plug 'junegunn/fzf.vim'

" Utility
Plug 'tpope/vim-surround'
Plug 'tpope/vim-commentary'
Plug 'tpope/vim-endwise'
Plug 'tpope/vim-repeat'
Plug 'tpope/vim-sleuth'
Plug 'jiangmiao/auto-pairs'
Plug 'derekwyatt/vim-fswitch'
Plug 'alvan/vim-closetag'
Plug 'octref/RootIgnore'
Plug 'christoomey/vim-tmux-navigator'
Plug 'junegunn/gv.vim'
Plug 'derekwyatt/vim-fswitch'
Plug 'justinmk/vim-sneak'
Plug 'Shougo/vimproc.vim', { 'do': 'make' }
Plug 'editorconfig/editorconfig-vim'

" Themes
Plug 'chriskempson/base16-vim'

call plug#end()
" }}}

" Settings {{{
" set mouse=a
set noshowcmd

set number
set relativenumber

set scrolloff=10
set clipboard=unnamedplus
set completeopt-=preview

" set tabstop=4
" set shiftwidth=4
set autoindent
set smartindent
set smarttab

set nowrap
" set cursorline
set wildignore+=*.so,*.swp,*.zip,*.o,*.png,*.jpg,*/target/*,*/build/*,*/node_modules/*
set noswapfile
set hidden
" set completeopt+=noselect
set noshowmode

set undofile
set undodir=~/.vim/undodir

set linebreak
" note trailing space at end of next line
set showbreak=>\ \ \
set shortmess+=c
set signcolumn=yes
" set termguicolors
" set ttymouse=sgr

set ignorecase
set smartcase
" }}}

" Color scheme settings {{{
let base16colorspace=256

function! s:base16_customize() abort
  call Base16hi("LineNr", g:base16_gui03, g:base16_gui00, g:base16_cterm03, g:base16_cterm00, "bold", "")
  call Base16hi("CursorLineNr", g:base16_gui04, g:base16_gui00, g:base16_cterm04, g:base16_cterm00, "bold", "")
  call Base16hi("SignColumn", g:base16_gui03, g:base16_gui00, g:base16_cterm03, g:base16_cterm00, "bold", "")
endfunction

augroup on_change_colorschema
  autocmd!
  autocmd ColorScheme * call s:base16_customize()
augroup END

colorscheme base16-default-dark
" }}}

" Create non-existing directories before writing buffer {{{
function! s:Mkdir()
  let dir = expand('%:p:h')

  if !isdirectory(dir)
    call mkdir(dir, 'p')
    echo 'Created non-existing directory: '.dir
  endif
endfunction

augroup on_buffer_write
	autocmd BufWritePre * call s:Mkdir()
augroup END
" }}}

" Deoplete {{{
let g:deoplete#enable_at_startup = 1
let g:deoplete#keyword_patterns = {}
" }}}

" ALE {{{
let g:ale_linters = {
  \'rust': ['rls'],
  \'python': ['flake8'],
  \'go': ['gometalinter'],
  \'typescript': [],
  \'elixir': [],
  \'cpp': ['clangcheck'],
  \}

let g:ale_fixers = {
  \'javascript': ['eslint']
  \}
let g:ale_sign_column_always = 1
let g:ale_set_highlights = 1
let g:ale_c_build_dir = './build'
" highlight clear ALEErrorSign
" highlight clear ALEWarningSign
" }}}

" Status line {{{
let g:lightline = {
      \ 'colorscheme': 'jellybeans',
      \ }
" }}}

" Language Client configuration {{{
let g:LanguageClient_serverCommands = {
    \ 'cpp': ['clangd'],
    \ 'c': ['clangd'],
    \ }

let g:LanguageClient_autoStart = 1
" }}}

" Use ctrl-[hjkl] to select the active split {{{
nmap <silent> <c-k> :wincmd k<CR>
nmap <silent> <c-j> :wincmd j<CR>
nmap <silent> <c-h> :wincmd h<CR>
nmap <silent> <c-l> :wincmd l<CR>
" }}}

" Beginnings and ends of lines {{{
nnoremap H ^
nnoremap L $
" }}}

" Continuous indentation shift {{{
vnoremap < <gv
vnoremap > >gv
" }}}

" Replace words with cn {{{
nnoremap cn *``cgn
" }}}

" Remap :W to :w {{{
cnoreabbrev W w
cnoreabbrev Q q
cnoreabbrev Wq wq
cnoreabbrev WQ wq
" }}}

" Remap exit terminal to ESC {{{
tnoremap <Esc> <C-\><C-n>
" }}}

" FZF {{{
" Set FZF to use ripgrep
let $FZF_DEFAULT_COMMAND = 'rg --files --follow --glob "!.git/*"'

" ESC to exit FZF
autocmd FileType fzf tnoremap <buffer> <Esc> <Esc>
" }}}

" Go {{{
let g:go_list_type = "quickfix"
let g:go_list_autoclose = 1
let g:go_metalinter_autosave = 0
let g:go_metalinter_enabled = []
let g:go_metalinter_autosave_enabled = []
" }}}

" Workaround for https://github.com/jiangmiao/auto-pairs/issues/187 {{{
autocmd VimEnter,BufEnter,BufWinEnter * silent! iunmap <buffer> <M-">
" }}}

" Add more filetypes to closetag {{{
let g:closetag_filenames = '*.html,*.xhtml,*.phtml,*.html.eex'
" }}}

" UltiSnips {{{
let g:UltiSnipsExpandTrigger="<tab>"
let g:UltiSnipsJumpForwardTrigger="<tab>"
let g:UltiSnipsJumpBackwardTrigger="<s-tab>"
" }}}

" Folds {{{
set foldmethod=syntax
set foldlevel=99
autocmd	FileType vim setlocal foldlevel=0 " Close all folds
autocmd	FileType vim setlocal foldmethod=marker
" }}}

" Tab bindings {{{
nnoremap tt :tabnew<cr>
nnoremap tc :tabclose<cr>
nnoremap tn :tabnext<cr>
nnoremap tp :tabprev<cr>
" }}}

" Elm {{{
let g:elm_setup_keybindings = 0
autocmd FileType elm map <buffer> K :ElmShowDocs<CR>
" }}}

" Indentation {{{
autocmd FileType glsl setlocal shiftwidth=2 tabstop=2 expandtab
autocmd FileType javascript setlocal shiftwidth=2 tabstop=2 expandtab
autocmd FileType cpp setlocal shiftwidth=2 tabstop=2 expandtab
autocmd FileType haskell setlocal shiftwidth=2 tabstop=2 expandtab
autocmd FileType lua setlocal shiftwidth=2 tabstop=2 expandtab
" }}}

" Rust racer {{{
let g:deoplete#sources#rust#racer_binary="/usr/bin/racer"
let g:deoplete#sources#rust#rust_source_path="~/.rustup/toolchains/stable-x86_64-unknown-linux-gnu/lib/rustlib/src/rust/src/"
autocmd Filetype rust nmap <buffer> gd <plug>DeopleteRustGoToDefinitionDefault
autocmd Filetype rust nmap <buffer> K  <plug>DeopleteRustShowDocumentation
" }}}

" Haskell {{{
let g:intero_use_neomake = 0
let g:hindent_on_save = 0

autocmd Filetype haskell map  <buffer> K :InteroInfo<CR>
" }}}

" Javascript {{{
let g:jsx_ext_required = 0
" }}}

" Fix for base16's highlighting of XML tags {{{
hi def link xmlEndTag Function
" }}}

" C/C++ {{{
let g:clang_format#auto_format = 1
let g:clang_format#style_options = {
  \ "SortIncludes": "false"
  \}
" }}}

" Emmet {{{
let g:user_emmet_expandabbr_key = '<C-e>'
" }}}

" Run love2d game {{{
autocmd FileType lua nmap <silent> <leader>mr :!love .<CR>
" }}}

" Leader keybinds {{{
let mapleader = " "

nmap <silent> <leader>ff :Files<CR>
nmap <silent> <leader>fg :GFiles<CR>
nmap <silent> <leader>ft :Tags<CR>
nmap <silent> <leader>fed :e $MYVIMRC<CR>
nmap <silent> <leader>fer :so $MYVIMRC<CR>

nmap <silent> <leader>tn :tabnext<CR>
nmap <silent> <leader>tp :tabp<CR>
nmap <silent> <leader>td :tabclose<CR>
nmap <silent> <leader>tN :tabnew<CR>

nmap <silent> <leader>bn :bnext<CR>
nmap <silent> <leader>bp :bprevious<CR>
nmap <silent> <leader>bb :Buffers<CR>
nmap <silent> <leader>bd :bdelete<CR>

nmap <silent> <leader>en <Plug>(ale_next_wrap)
nmap <silent> <leader>ep <Plug>(ale_previous_wrap)
nmap <silent> <leader>ef <Plug>(ale_fix)

nmap <silent> <leader>w/ :vsplit<CR>
nmap <silent> <leader>w- :split<CR>
nmap <silent> <leader>wb <C-W>=
nmap <silent> <leader>wd :q<CR>

nmap <silent> <leader>a :FSHere<CR>
" }}}
