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

" Polyglot

" Search
Plug 'junegunn/fzf.vim'

" Utility
Plug 'tpope/vim-surround'
Plug 'tpope/vim-commentary'
Plug 'tpope/vim-endwise'
Plug 'tpope/vim-repeat'
Plug 'tpope/vim-sleuth'
Plug 'tpope/vim-fugitive'
Plug 'tpope/vim-abolish'
Plug 'christoomey/vim-tmux-navigator'
Plug 'felipeagc/a.vim'
Plug 'editorconfig/editorconfig-vim'
Plug 'rhysd/vim-clang-format'
Plug 'cohama/lexima.vim'
Plug 'ludovicchabant/vim-gutentags'
Plug 'brooth/far.vim'
Plug 'airblade/vim-gitgutter'

" Themes
Plug 'nanotech/jellybeans.vim'

call plug#end()
" }}}

" Settings {{{
set mouse=a
set noshowcmd

set cursorline

" set number
" set relativenumber

set scrolloff=10
set clipboard=unnamedplus
set completeopt=noinsert,menuone,noselect

" set tabstop=4
" set shiftwidth=4
set autoindent
set smartindent
set smarttab

set wrap
set wildignore+=*.so,*.swp,*.zip,*.o,*.png,*.jpg,*.jpeg,*/target/*,*/build/*,*/node_modules/*,tags,*.glb,*.gltf,*.hdr
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
" set termguicolors
" set ttymouse=sgr

set ignorecase
set smartcase

" Don't auto indent ':' in c/c++
set cinoptions+=L0

" Open help vertically
autocmd FileType help wincmd L
autocmd FileType man wincmd L
" }}}

" Color scheme settings {{{
let g:jellybeans_use_gui_italics = 0
colorscheme jellybeans
" }}}

" Small quality of life stuff {{{

" Clear highlights with escape
nnoremap <silent> <esc> :noh<return><esc>

" Create non-existing directories before writing buffer
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

" Use ctrl-[hjkl] to select the active split
nmap <silent> <c-k> :wincmd k<CR>
nmap <silent> <c-j> :wincmd j<CR>
nmap <silent> <c-h> :wincmd h<CR>
nmap <silent> <c-l> :wincmd l<CR>

" Continuous indentation shift
vnoremap < <gv
vnoremap > >gv

" Remap :W to :w
cnoreabbrev W w
cnoreabbrev Q q
cnoreabbrev Wq wq
cnoreabbrev WQ wq

" Remap exit terminal to ESC
tnoremap <Esc> <C-\><C-n>

" Add more filetypes to closetag
let g:closetag_filenames = '*.html,*.xhtml,*.phtml,*.html.eex'

autocmd FileType netrw setl bufhidden=wipe
let g:netrw_fastbrowse = 0
" }}}

" FZF {{{
" Set FZF to use ripgrep
let $FZF_DEFAULT_COMMAND = 'rg --files --follow --glob "!.git/*"'

" ESC to exit FZF
autocmd FileType fzf tnoremap <buffer> <Esc> <Esc>
" }}}

" a.vim {{{
let g:alternateExtensions_vert = "frag"
let g:alternateExtensions_frag = "vert"
let g:alternateExtensions_glslv = "glslf"
let g:alternateExtensions_glslf = "glslv"
" }}}

" Gutentags {{{
let g:gutentags_generate_on_missing = 0
" }}}

" Folds {{{
set foldmethod=manual
autocmd	FileType vim setlocal foldlevel=0 " Close all folds
autocmd	FileType vim setlocal foldmethod=marker

augroup remember_folds
  autocmd!
  autocmd BufWinLeave *.* mkview
  autocmd BufWinEnter *.* silent! loadview
augroup END
" }}}

" Indentation {{{
autocmd FileType glsl setlocal shiftwidth=2 tabstop=2 expandtab
autocmd FileType javascript setlocal shiftwidth=2 tabstop=2 expandtab
autocmd FileType cpp setlocal shiftwidth=2 tabstop=2 expandtab
autocmd FileType c setlocal shiftwidth=2 tabstop=2 expandtab
autocmd FileType haskell setlocal shiftwidth=2 tabstop=2 expandtab
autocmd FileType lua setlocal shiftwidth=2 tabstop=2 expandtab
autocmd FileType json setlocal shiftwidth=2 tabstop=2 expandtab
" }}}

" Leader keybinds {{{
let mapleader = " "

nmap <silent> <leader>ff :Files<CR>
nmap <silent> <leader>fg :execute 'Rg '.input('Grep: ')<CR>
nmap <silent> <leader>fr :Farp<CR>
nmap <silent> <leader>fed :e $MYVIMRC<CR>
nmap <silent> <leader>fer :so $MYVIMRC<CR>

nmap <silent> <leader>tc :tabnew<CR>
nmap <silent> <leader>tn :tabnext<CR>
nmap <silent> <leader>tp :tabprev<CR>
nmap <silent> <leader>td :tabclose<CR>

nmap <silent> <leader>bn :bn<CR>
nmap <silent> <leader>bp :bp<CR>
nmap <silent> <leader>bb :Buffers<CR>
nmap <silent> <leader>bd :bd<CR>
nmap <silent> <leader>bD :bd!<CR>
nmap <silent> <leader>bcc :bufdo bd<CR>

nmap <silent> <leader>en :cnext<CR>
nmap <silent> <leader>ep :cprev<CR>
nmap <silent> <leader>el :clist<CR>

nmap <silent> <leader>w/ :vsplit<CR>
nmap <silent> <leader>w- :split<CR>
nmap <silent> <leader>wb <C-W>=
nmap <silent> <leader>wd :q<CR>

nmap <silent> <leader>gs :vertical Gstatus<CR>

nmap <silent> <leader>a :A<CR>
nmap <silent> <leader>A :AV<CR>
" }}}

" C/C++ bindings {{{
augroup cbindings
  autocmd!
  autocmd Filetype c nmap <buffer> <silent> <leader>mf :ClangFormat<CR>
  autocmd Filetype c nmap <buffer> <leader>mb :!ninja -C build<CR>
augroup end

augroup cppbindings
  autocmd!
  autocmd Filetype cpp nmap <buffer> <silent> <leader>mf :ClangFormat<CR>
  autocmd Filetype cpp nmap <buffer> <leader>mb :!ninja -C build<CR>
augroup end
" }}}
