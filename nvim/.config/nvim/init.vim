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

Plug 'ncm2/ncm2'
Plug 'roxma/nvim-yarp'
Plug 'ncm2/ncm2-ultisnips'
Plug 'SirVer/ultisnips'
Plug 'Shougo/echodoc.vim'

Plug 'autozimu/LanguageClient-neovim', {
    \ 'branch': 'next',
    \ 'do': 'bash install.sh',
    \ }

" Polyglot
Plug 'sheerun/vim-polyglot'

" Search
Plug 'junegunn/fzf', { 'do': './install --bin'}
Plug 'junegunn/fzf.vim'

" Utility
Plug 'tpope/vim-surround'
Plug 'tpope/vim-commentary'
Plug 'tpope/vim-endwise'
Plug 'tpope/vim-repeat'
Plug 'tpope/vim-sleuth'
Plug 'tpope/vim-fugitive'
Plug 'christoomey/vim-tmux-navigator'
Plug 'derekwyatt/vim-fswitch'
Plug 'editorconfig/editorconfig-vim'
Plug 'metakirby5/codi.vim'
Plug 'qpkorr/vim-bufkill'

" Themes
Plug 'AlessandroYorba/Sierra'

call plug#end()
" }}}

" Settings {{{
set mouse=a
set noshowcmd

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
let g:sierra_Midnight = 1
colorscheme sierra 

" let base16colorspace=256

" function! s:base16_customize() abort
"   call Base16hi("LineNr", g:base16_gui03, g:base16_gui00, g:base16_cterm03, g:base16_cterm00, "bold", "")
"   call Base16hi("CursorLineNr", g:base16_gui04, g:base16_gui00, g:base16_cterm04, g:base16_cterm00, "bold", "")
"   call Base16hi("SignColumn", g:base16_gui03, g:base16_gui00, g:base16_cterm03, g:base16_cterm00, "bold", "")
" endfunction

" augroup on_change_colorschema
"   autocmd!
"   autocmd ColorScheme * call s:base16_customize()
" augroup END

" colorscheme base16-default-dark
" }}}

" Small quality of life stuff {{{

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

" Beginnings and ends of lines
nnoremap H ^
nnoremap L $

" Continuous indentation shift
vnoremap < <gv
vnoremap > >gv

" Replace words with cn
nnoremap cn *``cgn

" Remap :W to :w
cnoreabbrev W w
cnoreabbrev Q q
cnoreabbrev Wq wq
cnoreabbrev WQ wq

" Remap exit terminal to ESC
tnoremap <Esc> <C-\><C-n>

" Add more filetypes to closetag
let g:closetag_filenames = '*.html,*.xhtml,*.phtml,*.html.eex'

" }}}

" NCM2 {{{
autocmd BufEnter * call ncm2#enable_for_buffer()

let g:UltiSnipsExpandTrigger       = "<Plug>(ultisnips_expand_or_jump)"
let g:UltiSnipsJumpForwardTrigger  = "<Plug>(ultisnips_expand_or_jump)"
" Let UltiSnips bind the jump backward trigger as there's nothing special
" about it.
let g:UltiSnipsJumpBackwardTrigger = "<S-Tab>"

" Try expanding snippet or jumping with UltiSnips and return <Tab> if nothing
" worked.
function! UltiSnipsExpandOrJumpOrTab()
  call UltiSnips#ExpandSnippetOrJump()
  if g:ulti_expand_or_jump_res > 0
    return ""
  else
    return "\<Tab>"
  endif
endfunction

" First try expanding with ncm2_ultisnips. This does both LSP snippets and
" normal snippets when there's a completion popup visible.
inoremap <silent> <expr> <Tab> ncm2_ultisnips#expand_or("\<Plug>(ultisnips_try_expand)")

" If that failed, try the UltiSnips expand or jump function. This handles
" short snippets when the completion popup isn't visible yet as well as
" jumping forward from the insert mode. Writes <Tab> if there is no special
" action taken.
inoremap <silent> <Plug>(ultisnips_try_expand) <C-R>=UltiSnipsExpandOrJumpOrTab()<CR>

" Select mode mapping for jumping forward with <Tab>.
snoremap <silent> <Tab> <Esc>:call UltiSnips#ExpandSnippetOrJump()<cr>
" }}}

" Echodoc {{{
set cmdheight=2
let g:echodoc#enable_at_startup = 1
let g:echodoc#type = 'signature'
" }}}

" LSP {{{
" \ 'cpp': ['cquery', '--log-file=/tmp/cq.log', '--init={"cacheDirectory":"/tmp/cquery"}'],
let g:LanguageClient_serverCommands = {
  \ 'cpp': ['clangd', '-compile-commands-dir=' . getcwd() . '/build'],
  \ }

nnoremap <silent> K :call LanguageClient#textDocument_hover()<CR>
nnoremap <silent> gd :call LanguageClient#textDocument_definition()<CR>
nnoremap <silent> <F2> :call LanguageClient#textDocument_rename()<CR>
" }}}

" FZF {{{
" Set FZF to use ripgrep
let $FZF_DEFAULT_COMMAND = 'rg --files --follow --glob "!.git/*"'

" ESC to exit FZF
autocmd FileType fzf tnoremap <buffer> <Esc> <Esc>
" }}}

" Folds {{{
set foldmethod=manual
autocmd	FileType vim setlocal foldlevel=0 " Close all folds
autocmd	FileType vim setlocal foldmethod=marker

augroup remember_folds
  autocmd!
  autocmd BufWinLeave * mkview
  autocmd BufWinEnter * silent! loadview
augroup END
" }}}

" Indentation {{{
autocmd FileType glsl setlocal shiftwidth=2 tabstop=2 expandtab
autocmd FileType javascript setlocal shiftwidth=2 tabstop=2 expandtab
autocmd FileType cpp setlocal shiftwidth=2 tabstop=2 expandtab
autocmd FileType haskell setlocal shiftwidth=2 tabstop=2 expandtab
autocmd FileType lua setlocal shiftwidth=2 tabstop=2 expandtab
" }}}

" Leader keybinds {{{
let mapleader = " "

nmap <silent> <leader>ff :Files<CR>
nmap <silent> <leader>fg :Rg<CR>
nmap <silent> <leader>fed :e $MYVIMRC<CR>
nmap <silent> <leader>fer :so $MYVIMRC<CR>

nmap <silent> <leader>bn :BF<CR>
nmap <silent> <leader>bp :BB<CR>
nmap <silent> <leader>bb :Buffers<CR>
nmap <silent> <leader>bd :BD<CR>

nmap <silent> <leader>en :cnext<CR>
nmap <silent> <leader>ep :cprev<CR>
nmap <silent> <leader>el :clist<CR>

nmap <silent> <leader>w/ :vsplit<CR>
nmap <silent> <leader>w- :split<CR>
nmap <silent> <leader>wb <C-W>=
nmap <silent> <leader>wd :q<CR>

nmap <silent> <leader>mf :call LanguageClient#textDocument_formatting()<CR>

nmap <silent> <leader>a :FSHere<CR>
" }}}
