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

" Go
Plug 'fatih/vim-go', { 'do': ':GoInstallBinaries' }
Plug 'zchee/deoplete-go', { 'do': 'make'}

" Solidity
Plug 'tomlion/vim-solidity'

" Haskell
Plug 'neovimhaskell/haskell-vim'

" Elixir
Plug 'elixir-editors/vim-elixir'
Plug 'slashmili/alchemist.vim'

" Elm
Plug 'ElmCast/elm-vim'

" Typescript
Plug 'leafgarland/typescript-vim'
Plug 'peitalin/vim-jsx-typescript'

" GDScript
Plug 'quabug/vim-gdscript'

" Meson
Plug 'stfl/meson.vim'

" Polyglot
Plug 'sheerun/vim-polyglot'

" Snippets
Plug 'SirVer/ultisnips'
Plug 'honza/vim-snippets'
Plug 'mattn/emmet-vim'

" UI
Plug 'Shougo/echodoc.vim'
Plug 'scrooloose/nerdtree'
Plug 'equalsraf/neovim-gui-shim'
Plug 'Yggdroot/indentLine'

" Wrappers
Plug 'tpope/vim-fugitive'

" Completion / linting
Plug 'w0rp/ale'
Plug 'Shougo/deoplete.nvim', { 'do': ':UpdateRemotePlugins' }
Plug 'autozimu/LanguageClient-neovim', {
    \ 'branch': 'next',
    \ 'do': 'install.sh',
    \ }

" Search
Plug 'junegunn/fzf', { 'do': './install --bin'}
Plug 'junegunn/fzf.vim'

" Utility
Plug 'tpope/vim-surround'
Plug 'tpope/vim-commentary'
Plug 'jiangmiao/auto-pairs'
Plug 'derekwyatt/vim-fswitch'
Plug 'alvan/vim-closetag'
Plug 'editorconfig/editorconfig-vim'
Plug 'octref/RootIgnore'
Plug 'tpope/vim-endwise'
Plug 'christoomey/vim-tmux-navigator'

" Themes
Plug 'chriskempson/base16-vim'

call plug#end()
" }}}

" Settings {{{
set mouse=a
set noshowcmd

set number
set relativenumber

set scrolloff=10
set clipboard=unnamedplus
set completeopt-=preview

set tabstop=4
set shiftwidth=4
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
" }}}

" Color scheme settings {{{
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

autocmd BufWritePre * call s:Mkdir()
" }}}

" Disable polyglot for certain languages {{{
let g:polyglot_disabled = ['elm']
" }}}

" Deoplete {{{
let g:deoplete#enable_at_startup = 1
" }}}

" ALE {{{
let g:ale_linters = {
\   'rust': ['rls'],
\   'python': ['flake8'],
\	'go': ['gometalinter'],
\	'typescript': [],
\}
let g:ale_sign_column_always = 1
let g:ale_set_highlights = 0
" highlight clear ALEErrorSign
" highlight clear ALEWarningSign
" }}}

" Status line {{{
" function! LinterStatus() abort
"     let l:counts = ale#statusline#Count(bufnr(''))

"     let l:all_errors = l:counts.error + l:counts.style_error
"     let l:all_non_errors = l:counts.total - l:all_errors

"     return l:counts.total == 0 ? 'OK' : printf(
"     \   '%dW %dE',
"     \   all_non_errors,
"     \   all_errors
"     \)
" endfunction

" set statusline=
" set statusline+=%f\ %{LinterStatus()}\ %m\ %r\ %h
" set statusline+=%=
" set statusline+=%l,%v\ \ \ \ \ \ \ \ \ \ \ \ %y

" Function: display errors from Ale in statusline
function! LinterStatus() abort
   let l:counts = ale#statusline#Count(bufnr(''))
   let l:all_errors = l:counts.error + l:counts.style_error
   let l:all_non_errors = l:counts.total - l:all_errors
   return l:counts.total == 0 ? '' : printf(
   \ 'W:%d E:%d',
   \ l:all_non_errors,
   \ l:all_errors
   \)
endfunction

let g:currentmode={
    \ 'n'  : 'N ',
    \ 'no' : 'N·Operator Pending ',
    \ 'v'  : 'V ',
    \ 'V'  : 'V·Line ',
    \ '^V' : 'V·Block ',
    \ 's'  : 'Select ',
    \ 'S'  : 'S·Line ',
    \ '^S' : 'S·Block ',
    \ 'i'  : 'I ',
    \ 'R'  : 'R ',
    \ 'Rv' : 'V·Replace ',
    \ 'c'  : 'Command ',
    \ 'cv' : 'Vim Ex ',
    \ 'ce' : 'Ex ',
    \ 'r'  : 'Prompt ',
    \ 'rm' : 'More ',
    \ 'r?' : 'Confirm ',
    \ '!'  : 'Shell ',
    \ 't'  : 'Terminal '
    \}

hi User1 ctermbg=8		ctermfg=white   guibg=green guifg=red
hi User2 ctermbg=red	ctermfg=blue  guibg=red   guifg=blue
hi User3 ctermbg=blue	ctermfg=green guibg=blue  guifg=green

set laststatus=2
set statusline=
set statusline+=%1*\ %{toupper(g:currentmode[mode()])}%*
set statusline+=\ %f\ %*
set statusline+=\ %m
set statusline+=\ %{fugitive#statusline()}
set statusline+=%=
set statusline+=\ %{LinterStatus()}
set statusline+=\ %n
set statusline+=\ %y
set statusline+=\ \ %*
" }}}

" Language Client configuration {{{
let g:LanguageClient_serverCommands = {
    \ 'rust': ['rustup', 'run', 'stable', 'rls'],
    \ }

let g:LanguageClient_autoStart = 1
" }}}

" NERDTree {{{
nnoremap <C-t> :NERDTreeToggle<CR>
let NERDTreeRespectWildIgnore=1

" Close vim if the only window open in NERDTree
autocmd bufenter * if (winnr("$") == 1 && exists("b:NERDTree") && b:NERDTree.isTabTree()) | q | endif
" }}}

" Unbind C-Z {{{
nnoremap <c-z> <nop>
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
" }}}

" Esc to exit FZF {{{
autocmd FileType fzf tnoremap <buffer> <Esc> <Esc>
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

nmap <silent> <leader>w/ :vsplit<CR>
nmap <silent> <leader>w- :split<CR>
nmap <silent> <leader>wb <C-W>=
nmap <silent> <leader>wd :q<CR>
" }}}

" Go {{{
autocmd FileType go nmap <silent> <leader>mm :GoInfo<CR>
autocmd FileType go nmap <silent> <leader>md :GoDoc<CR>
let g:go_list_autoclose = 1
let g:go_metalinter_autosave = 0
" }}}

" Fix for JSX tags {{{
autocmd BufNewFile,BufRead *.tsx,*.jsx set filetype=typescript.jsx
exec "hi xmlTagName guifg=#" . g:base16_gui08
exec "hi htmlTagName guifg=#" . g:base16_gui08
exec "hi xmlTag guifg=#" . g:base16_gui08
exec "hi xmlEndTag guifg=#" . g:base16_gui08
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
