if &compatible
  set nocompatible
endif

syntax enable
filetype plugin on
filetype plugin indent on

" Automatically install plugin manager {{{
if empty(glob('~/.local/share/nvim/site/autoload/plug.vim'))
  silent !curl -fLo ~/.local/share/nvim/site/autoload/plug.vim --create-dirs
        \ https://raw.githubusercontent.com/junegunn/vim-plug/master/plug.vim
  autocmd VimEnter * PlugInstall --sync | source $MYVIMRC
endif
" }}}

" Plugins {{{
call plug#begin('~/.local/share/nvim/plugged')

" Search
Plug 'junegunn/fzf.vim'

" Utility
Plug 'tpope/vim-surround'
Plug 'tpope/vim-commentary'
Plug 'tpope/vim-endwise'
Plug 'tpope/vim-repeat'
Plug 'tpope/vim-fugitive'
Plug 'tpope/vim-abolish'
Plug 'felipeagc/a.vim'
Plug 'editorconfig/editorconfig-vim'
Plug 'rhysd/vim-clang-format'
Plug 'cohama/lexima.vim'
Plug 'ludovicchabant/vim-gutentags'
Plug 'airblade/vim-gitgutter'
Plug 'derekwyatt/vim-fswitch'
Plug 'brooth/far.vim'

" Languages
Plug 'tikhomirov/vim-glsl'
Plug 'beyondmarc/hlsl.vim'
Plug 'ziglang/zig.vim'
Plug 'fatih/vim-go'
Plug '~/.local/share/nvim/plugged/fl.vim'

" Themes
Plug 'gruvbox-community/gruvbox'

call plug#end()
" }}}

" Settings {{{
set exrc
set mouse=a
set noshowcmd

set cursorline

" set number
" set relativenumber

set scrolloff=10
set clipboard=unnamedplus
set completeopt=noinsert,menuone,noselect

set tabstop=4
set shiftwidth=4
set autoindent
set smartindent
set smarttab

set wrap
set wildignore+=*.so,*.swp,*.zip,*.o,*.png,*.jpg,*.jpeg,*/target/*,*/build/*,*/node_modules/*,tags,*.glb,*.gltf,*.hdr
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
set cinoptions+=l1
" }}}

" Color scheme settings {{{
let g:gruvbox_contrast_dark = 'hard'
let g:gruvbox_sign_column = 'bg0'
let g:gruvbox_termcolors=16

colorscheme gruvbox
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

" Continuous indentation shift
vnoremap < <gv
vnoremap > >gv
vnoremap <s-lt> <gv

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

" Fix false positive C bracket error
let c_no_bracket_error=1
let c_no_curly_error = 1

" Open help vertically
autocmd FileType help wincmd L
autocmd FileType man wincmd L

" netrw
let g:netrw_banner=0
let g:netrw_browse_split=4  " open in prior window
let g:netrw_altv=1          " split to the right
let g:netrw_liststyle=3     " tree view
let g:netrw_list_hide=netrw_gitignore#Hide()
let g:netrw_localrmdir='rm -r'

nnoremap <C-h> <C-w>h
nnoremap <C-j> <C-w>j
nnoremap <C-k> <C-w>k
nnoremap <C-l> <C-w>l
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
set foldmethod=marker
set foldlevel=1
autocmd	FileType vim setlocal foldlevel=0 " Close all folds
autocmd	FileType vim setlocal foldmethod=marker
" }}}

" Indentation {{{
autocmd FileType glsl setlocal shiftwidth=4 tabstop=4 expandtab
autocmd FileType javascript setlocal shiftwidth=2 tabstop=2 expandtab
autocmd FileType cpp setlocal shiftwidth=4 tabstop=4 expandtab
autocmd FileType c setlocal shiftwidth=4 tabstop=4 expandtab
autocmd FileType haskell setlocal shiftwidth=2 tabstop=2 expandtab
autocmd FileType lua setlocal shiftwidth=2 tabstop=2 expandtab
autocmd FileType json setlocal shiftwidth=2 tabstop=2 expandtab
autocmd FileType go setlocal shiftwidth=4 tabstop=4 noexpandtab
autocmd FileType rmd setlocal shiftwidth=2 tabstop=2 expandtab
autocmd FileType bzl setlocal shiftwidth=4 tabstop=4 expandtab
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
nmap <silent> <leader>el :copen<CR>
nmap <silent> <leader>eL :cclose<CR>

nmap <silent> <leader>w/ :vsplit<CR>
nmap <silent> <leader>w- :split<CR>
nmap <silent> <leader>wb <C-W>=
nmap <silent> <leader>wd :q<CR>

nmap <silent> <leader>gs :vertical Gstatus<CR>

nmap <silent> <leader>a :FSHere<CR>
nmap <silent> <leader>A :FSSplitRight<CR>
" }}}

" C/C++ {{{
let g:compiler_gcc_ignore_unmatched_lines = 1

augroup cbindings
  autocmd!
  autocmd Filetype c setlocal makeprg=ninja\ -C\ build
  autocmd Filetype c nmap <buffer> <silent> <leader>mf :ClangFormat<CR>
  autocmd Filetype c nmap <buffer> <leader>mb :make<CR>
augroup end

augroup cppbindings
  autocmd!
  autocmd Filetype cpp setlocal makeprg=ninja\ -C\ build
  autocmd Filetype cpp nmap <buffer> <silent> <leader>mf :ClangFormat<CR>
  autocmd Filetype cpp nmap <buffer> <leader>mb :make<CR>
augroup end
" }}}

" LaTeX {{{
let g:tex_flavor="latex"

function! LaunchZathura()
  execute 'silent !zathura "' . expand('%:p:r') . '.pdf" &'
endfunction

augroup texbindings
  autocmd!
  autocmd Filetype tex nmap <buffer> <silent> <leader>mp :call LaunchZathura()<CR>
  autocmd Filetype tex nmap <buffer> <leader>mb :!pdflatex "%"<CR>
augroup end
" }}}

" R Markdown {{{
augroup rmdbindings
  autocmd!
  autocmd Filetype rmd nmap <buffer> <silent> <leader>mp :call LaunchZathura()<CR>
  autocmd Filetype rmd nmap <buffer> <leader>mb :!echo<space>"require(rmarkdown);<space>render('<c-r>%')"<space>\|<space>R<space>--vanilla<enter>
augroup end
" }}}

" Zig {{{
let g:zig_fmt_autosave = 0

augroup zigbindings
  autocmd!
  autocmd Filetype zig nmap <buffer> <leader>mb :!zig build<CR>
augroup end
" }}}

" Go {{{
augroup gobindings
  autocmd!
  autocmd Filetype go nmap <buffer> <leader>mb :GoBuild<CR>
  autocmd Filetype go nmap <buffer> <leader>mf :GoFmt<CR>
  autocmd Filetype go nmap <buffer> <leader>mi :GoImports<CR>
augroup end
" }}}

" Python {{{
augroup pythonbindings
  autocmd!
  autocmd Filetype python nmap <buffer> <leader>mf :YAPF<CR>
augroup end
" }}}

" D {{{
augroup dbindings
  autocmd FileType d setlocal efm=%*[^@]@%f\(%l\):\ %m,%f\(%l\\,%c\):\ %m,%f\(%l\):\ %m
  autocmd Filetype d setlocal makeprg=dub\ build\ -q
  autocmd Filetype d nmap <buffer> <leader>mb :make<CR>
augroup end
"}}}

" GLSL {{{
augroup glslbindings
  autocmd Filetype glsl setlocal makeprg=make\ -C\ shaders
  autocmd Filetype glsl nmap <buffer> <leader>mb :make<CR>
  autocmd BufEnter *.frag let b:fswitchdst = 'vert' | let b:fswitchlocs = '.'
  autocmd BufEnter *.vert let b:fswitchdst = 'frag' | let b:fswitchlocs = '.'
augroup end
" }}}
