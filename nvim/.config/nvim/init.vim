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
Plug 'junegunn/fzf'
Plug 'junegunn/fzf.vim'

" Utility
Plug 'tpope/vim-surround'
Plug 'tpope/vim-commentary'
Plug 'tpope/vim-endwise'
Plug 'tpope/vim-repeat'
Plug 'tpope/vim-fugitive'
Plug 'tpope/vim-abolish'
Plug 'tpope/vim-unimpaired'
Plug 'tpope/vim-dispatch'
Plug 'editorconfig/editorconfig-vim'
Plug 'cohama/lexima.vim' " Auto closing braces
Plug 'ludovicchabant/vim-gutentags'
" Plug 'airblade/vim-gitgutter'
Plug 'derekwyatt/vim-fswitch'
Plug 'brooth/far.vim'
Plug 'tommcdo/vim-lion' " Alignment
Plug 'dense-analysis/ale'
" Plug 'octol/vim-cpp-enhanced-highlight'
Plug 'bfrg/vim-cpp-modern'

" Languages
Plug 'tikhomirov/vim-glsl'
Plug 'beyondmarc/hlsl.vim'
Plug 'ziglang/zig.vim'
" Plug 'chadversary/vim-meson'
Plug '~/.local/share/nvim/plugged/fl.vim'

" Themes
Plug 'gruvbox-community/gruvbox'
Plug 'sainnhe/gruvbox-material'
Plug 'junegunn/seoul256.vim'

call plug#end()
" }}}

" Settings {{{
set exrc
set mouse=a
set noshowcmd

" set cursorline

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
set modeline
set modelines=1

set undofile
set undodir=~/.vim/undodir

set linebreak
" note trailing space at end of next line
set showbreak=>\ \ \
set shortmess+=c
set termguicolors
" set ttymouse=sgr

set ignorecase
set smartcase

" Don't auto indent ':' in c/c++
set cinoptions+=L0
set cinoptions+=l1
" }}}

" Color scheme settings {{{
" colorscheme gruvbox
let g:seoul256_srgb = 1
let g:seoul256_background = 234
colo seoul256
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

nnoremap <C-j> <C-w>w
nnoremap <C-k> <C-w>W
" }}}

" ALE {{{
let g:ale_completion_enabled = 1
let g:ale_linters = {
\   'c': [],
\   'cpp': [],
\   'd': [],
\   'python': [],
\   'tex': [],
\   'zig': [],
\   'go': ['gopls'],
\}
let g:ale_fixers = {
\   'c': ['clang-format'],
\   'cpp': ['clang-format'],
\   'd': [],
\   'python': [],
\   'tex': [],
\   'zig': [],
\   'go': ['goimports', 'gofmt'],
\}

let g:ale_set_loclist = 0
let g:ale_set_quickfix = 1
" }}}

" FZF {{{
let $FZF_DEFAULT_OPTS='--color=gutter:-1 --layout=reverse'

if executable('ag')
	let $FZF_DEFAULT_COMMAND = 'ag -g ""'
endif

let g:fzf_preview_window = ''

autocmd FileType fzf tnoremap <buffer> <Esc> <Esc>
" }}}

" Gutentags {{{
let g:gutentags_generate_on_missing = 0
" }}}

" Folds {{{
set foldmethod=marker
set foldlevel=0
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
" }}}

" Leader keybinds {{{
let mapleader = " "

nmap <silent> <c-p> :Files<CR>
nmap <silent> <leader>fg :execute 'Ag '.input('Grep: ')<CR>
nmap <silent> <leader>fr :Farp<CR>
nmap <silent> <leader>fed :e $MYVIMRC<CR>
nmap <silent> <leader>feg :e $HOME/.config/nvim/ginit.vim<CR>

nmap <silent> <leader>tc :tabnew<CR>
nmap <silent> <leader>td :tabclose<CR>

nmap <silent> <leader>bn :bn<CR>
nmap <silent> <leader>bp :bp<CR>
nmap <silent> <leader>bb :Buffers<CR>
nmap <silent> <leader>bd :bd<CR>
nmap <silent> <leader>bD :bd!<CR>
nmap <silent> <leader>bcc :bufdo bd<CR>

nmap <silent> <leader>db :Break<CR>
nmap <silent> <leader>dB :Clear<CR>
nmap <silent> <leader>ds :Step<CR>
nmap <silent> <leader>dn :Over<CR>

nmap <silent> <leader>w/ :vsplit<CR>
nmap <silent> <leader>w- :split<CR>
nmap <silent> <leader>wb <C-W>=
nmap <silent> <leader>wd :q<CR>

nmap <silent> <leader>gs :vertical Gstatus<CR>

nmap <silent> <leader>a :FSHere<CR>
nmap <silent> <leader>A :FSSplitRight<CR>

nmap <silent> <leader>mc :Copen<CR>
" }}}

" C/C++ {{{
let g:compiler_gcc_ignore_unmatched_lines = 1
let g:cpp_no_cpp17 = 1

function! SetCMakeprg()
	" We have to add this pattern twice because dispatch.vim is retarded
	setlocal errorformat+=%-G%.%#,%-G%.%# 

	if !empty(glob("meson.build"))
		setlocal makeprg=ninja\ -C\ build
	endif
	if !empty(glob("CMakeLists.txt"))
		setlocal makeprg=ninja\ -C\ build
	endif
	if !empty(glob("makefile")) || !empty(glob("Makefile"))
		setlocal makeprg=make
	endif
endfunction

augroup cbindings
  autocmd!
  autocmd Filetype c call SetCMakeprg()
  autocmd Filetype c nmap <buffer> <silent> <leader>mf :ALEFix<CR>
  autocmd Filetype c nmap <buffer> <F7> :Make<CR>
augroup end

augroup cppbindings
  autocmd!
  autocmd Filetype cpp call SetCMakeprg()
  autocmd Filetype cpp nmap <buffer> <silent> <leader>mf :ALEFix<CR>
  autocmd Filetype cpp nmap <buffer> <F7> :Make<CR>
augroup end

augroup cudabindings
  autocmd!
  autocmd Filetype cuda call SetCMakeprg()
  autocmd Filetype cuda nmap <buffer> <silent> <leader>mf :ALEFix<CR>
  autocmd Filetype cuda nmap <buffer> <F7> :Make<CR>
augroup end
" }}}

" LaTeX {{{
let g:tex_flavor="latex"

function! LaunchZathura()
  execute 'silent !zathura "' . expand('%:p:r') . '.pdf" &'
endfunction

augroup texbindings
  autocmd!
  autocmd Filetype tex setlocal makeprg=rubber\ --pdf\ %
  autocmd Filetype tex setlocal efm=%f:%l:\ %m,%f:%l-%\\d%\\+:\ %m
  autocmd Filetype tex nmap <buffer> <silent> <leader>mp :call LaunchZathura()<CR>
  autocmd Filetype tex nmap <buffer> <F7> :Make<CR>
augroup end
" }}}

" Asciidoc {{{
function! OpenAsciidoc()
  execute 'silent !xdg-open "' . expand('%:p:r') . '.pdf" &'
endfunction

augroup adocbindings
  autocmd!
  autocmd Filetype asciidoc setlocal makeprg=asciidoctor-pdf\ %
  autocmd Filetype asciidoc nmap <buffer> <F7> :Make<CR>
  autocmd Filetype asciidoc nmap <buffer> <silent> <leader>mp :call OpenAsciidoc()<CR>
augroup end
" }}}

" Fl {{{
augroup flbindings
  autocmd Filetype fl setlocal makeprg=make
  autocmd Filetype fl setlocal errorformat=%f:%l:%c:\ %trror:\ %m
  autocmd Filetype fl nmap <buffer> <F7> :Make<CR>
augroup end
" }}}

" Go {{{
let g:go_highlight_trailing_whitespace_error=0

augroup gobindings
  autocmd!
  autocmd Filetype go setlocal makeprg=go\ build
  autocmd Filetype go nmap <buffer> <F7> :Make<CR>
  autocmd Filetype go nmap <buffer> <silent> <leader>mf :ALEFix<CR>
augroup end
" }}}

" Python {{{
augroup pythonbindings
  autocmd!
  autocmd Filetype python nmap <buffer> <leader>mf :YAPF<CR>
augroup end
" }}}

" D {{{
function! SetDMakeprg()
	if !empty(glob("dub.sdl")) || !empty(glob("dub.json"))
		setlocal makeprg=dub\ build
	elseif !empty(glob("makefile")) || !empty(glob("Makefile")) || !empty(glob("GNUmakefile"))
		setlocal makeprg=make
	elseif !empty(glob("Tupfile"))
		setlocal makeprg=tup\ .
	endif
endfunction

augroup dbindings
  autocmd Filetype d call SetDMakeprg()

  " autocmd FileType d setlocal errorformat=%f\(%l\\,%c\):\ %trror:\ %m,%-G%.%#
  " autocmd FileType d setlocal errorformat=%f(%l,%c):\ %trror:\ %m
  autocmd FileType d setlocal efm=%*[^@]@%f\(%l\):\ %m,%f\(%l\\,%c\):\ %m,%f\(%l\):\ %m
  autocmd Filetype d nmap <buffer> <F7> :Make<CR>
augroup end
"}}}

" Zig {{{
let g:zig_fmt_autosave = 0
augroup zigbindings
	autocmd Filetype zig nmap <buffer> <F7> :Make<CR>
augroup end
" }}}

" GLSL {{{
augroup glslbindings
  autocmd Filetype glsl setlocal makeprg=make\ -C\ shaders
  autocmd Filetype glsl nmap <buffer> <F7> :make<CR>
  autocmd BufEnter *.frag let b:fswitchdst = 'vert' | let b:fswitchlocs = '.'
  autocmd BufEnter *.vert let b:fswitchdst = 'frag' | let b:fswitchlocs = '.'
augroup end
" }}}

" HLSL {{{
augroup hlslbindings
augroup end
" }}}
