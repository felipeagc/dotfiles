if &compatible
	set nocompatible
endif

" Required:
set runtimepath+=~/.local/share/dein/repos/github.com/Shougo/dein.vim

" Required:
if dein#load_state('~/.local/share/dein')
	call dein#begin('~/.local/share/dein')

	" Let dein manage dein
	" Required:
	call dein#add('~/.local/share/dein/repos/github.com/Shougo/dein.vim')

	" Completion
	call dein#add('Shougo/deoplete.nvim')
	call dein#add('Shougo/echodoc.vim')

	" Linting
	call dein#add('w0rp/ale')

	" Fuzzy finders
	call dein#add('ctrlpvim/ctrlp.vim')
	
	" Git
	call dein#add('tpope/vim-fugitive')

	" Languages
	call dein#add('rust-lang/rust.vim')
	call dein#add('racer-rust/vim-racer')
	call dein#add('rhysd/rust-doc.vim')

	call dein#add('rhysd/vim-crystal')

	call dein#add('mattn/emmet-vim')

	call dein#add('carlitux/deoplete-ternjs')

	call dein#add('mhartington/nvim-typescript')

	call dein#add('rhysd/vim-clang-format')
	call dein#add('zchee/deoplete-clang')

	call dein#add('neovimhaskell/haskell-vim')
	call dein#add('itchyny/vim-haskell-indent')
	call dein#add('eagletmt/neco-ghc')
	"call dein#add('eagletmt/ghcmod-vim')

	" Utilities
	call dein#add('tpope/vim-surround')
	"call dein#add('Shougo/vimproc.vim', {'build' : 'make'})
	call dein#add('Shougo/neosnippet.vim')
	call dein#add('Shougo/neosnippet-snippets')
	call dein#add('scrooloose/nerdcommenter')
	call dein#add('jiangmiao/auto-pairs')
	call dein#add('hecal3/vim-leader-guide')
	call dein#add('derekwyatt/vim-fswitch')

	" Line
	call dein#add('itchyny/lightline.vim')

	" Themes
	call dein#add('dikiaap/minimalist')
	call dein#add('ajh17/Spacegray.vim')
	call dein#add('joshdick/onedark.vim')

	" Other
	call dein#add('equalsraf/neovim-gui-shim')
	call dein#add('metakirby5/codi.vim')
	call dein#add('junegunn/goyo.vim')
	call dein#add('sheerun/vim-polyglot')

	" Required:
	call dein#end()
	call dein#save_state()
endif

" Required:
filetype plugin indent on
syntax enable

" If you want to install not installed plugins on startup.
if dein#check_install()
	call dein#install()
endif

"End dein Scripts-------------------------

if (empty($TMUX))
	if (has("nvim"))
		"For Neovim 0.1.3 and 0.1.4 < https://github.com/neovim/neovim/pull/2198 >
		let $NVIM_TUI_ENABLE_TRUE_COLOR=1
	endif
	"For Neovim > 0.1.5 and Vim > patch 7.4.1799 < https://github.com/vim/vim/commit/61be73bb0f965a895bfb064ea3e55476ac175162 >
	"Based on Vim patch 7.4.1770 (`guicolors` option) < https://github.com/vim/vim/commit/8a633e3427b47286869aa4b96f2bfc1fe65b25cd >
	" < https://github.com/neovim/neovim/wiki/Following-HEAD#20160511 >
	if (has("termguicolors"))
		set termguicolors
	endif
endif

if (has("autocmd") && !has("gui"))
  let s:white = { "gui": "#ABB2BF", "cterm": "145", "cterm16" : "7" }
  autocmd ColorScheme * call onedark#set_highlight("Normal", { "fg": s:white }) " No `bg` setting
end

let g:lightline = {
	\ 'colorscheme': 'onedark',
	\ 'active': {
	\   'left': [ 
	\		[ 'mode', 'paste' ],
	\		[ 'ale' ],
	\		[ 'fugitive', 'filename' ]
	\	]
	\ },
	\ 'component_function': {
	\	'fugitive': 'LightlineFugitive',
	\	'readonly': 'LightlineReadonly',
	\   'modified': 'LightlineModified',
	\   'filename': 'LightlineFilename',
	\   'ale': 'ALEGetStatusLine'
	\ }
	\ }

function! LightlineModified()
  if &filetype == "help"
    return ""
  elseif &modified
    return "+"
  elseif &modifiable
    return ""
  else
    return ""
  endif
endfunction

function! LightlineReadonly()
  if &filetype == "help"
    return ""
  elseif &readonly
    return "⭤"
  else
    return ""
  endif
endfunction

function! LightlineFugitive()
  return exists('*fugitive#head') ? fugitive#head() : ''
endfunction

function! LightlineFilename()
  return ('' != LightlineReadonly() ? LightlineReadonly() . ' ' : '') .
       \ ('' != expand('%:t') ? expand('%:t') : '[No Name]') .
       \ ('' != LightlineModified() ? ' ' . LightlineModified() : '')
endfunction

colorscheme onedark

let loaded_matchparen = 0

let g:deoplete#enable_at_startup = 1
let g:deoplete#enable_ignore_case = 1
let g:deoplete#auto_complete_start_length = 1
let g:deoplete#sources#clang#libclang_path = '/usr/lib/libclang.so'
let g:deoplete#sources#clang#clang_header = '/usr/lib/clang'
let g:auto_complete_start_length = 1

let g:echodoc_enable_at_startup=1

let loaded_matchparen = 1


let g:racer_cmd = '~/.cargo/bin/racer'
let g:racer_experimental_completer = 1

let g:NERDCreateDefaultMappings = 0

let g:ale_lint_on_save = 1
let g:ale_lint_on_text_changed = 0
let g:ale_statusline_format = ['⨉ %d', '⚠ %d', 'ok']
let g:ale_sign_error = '⨉'
let g:ale_sign_warning = '⚠'
let g:ale_sign_column_always = 1
let g:ale_linters = {
			\ 'cpp': ['clang'],
			\ 'lua': [],
			\ 'haskell': ['hdevtools'],
			\ 'typescript': ['typecheck']
			\}
let g:ale_cpp_clang_options = '-std=c++14 -Wall -Ilib/gl3w/include'

let g:haskellmode_completion_ghc = 0
autocmd FileType haskell setlocal omnifunc=necoghc#omnifunc

set number
set clipboard=unnamedplus
set completeopt-=preview
set tabstop=4
set shiftwidth=4
set cursorline
set wildignore+=*.so,*.swp,*.zip,*.o,*.png,*.jpg,*/target/*,*/build/*,*/node_modules/*
set noswapfile
set hidden
"set completeopt+=noselect
set noshowmode
set undofile
set undodir=~/.vim/undodir

" Use ctrl-[hjkl] to select the active split!
nmap <silent> <c-k> :wincmd k<CR>
nmap <silent> <c-j> :wincmd j<CR>
nmap <silent> <c-h> :wincmd h<CR>
nmap <silent> <c-l> :wincmd l<CR>

" Continuous indentation shift
vnoremap < <gv
vnoremap > >gv

" Fix filetype indentations
autocmd Filetype haskell setlocal expandtab|setlocal shiftwidth=2|setlocal softtabstop=2
autocmd Filetype cabal setlocal expandtab|setlocal shiftwidth=2|setlocal softtabstop=2
autocmd Filetype lua setlocal expandtab|setlocal shiftwidth=2|setlocal softtabstop=2
autocmd Filetype javascript setlocal expandtab|setlocal shiftwidth=2|setlocal softtabstop=2
autocmd Filetype typescript setlocal expandtab|setlocal shiftwidth=2|setlocal softtabstop=2

nnoremap cn *``cgn

"
" Leader bindings:
"

let mapleader = " "

let g:all_key_map = {}

let g:lmap = {}

let g:lmap.f = { 'name' : 'File' }
let g:lmap.f.e = { 'name' : 'Edit' }

nmap <silent> <leader>ff :CtrlP<CR>
let g:lmap.f.f = ['', 'Find']
nmap <silent> <leader>fed :e $MYVIMRC<CR>
let g:lmap.f.e.d = ['', 'Config']
nmap <silent> <leader>feg :e ~/.config/nvim/ginit.vim<CR>
let g:lmap.f.e.g = ['', 'Graphical config']
nmap <silent> <leader>fer :so $MYVIMRC<CR>
let g:lmap.f.e.r = ['', 'Reload config']

let g:lmap.b = { 'name' : 'Buffer' }

nmap <silent> <leader>bn :bnext<CR>
let g:lmap.b.n = ['', 'Next']
nmap <silent> <leader>bp :bprevious<CR>
let g:lmap.b.p = ['', 'Previous']
nmap <silent> <leader>bb :CtrlPBuffer<CR>
let g:lmap.b.b = ['', 'Find']
nmap <silent> <leader>bd :bdelete<CR>
let g:lmap.b.d = ['', 'Delete']

let g:lmap.w = { 'name' : 'Window' }

nmap <silent> <leader>w/ :vsp<CR>
let g:lmap.w['/'] = ['', 'Split vertically']
nmap <silent> <leader>w- :sp<CR>
let g:lmap.w['-'] = ['', 'Split horizontally']

let g:lmap.t = { 'name' : 'Tab' }

nmap <silent> <leader>tt :tabnew<CR>
let g:lmap.t.t = ['', 'New tab']
nmap <silent> <leader>tn :tabn<CR>
let g:lmap.t.n = ['', 'Next']
nmap <silent> <leader>tp :tabp<CR>
let g:lmap.t.p = ['', 'Previous']
nmap <silent> <leader>td :tabclose<CR>
let g:lmap.t.d = ['', 'Delete']

let g:lmap.e = { 'name' : 'Error' }

nmap <silent> <leader>en <Plug>(ale_next_wrap)
let g:lmap.e.n = ['', 'Next']
nmap <silent> <leader>ep <Plug>(ale_previous_wrap)
let g:lmap.e.p = ['', 'Previous']

let g:lmap.c = { 'name' : 'Comment' }

nmap <silent> <leader>cl <Plug>NERDCommenterToggle('n', 'Toggle')<CR>
vmap <silent> <leader>cl <Plug>NERDCommenterToggle gv<CR>
let g:lmap.c.l = ['', 'Toggle']

let g:lmap.m = { 'name' : 'Mode' }

let g:lmap.m.s = ['', 'Switch']
autocmd FileType cpp map <buffer> <leader>ms :FSHere<CR>
let g:lmap.m.f = ['', 'Format']
autocmd FileType cpp map <buffer> <leader>mf :ClangFormat<CR>

let g:lmap.m.t = { 'name' : 'Type' }
let g:lmap.m.t.t = ['', 'Show']
autocmd FileType haskell map <buffer> <leader>mtt :GhcModType<CR>
let g:lmap.m.t.c = ['', 'Clear']
autocmd FileType haskell map <buffer> <leader>mtc :GhcModTypeClear<CR>

call leaderGuide#register_prefix_descriptions(" ", "g:lmap")

let g:all_key_map['<Leader>'] = g:lmap
let g:all_key_map['<Leader>']['name'] = '<Leader>'
nmap <silent> <Leader> :<c-u>LeaderGuide '<Leader>'<CR>
vmap <silent> <Leader> :<c-u>LeaderGuideVisual '<Leader>'<CR>

