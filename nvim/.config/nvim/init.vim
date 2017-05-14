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
	call dein#add('Shougo/deoplete.nvim', { 'build': ':UpdateRemotePlugins' })
	call dein#add('Shougo/echodoc.vim')
	call dein#add('ervandew/supertab')

	" Linting
	call dein#add('w0rp/ale')

	" Fuzzy finders
	call dein#add('junegunn/fzf', { 'build': './install --all'})
	call dein#add('junegunn/fzf.vim')

	" Git
	call dein#add('tpope/vim-fugitive')
	call dein#add('mhinz/vim-signify')

	" Tags
	call dein#add('ludovicchabant/vim-gutentags')

	" Org mode
	call dein#add('jceb/vim-orgmode')

	" Rust
	call dein#add('rust-lang/rust.vim')
	" call dein#add('racer-rust/vim-racer')
	" call dein#add('rhysd/rust-doc.vim')

	" Crystal
	call dein#add('rhysd/vim-crystal')

	" HTML
	call dein#add('mattn/emmet-vim')

	" Javascript
	call dein#add('carlitux/deoplete-ternjs')

	" Typescript
	call dein#add('mhartington/nvim-typescript')

	" C/C++
	call dein#add('rhysd/vim-clang-format')
	call dein#add('zchee/deoplete-clang')

	" QML
	call dein#add('peterhoeg/vim-qml')

	" Haskell
	call dein#add('neovimhaskell/haskell-vim')
	call dein#add('itchyny/vim-haskell-indent')
	call dein#add('eagletmt/neco-ghc')
	"call dein#add('eagletmt/ghcmod-vim')

	" Go
	call dein#add('zchee/deoplete-go')

	" Utilities
	call dein#add('tpope/vim-surround')
	"call dein#add('Shougo/vimproc.vim', {'build' : 'make'})
	call dein#add('Shougo/neosnippet.vim')
	call dein#add('Shougo/neosnippet-snippets')
	call dein#add('tpope/vim-commentary')
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
	call dein#add('wellle/targets.vim')
	" call dein#add('autozimu/LanguageClient-neovim', { 'build': ':UpdateRemotePlugins' })

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
		let $NVIM_TUI_ENABLE_TRUE_COLOR=1
	endif
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
let g:deoplete#sources#clang#flags = [
	\ "-Isubprojects/glad/include"
	\ ]

let g:gutentags_cache_dir = '~/.local/share/gutentags'

let g:echodoc_enable_at_startup=1

let loaded_matchparen = 1

let g:racer_cmd = '~/.cargo/bin/racer'
let g:racer_experimental_completer = 1

let g:ale_lint_on_save = 1
let g:ale_lint_on_text_changed = 0
let g:ale_statusline_format = ['⨉ %d', '⚠ %d', 'ok']
let g:ale_sign_error = '⨉'
let g:ale_sign_warning = '⚠'
let g:ale_sign_column_always = 1
let g:ale_set_loclist = 0
let g:ale_set_quickfix = 0
let g:ale_open_list = 0
let g:ale_cpp_clang_options = "-std=c++14 -Isubprojects/glad/include"
let g:ale_linters = {
			\ 'cpp': ['clang'],
			\ 'lua': [],
			\ 'haskell': ['hdevtools'],
			\ 'typescript': ['typecheck'],
			\ 'rust': []
			\}

" let g:haskellmode_completion_ghc = 0
" autocmd FileType haskell setlocal omnifunc=necoghc#omnifunc
let g:necoghc_enable_detailed_browse = 1

let g:SuperTabDefaultCompletionType = "<c-n>"
let g:SuperTabCrMapping = 0

let g:go_list_type = ""

let g:LanguageClient_serverCommands = {
    \ 'rust': ['rls']
    \ }
let g:LanguageClient_autoStart = 1

let $FZF_DEFAULT_COMMAND = 'ag -g ""'

set mouse=a
set noshowcmd
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
set wrap
set linebreak
" note trailing space at end of next line
set showbreak=>\ \ \

" Use ctrl-[hjkl] to select the active split!
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

" Language server bindings
" nnoremap <silent> K :call LanguageClient_textDocument_hover()<CR>
" nnoremap <silent> gd :call LanguageClient_textDocument_definition()<CR>
" nnoremap <silent> <F2> :call LanguageClient_textDocument_rename()<CR>

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

nnoremap cn *``cgn

"
" Leader bindings:
"

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

