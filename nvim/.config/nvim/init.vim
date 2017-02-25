if &compatible
	set nocompatible
endif

" Required:
set runtimepath+=/home/felipe/.local/share/dein/repos/github.com/Shougo/dein.vim

" Required:
if dein#load_state('/home/felipe/.local/share/dein')
	call dein#begin('/home/felipe/.local/share/dein')

	" Let dein manage dein
	" Required:
	call dein#add('/home/felipe/.local/share/dein/repos/github.com/Shougo/dein.vim')

	" Add or remove your plugins here:
	call dein#add('Shougo/neosnippet.vim')
	call dein#add('Shougo/neosnippet-snippets')
	call dein#add('Shougo/deoplete.nvim')
	call dein#add('Shougo/denite.nvim')
	call dein#add('tpope/vim-fugitive')
	call dein#add('tpope/vim-surround')
	call dein#add('equalsraf/neovim-gui-shim')
	call dein#add('rakr/vim-one')
	call dein#add('Raimondi/delimitMate')
	call dein#add('Shougo/vimproc.vim')
	call dein#add('joshdick/onedark.vim')
	call dein#add('itchyny/lightline.vim')
	call dein#add('mhinz/vim-startify')
	call dein#add('rust-lang/rust.vim')
	call dein#add('racer-rust/vim-racer')
	call dein#add('zchee/deoplete-clang')
	call dein#add('hecal3/vim-leader-guide')
	call dein#add('rhysd/rust-doc.vim')
	call dein#add('neomake/neomake')

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

set number
set relativenumber

set clipboard=unnamedplus

set completeopt-=preview

set tabstop=4
set shiftwidth=4

" Use ctrl-[hjkl] to select the active split!
nmap <silent> <c-k> :wincmd k<CR>
nmap <silent> <c-j> :wincmd j<CR>
nmap <silent> <c-h> :wincmd h<CR>
nmap <silent> <c-l> :wincmd l<CR>

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
  \ 'separator': { 'left': '', 'right': '' },
  \ 'subseparator': { 'left': '', 'right': '' }
  \ }

colorscheme onedark

let g:ctrlp_custom_ignore = {
			\ 'dir': 'node_modules'
			\ }

let loaded_matchparen = 1

set cursorline

set wildignore+=*.so,*.swp,*.zip,*.o,*.png,*.jpg
map <F4> :FSHere<CR>

set noswapfile

let g:ale_lint_on_save = 1
let g:ale_lint_on_text_changed = 0

set hidden
let g:racer_cmd = '/home/felipe/.cargo/bin/racer'
let g:racer_experimental_completer = 1

let g:deoplete#enable_at_startup = 1

let g:deoplete#sources#clang#libclang_path = '/usr/lib/libclang.so'
let g:deoplete#sources#clang#clang_header = '/usr/lib/clang'

nnoremap cn *``cgn

let mapleader = " "

let g:all_key_map = {}

let g:lmap = {}

let g:lmap.f = { 'name' : 'File' }
let g:lmap.f.e = { 'name' : 'Edit' }

nmap <silent> <leader>ff :Denite file_rec<CR>
let g:lmap.f.f = ['Denite file_rec', 'Find']
nmap <silent> <leader>fed :e $MYVIMRC<CR>
let g:lmap.f.e.d = ['', 'Config']
nmap <silent> <leader>feg :e ~/.config/nvim/ginit.vim<CR>
let g:lmap.f.e.g = ['', 'Graphical config']
nmap <silent> <leader>fer :so %<CR>
let g:lmap.f.e.r = ['', 'Reload config']


let g:lmap.b = { 'name' : 'Buffer' }

nmap <silent> <leader>bn :bnext<CR>
let g:lmap.b.n = ['', 'Next']
nmap <silent> <leader>bp :bprevious<CR>
let g:lmap.b.p = ['', 'Previous']
nmap <silent> <leader>bb :Denite buffer<CR>
let g:lmap.b.b = ['', 'Find']
nmap <silent> <leader>bd :bdelete<CR>
let g:lmap.b.d = ['', 'Delete']

let g:lmap.w = { 'name' : 'Window' }

nmap <silent> <leader>w/ :vsp<CR>
let g:lmap.w['/'] = ['', 'Split vertically']
nmap <silent> <leader>w- :sp<CR>
let g:lmap.w['-'] = ['', 'Split horizontally']

let g:lmap.e = { 'name' : 'Error' }

nmap <silent> <leader>en :cnext<CR>
let g:lmap.e.n = ['', 'Next']
nmap <silent> <leader>ep :cprevious<CR>
let g:lmap.e.p = ['', 'Previous']

call leaderGuide#register_prefix_descriptions(" ", "g:lmap")

let g:all_key_map['<Leader>'] = g:lmap
let g:all_key_map['<Leader>']['name'] = '<Leader>'
nmap <silent> <Leader> :<c-u>LeaderGuide '<Leader>'<CR>
vmap <silent> <Leader> :<c-u>LeaderGuideVisual '<Leader>'<CR>

call denite#custom#var('file_rec', 'command',
	\ ['ag', '--follow', '--nocolor', '--nogroup', '-g', ''])

let g:neomake_echo_current_error=1
let g:neomake_verbose=0
autocmd BufWritePost *.rs NeomakeProject cargo

