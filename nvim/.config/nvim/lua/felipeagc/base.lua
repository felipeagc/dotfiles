function create_augroup(filetype, callback)
    if type(filetype) == "table" then
        for _, ft in ipairs(filetype) do
            create_augroup(ft, callback)
        end
    else
        local group = vim.api.nvim_create_augroup(filetype .. "_augroup", {})
        vim.api.nvim_create_autocmd({ "FileType" }, {
            pattern = filetype,
            callback = callback,
            group = group,
        })
    end
end

vim.pack.add({
    "https://github.com/tpope/vim-surround",
    "https://github.com/tpope/vim-endwise",
    "https://github.com/tpope/vim-repeat",
    "https://github.com/tpope/vim-abolish",
    "https://github.com/tpope/vim-unimpaired",
    "https://github.com/tpope/vim-dispatch",
    "https://github.com/tpope/vim-projectionist",
    "https://github.com/editorconfig/editorconfig-vim",
})

-- Vim options {{{
vim.o.termguicolors = true

vim.o.exrc = true
vim.o.showcmd = true
vim.o.mouse = "a"

vim.o.scrolloff = 5
vim.o.clipboard = "unnamedplus"
vim.o.completeopt = "menu,menuone,noselect"

vim.o.tabstop = 4
vim.o.softtabstop = 4
vim.o.shiftwidth = 4
vim.o.autoindent = true
vim.o.smartindent = true
vim.o.smarttab = true
vim.o.expandtab = true

vim.o.wrap = true
vim.o.wildignore = vim.o.wildignore
    .. "*.so,*.swp,*.zip,*.o,*.png,*.jpg,*.jpeg,*/target/*,*/build/*,*/node_modules/*,tags,*.glb,*.gltf,*.hdr"
vim.o.hidden = true
vim.o.showmode = false
vim.o.modeline = true
vim.o.modelines = 1

vim.o.undofile = true
vim.o.undodir = vim.fn.stdpath("data") .. "/undodir"

vim.o.linebreak = true

vim.o.showbreak = ">   "
vim.o.shortmess = vim.o.shortmess .. "c"

vim.o.ignorecase = true
vim.o.smartcase = true

vim.o.cinoptions = vim.o.cinoptions .. "L0"
vim.o.cinoptions = vim.o.cinoptions .. "l1"

vim.o.pumheight = 8        -- Completion menu height
vim.o.signcolumn = "yes:1" -- Configure minimum gutter width

-- Tells vim to write changes back to the same file handle
vim.o.backupcopy = "yes"

vim.wo.number = true
-- vim.wo.cursorline = true
vim.wo.foldmethod = "marker"
-- vim.wo.foldlevel = 0
-- }}}

-- Keybinds {{{
vim.g.mapleader = " "

vim.keymap.set("n", "<C-p>", ":Telescope find_files<CR>", { silent = true })
vim.keymap.set("n", "<C-t>", ":Telescope lsp_dynamic_workspace_symbols<CR>", { silent = true })
vim.keymap.set("n", "<C-b>", ":Telescope buffers<CR>", { silent = true })
vim.keymap.set("n", "<Leader>fg", ":Telescope live_grep<CR>", { silent = true })
vim.keymap.set("n", "<Leader>fr", ":Telescope resume<CR>", { silent = true })

vim.keymap.set("n", "<C-j>", "<C-w>w", { remap = false })
vim.keymap.set("n", "<C-k>", "<C-w>W", { remap = false })

vim.keymap.set("n", "<C-e>", ":CNext<CR>", { silent = true })
vim.keymap.set("n", "<C-q>", ":CPrev<CR>", { silent = true })

-- Continuous indentation shift
vim.keymap.set("v", "<", "<gv", { remap = false })
vim.keymap.set("v", ">", ">gv", { remap = false })
vim.keymap.set("v", "<s-lt>", "<gv", { remap = false })

vim.keymap.set("v", "J", ":m '>+1<CR>gv=gv", { remap = false })
vim.keymap.set("v", "K", ":m '<-2<CR>gv=gv", { remap = false })

vim.keymap.set("n", "n", "nzzzv", { remap = false })
vim.keymap.set("n", "N", "Nzzzv", { remap = false })
vim.keymap.set("n", "<C-d>", "<C-d>zz", { remap = false })
vim.keymap.set("n", "<C-u>", "<C-u>zz", { remap = false })

vim.keymap.set("n", "J", "mzJ`z", { remap = false })

vim.keymap.set("n", "<Leader>fed", ":e " .. vim.fn.stdpath("config") .. "/init.lua<CR>", { silent = true })
vim.keymap.set("n", "<Leader>fer", ":source " .. vim.fn.stdpath("config") .. "/init.lua<CR>:restart<CR>",
    { silent = false })
vim.keymap.set("n", "<Leader>feg", ":e " .. vim.fn.stdpath("config") .. "/ginit.vim<CR>", { silent = true })

vim.keymap.set("n", "<Leader>w/", ":vsp<CR>", { silent = true })
vim.keymap.set("n", "<Leader>w-", ":sp<CR>", { silent = true })
vim.keymap.set("n", "<Leader>wd", ":q<CR>", { silent = true })
vim.keymap.set("n", "<Leader>wb", "<C-w>=", { silent = true })

vim.keymap.set("n", "<Leader>bd", ":bd<CR>", { silent = true })
vim.keymap.set("n", "<Leader>bcc", ":%bd|e#<CR>", { silent = true })

vim.keymap.set("n", "<Leader>tn", ":tabnext<CR>", { silent = true })
vim.keymap.set("n", "<Leader>tp", ":tabprev<CR>", { silent = true })
vim.keymap.set("n", "<Leader>tc", ":tabnew<CR>", { silent = true })

vim.keymap.set("n", "<Leader>gs", ":LazyGitCurrentFile<CR>", { silent = true })

vim.keymap.set("n", "<C-a>", ":A<CR>", { silent = true })

vim.keymap.set("n", "<A-p>", "<nop>", { silent = true })

vim.keymap.set("n", "<f7>", ":Make<CR>", { silent = true })
vim.keymap.set("n", "<Leader>mb", ":Make<CR>", { silent = true })
vim.keymap.set("n", "<A-r>", ":Make<CR>", { silent = true })

vim.keymap.set("n", "<Leader>tt", ":TestSuite<CR>", { silent = true })
vim.keymap.set("n", "<Leader>tf", ":TestFile<CR>", { silent = true })
vim.keymap.set("n", "<Leader>tm", ":TestNearest<CR>", { silent = true })

-- Disable ex mode binding
vim.cmd([[map Q <Nop>]])
-- }}}

-- Small quality of life stuff {{{
-- Remap :W to :w
vim.cmd([[
	cnoreabbrev W w
	cnoreabbrev Q q
	cnoreabbrev Wq wq
	cnoreabbrev WQ wq
]])

-- Clear highlights with escape
vim.cmd([[
	nnoremap <silent> <esc> :noh<return><esc>
]])

-- Create non-existing directories before writing buffer
vim.api.nvim_create_autocmd("BufWritePre", {
    callback = function()
        if vim.tbl_contains({ "oil" }, vim.bo.ft) then
            return
        end
        local dir = vim.fn.expand("<afile>:p:h")
        if vim.fn.isdirectory(dir) == 0 then
            vim.fn.mkdir(dir, "p")
        end
    end,
})

-- Open help vertically
vim.cmd([[
    autocmd FileType help wincmd L
    autocmd FileType man wincmd L
]])

--  Fix false positive C bracket error
vim.cmd([[
    let c_no_bracket_error=1
    let c_no_curly_error=1
]])

-- Quickfix helpers
vim.cmd([[
    command! CNext try | cnext | catch | clast | catch | endtry
    command! CPrev try | cprev | catch | cfirst | catch | endtry
]])
-- }}}

-- Indentation {{{
vim.cmd([[
    autocmd FileType c setlocal shiftwidth=4 tabstop=4 expandtab
    " autocmd FileType cpp setlocal shiftwidth=4 tabstop=4 expandtab
    autocmd FileType dart setlocal shiftwidth=2 tabstop=2 expandtab
    autocmd FileType glsl setlocal shiftwidth=4 tabstop=4 expandtab
    autocmd FileType haskell setlocal shiftwidth=2 tabstop=2 expandtab
    autocmd FileType html setlocal shiftwidth=2 tabstop=2 expandtab
    autocmd FileType htmldjango setlocal shiftwidth=2 tabstop=2 expandtab
    autocmd FileType lua setlocal shiftwidth=4 tabstop=4 expandtab
    autocmd FileType ocaml setlocal shiftwidth=2 tabstop=2 expandtab
    autocmd FileType rust setlocal shiftwidth=4 tabstop=4 expandtab
    autocmd FileType sbt setlocal shiftwidth=2 tabstop=2 expandtab
    autocmd FileType scala setlocal shiftwidth=2 tabstop=2 expandtab
    autocmd FileType svelte setlocal shiftwidth=2 tabstop=2 expandtab
    autocmd FileType javascript setlocal shiftwidth=2 tabstop=2 expandtab
    autocmd FileType javascriptreact setlocal shiftwidth=2 tabstop=2 expandtab
    autocmd FileType typescript setlocal shiftwidth=2 tabstop=2 expandtab
    autocmd FileType typescriptreact setlocal shiftwidth=2 tabstop=2 expandtab
    autocmd FileType zig setlocal shiftwidth=4 tabstop=4 expandtab
    autocmd FileType sql setlocal shiftwidth=4 tabstop=4 expandtab
    autocmd FileType terraform setlocal shiftwidth=2 tabstop=2 expandtab
    autocmd FileType capnp setlocal shiftwidth=2 tabstop=2 expandtab
]])
-- }}}

-- Other filetypes {{{
vim.cmd([[
autocmd BufRead,BufNewFile *.wgsl set filetype=wgsl
autocmd BufRead,BufNewFile *.hlsl set filetype=hlsl
autocmd BufRead,BufNewFile Tiltfile set filetype=starlark
autocmd BufRead,BufNewFile Dockerfile.* set filetype=dockerfile
autocmd BufRead,BufNewFile *.ixx set filetype=cpp
autocmd BufRead,BufNewFile *.mxx set filetype=cpp
autocmd BufRead,BufNewFile *.mpp set filetype=cpp
autocmd BufRead,BufNewFile *.cppm set filetype=cpp
autocmd BufRead,BufNewFile *.slang set filetype=slang
autocmd BufRead,BufNewFile *.tofu set filetype=terraform

autocmd FileType hlsl setlocal commentstring=//\\ %s
autocmd FileType slang setlocal commentstring=//\\ %s
]])
-- }}}
