vim.pack.add({
    { src = "https://github.com/MunifTanjim/nui.nvim" },
    { src = "https://github.com/julienvincent/hunk.nvim" }
})

require("hunk").setup()

local function find_jj_dir(path)
    local uv = vim.loop
    path = path or uv.cwd()

    while path do
        local jj_path = path .. "/.jj"
        local stat = uv.fs_stat(jj_path)
        if stat and stat.type == "directory" then
            return jj_path
        end
        local parent = path:match("(.+)/[^/]+$")
        if parent == path or parent == nil then
            return nil
        end
        path = parent
    end
end

local function open_floating_term(cmd)
    local buf = vim.api.nvim_create_buf(false, true)
    local width = math.floor(vim.o.columns * 0.9)
    local height = math.floor(vim.o.lines * 0.9)
    local row = math.floor((vim.o.lines - height) / 2)
    local col = math.floor((vim.o.columns - width) / 2)

    local win = vim.api.nvim_open_win(buf, true, {
        relative = "editor",
        style = "minimal",
        border = "rounded",
        width = width,
        height = height,
        row = row,
        col = col,
    })

    local term_job_id = vim.fn.termopen(cmd, {
        env = { TERM = "xterm-256color", COLORTERM = "truecolor" },
        on_exit = function()
            if vim.api.nvim_win_is_valid(win) then
                vim.api.nvim_win_close(win, true)
            end
        end,
    })

    vim.cmd("startinsert")
    return term_job_id
end

vim.api.nvim_create_user_command("Jjui", function()
    open_floating_term("jjui")
end, { desc = "Open jjui" })

vim.api.nvim_create_user_command("JjuiOrLazyGit", function()
    local jj_dir = find_jj_dir()
    if jj_dir then
        open_floating_term("jjui")
    else
        vim.cmd("LazyGitCurrentFile")
    end
end, { desc = "Open jjui if inside a jj repo, otherwise LazyGit" })
