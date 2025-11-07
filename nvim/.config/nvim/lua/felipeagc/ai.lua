vim.pack.add({
    { src = "https://github.com/zbirenbaum/copilot.lua" },
})

require("copilot").setup({
    panel = { enabled = false },
    filetypes = {
        typescript = true,
        typescriptreact = true,
        javascript = true,
        javascriptreact = true,
        python = true,
        elixir = true,
        heex = true,
        clojure = true,
        ["*"] = false,
    },
    suggestion = {
        enabled = true,
        auto_trigger = true,
        hide_during_completion = true,
        debounce = 75,
        trigger_on_accept = true,
        keymap = {
            accept = "<C-l>",
            accept_word = false,
            accept_line = false,
            next = false,
            prev = false,
            dismiss = false,
        },
    },
})

local copilot_term_buf = nil

vim.api.nvim_create_user_command("CopilotCli", function()
    -- Check if we already have a valid buffer
    if copilot_term_buf and vim.api.nvim_buf_is_valid(copilot_term_buf) then
        -- See if it's already visible in a window
        local wins = vim.fn.win_findbuf(copilot_term_buf)
        if #wins > 0 then
            -- Jump to the first window showing it
            vim.api.nvim_set_current_win(wins[1])
            return
        end
        -- Not visible, reopen it in a vertical split
        vim.cmd("vsplit | wincmd l")
        vim.api.nvim_set_current_buf(copilot_term_buf)
    else
        -- No valid buffer yet, create a new one
        vim.cmd("vsplit | wincmd l | terminal copilot")
        copilot_term_buf = vim.api.nvim_get_current_buf()
    end
    vim.cmd("startinsert")
end, { desc = "Open or focus Copilot CLI terminal" })
