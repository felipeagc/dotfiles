create_augroup("odin", function()
    if vim.fn.filereadable("build.sh") == 1 and vim.fn.has("unix") then
        vim.api.nvim_buf_set_option(0, "makeprg", "sh ./build.sh")
    elseif vim.fn.filereadable("build.bat") == 1 and not vim.fn.has("unix") then
        vim.api.nvim_buf_set_option(0, "makeprg", "./build.bat")
    else
        vim.api.nvim_buf_set_option(0, "makeprg", "odin build .")
    end
end)
