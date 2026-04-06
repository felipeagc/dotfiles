vim.pack.add({
    { src = "https://github.com/nvim-treesitter/nvim-treesitter", version = "main" },
})

local parsers = {
    "bash",
    "c",
    "cpp",
    "css",
    "go",
    "html",
    "java",
    "javascript",
    "typescript",
    "ledger",
    "lua",
    "make",
    "markdown",
    "python",
    "rust",
    "tsx",
    "typescript",
    "yaml",
    "zig",
}

require("nvim-treesitter").setup {}
require("nvim-treesitter").install(parsers)

local parser_to_filetypes = {
    ["typescript"] = {"typescript", "typescriptreact"},
    ["javascript"] = {"javascript", "javascriptreact"},
}

for _, parser in ipairs(parsers) do
    local fts = { parser }
    if parser_to_filetypes[parser] ~= nil then
        fts = parser_to_filetypes[parser]
    end

    for _, ft in ipairs(fts) do
        vim.api.nvim_create_autocmd('FileType', {
          pattern = { ft },
          callback = function() vim.treesitter.start() end,
        })
    end
end

vim.api.nvim_create_autocmd('PackChanged', {
  desc = 'Handle nvim-treesitter updates',
  group = vim.api.nvim_create_augroup('nvim-treesitter-pack-changed-update-handler', { clear = true }),
  callback = function(event)
    if event.data.kind == 'update' then
      vim.notify('nvim-treesitter updated, running TSUpdate...', vim.log.levels.INFO)
      ---@diagnostic disable-next-line: param-type-mismatch
      local ok = pcall(vim.cmd, 'TSUpdate')
      if ok then
        vim.notify('TSUpdate completed successfully!', vim.log.levels.INFO)
      else
        vim.notify('TSUpdate command not available yet, skipping', vim.log.levels.WARN)
      end
    end
  end,
})
