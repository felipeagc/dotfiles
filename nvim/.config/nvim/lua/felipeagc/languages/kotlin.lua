local kotlin_projectionist_config = {
    ["build.gradle.kts"] = {
        ["src/main/kotlin/*.kt"] = {
            type = "source",
            alternate = "src/test/kotlin/{}Test.kt",
        },
        ["src/test/kotlin/*Test.kt"] = {
            type = "test",
            alternate = "src/main/kotlin/{}.kt",
        },
    },
}

local new_projectionist_heuristics
if vim.g.projectionist_heuristics then
    new_projectionist_heuristics = vim.tbl_extend("force", vim.g.projectionist_heuristics, kotlin_projectionist_config)
else
    new_projectionist_heuristics = kotlin_projectionist_config
end

vim.g.projectionist_heuristics = new_projectionist_heuristics
