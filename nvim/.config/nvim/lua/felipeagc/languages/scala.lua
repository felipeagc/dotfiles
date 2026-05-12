vim.lsp.config("metals", {
    cmd = { "metals" },
    filetypes = { "scala", "sbt" },
    root_markers = { "build.mill", "build.mill.yaml", "build.sbt", "build.sc", "build.gradle", "build.gradle.kts", ".scala-build", "pom.xml" },
    init_options = {
        globSyntax = "vscode",
    },
    settings = {
        startMcpServer = false,
    },
})
vim.lsp.enable("metals")
