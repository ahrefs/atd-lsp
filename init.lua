vim.lsp.set_log_level "trace"

-- Set up an autocmd to start the LSP server when opening .atd files
vim.api.nvim_create_autocmd("BufRead", {
    pattern = {"*.atd"},
    callback = function()
        vim.lsp.start({
            name = "atd-lsp",
            cmd = {"_build/default/bin/main.exe"},
            -- or if installed via opam:
            -- cmd = {"atdlsp"},
            root_dir = vim.loop.cwd()
        })
    end,
})


-- Use the command before
-- :lua vim.lsp.buf.definition()
-- :lua vim.lsp.buf.document_symbol()
