local M = {}

function M.check()
  local h = vim.health
  h.start("prigh.nvim")
  if vim.fn.has("nvim-0.10") == 1 and (vim.text and vim.text.diff or vim.diff) then
    h.ok("Neovim " .. tostring(vim.version()))
  else
    h.error("Neovim 0.10 or newer is required", { "upgrade Neovim" })
  end
  local cmd = require("prigh.config").options.cmd
  if type(cmd) ~= "table" or #cmd == 0 then
    h.error("`cmd` must be a non-empty list", { "require('prigh').setup{ cmd = { 'prigh', 'acp' } }" })
  elseif vim.fn.executable(cmd[1]) == 1 then
    h.ok(string.format("agent: %s (%s)", table.concat(cmd, " "), vim.fn.exepath(cmd[1])))
  else
    h.error(cmd[1] .. " not found on PATH", {
      "install prigh (see its README) or set require('prigh').setup{ cmd = { '/path/to/prigh', 'acp' } }",
    })
  end
  if pcall(vim.treesitter.language.inspect, "markdown") then
    h.ok("markdown treesitter parser: the chat is highlighted")
  else
    h.warn("no markdown treesitter parser: the chat is plain text", { ":TSInstall markdown markdown_inline" })
  end
  if vim.fn.executable("git") == 1 then
    h.ok("git: @ completion lists the project's files")
  else
    h.info("git not found: @ completion walks the directory instead")
  end
  local agent = package.loaded["prigh"] and require("prigh").statusline() or ""
  if agent ~= "" then
    h.info("state: " .. agent)
  end
end

return M
