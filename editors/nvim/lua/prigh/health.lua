local M = {}

--- Whether `bin acp` is prigh's ACP server: an older prigh, or its TUI
--- wrapper from before it passed `acp` on to the backend, has none.
function M.check_acp(bin)
  local h = vim.health
  local ok, res = pcall(function()
    return vim.system({ bin, "acp", "-help" }, { text = true, stdin = false }):wait(5000)
  end)
  local out = ok and ((res.stdout or "") .. (res.stderr or "")) or tostring(res)
  if ok and res.code == 0 and out:find("Agent Client Protocol", 1, true) then
    h.ok(bin .. " acp: prigh's ACP server")
  else
    local first = vim.trim(out):match("[^\n]*") or ""
    h.error(string.format("%s acp is not prigh's ACP server%s", bin, first ~= "" and (": " .. first) or ""), {
      "update prigh (nix profile upgrade, or rebuild): `prigh acp` needs a prigh with the acp command",
      "or point cmd at the backend binary: require('prigh').setup{ cmd = { '/path/to/backend/prigh', 'acp' } }",
    })
  end
end

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
    if cmd[2] == "acp" then
      M.check_acp(cmd[1])
    end
  else
    h.error(cmd[1] .. " not found on PATH", {
      "install prigh (see its README) or set require('prigh').setup{ cmd = { '/path/to/prigh', 'acp' } }",
    })
  end
  local doc = vim.api.nvim_get_runtime_file("doc/prigh.txt", false)[1]
  if not doc then
    h.warn("doc/prigh.txt is not on the runtimepath: the plugin directory was not added", {
      'vim.opt.rtp:append("<the prigh repository>/editors/nvim") (see the README\'s Install)',
    })
  elseif vim.fn.filereadable(vim.fs.joinpath(vim.fs.dirname(doc), "tags")) == 1 then
    h.ok("help: :help prigh")
  else
    h.warn("no help tags: :help prigh fails", { ":helptags " .. vim.fs.dirname(doc) })
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
