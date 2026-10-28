-- Global keymaps under the prefix (and disabled), the chat help, commands
-- and :checkhealth prigh.
local T = require("helpers")
vim.g.mapleader = " "
require("prigh").setup({ cmd = { "true" } })
T.section("global keymaps (leader is space)")
for _, mode in ipairs({ "n", "x" }) do
  local maps = vim.api.nvim_get_keymap(mode)
  table.sort(maps, function(a, b)
    return a.lhs < b.lhs
  end)
  for _, m in ipairs(maps) do
    if m.desc and m.desc:find("^prigh") then
      T.out(string.format("%s %-12s %s", mode, m.lhs, m.desc))
    end
  end
end
T.section("commands")
local cmds = vim.api.nvim_get_commands({})
local names = vim.tbl_keys(cmds)
table.sort(names)
for _, n in ipairs(names) do
  if n:find("^Prigh") then
    T.out(string.format("%-14s %s", n, cmds[n].definition))
  end
end
T.section("keymaps = false")
vim.cmd("mapclear")
require("prigh").setup({ keymaps = false })
local n = 0
for _, m in ipairs(vim.api.nvim_get_keymap("n")) do
  if m.desc and m.desc:find("^prigh") then
    n = n + 1
  end
end
T.out("prigh keymaps: " .. n)
T.section("chat help (g?)")
local _, chat = require("prigh").get()
chat:open()
vim.api.nvim_set_current_win(chat.win)
T.feed("g?")
for _, l in ipairs(vim.api.nvim_buf_get_lines(0, 0, -1, false)) do
  T.out(l)
end
T.feed("q")
T.out("help closed: " .. tostring(vim.api.nvim_get_current_win() == chat.win))
T.section("buffer-local keys")
for _, b in ipairs({ { "chat", chat.buf }, { "input", chat.input } }) do
  for _, mode in ipairs({ "n", "i" }) do
    for _, m in ipairs(vim.api.nvim_buf_get_keymap(b[2], mode)) do
      T.out(string.format("%-5s %s %-7s %s", b[1], mode, m.lhs, m.desc))
    end
  end
end
T.section(":checkhealth prigh")
require("prigh").setup({ cmd = { "prigh-does-not-exist", "acp" } })
local doc_dir = vim.fs.dirname(vim.api.nvim_get_runtime_file("doc/prigh.txt", false)[1])
for _, k in ipairs({ "start", "ok", "warn", "error", "info" }) do
  vim.health[k] = function(msg, advice)
    msg = msg:gsub("Neovim %S+", "Neovim <version>"):gsub("/%S*/(prigh%-[%w%-]+)", "<bin>/%1")
    T.out(string.format("%-5s %s", k, msg))
    for _, a in ipairs(type(advice) == "table" and advice or advice and { advice } or {}) do
      T.out("      advice: " .. a:gsub(vim.pesc(doc_dir), "<plugin>/doc"))
    end
  end
end
-- doc/tags is generated (and ignored by git): pretend each way.
local filereadable = vim.fn.filereadable
local tags_built = false
vim.fn.filereadable = function(path)
  if path == vim.fs.joinpath(doc_dir, "tags") then
    return tags_built and 1 or 0
  end
  return filereadable(path)
end
require("prigh.health").check()
T.section(":checkhealth prigh, help tags built")
tags_built = true
require("prigh.health").check()
T.section(":checkhealth prigh, plugin directory not on the runtimepath")
local get_runtime_file = vim.api.nvim_get_runtime_file
vim.api.nvim_get_runtime_file = function(name, all)
  if name == "doc/prigh.txt" then
    return {}
  end
  return get_runtime_file(name, all)
end
require("prigh.health").check()
vim.api.nvim_get_runtime_file = get_runtime_file
vim.fn.filereadable = filereadable
T.section(":checkhealth prigh, whether cmd has prigh's acp")
local bin_dir = vim.fn.tempname()
vim.fn.mkdir(bin_dir, "p")
local function script(name, body)
  local path = vim.fs.joinpath(bin_dir, name)
  vim.fn.writefile({ "#!/bin/sh", body }, path)
  vim.fn.setfperm(path, "rwxr-xr-x")
  return path
end
local fakes = {
  { "prigh-backend", "echo 'Serve the Agent Client Protocol on stdin/stdout, for editors'" },
  { "prigh-old-wrapper", "echo \"no such directory: $1\" >&2; exit 2" },
  { "prigh-old-backend", "echo 'Error parsing command line: unknown subcommand acp' >&2; exit 1" },
}
for _, fake in ipairs(fakes) do
  local path = script(fake[1], fake[2])
  require("prigh").setup({ cmd = { path, "acp" } })
  local start = vim.health.start
  vim.health.start = function() end
  local ok = vim.health.ok
  vim.health.ok = function(msg, advice)
    if msg:find("^agent: ") then
      return
    end
    ok(msg, advice)
  end
  require("prigh.health").check_acp(path)
  vim.health.start, vim.health.ok = start, ok
end
vim.fn.delete(bin_dir, "rf")
