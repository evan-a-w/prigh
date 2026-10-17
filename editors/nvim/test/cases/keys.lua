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
for _, k in ipairs({ "start", "ok", "warn", "error", "info" }) do
  vim.health[k] = function(msg, advice)
    msg = msg:gsub("Neovim %S+", "Neovim <version>")
    T.out(string.format("%-5s %s", k, msg))
    for _, a in ipairs(type(advice) == "table" and advice or advice and { advice } or {}) do
      T.out("      advice: " .. a)
    end
  end
end
require("prigh.health").check()
