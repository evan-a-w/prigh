-- Runs one test case: nvim --headless -u NONE -l run_case.lua CASE
local dir = vim.fn.fnamemodify(debug.getinfo(1, "S").source:sub(2), ":p:h")
vim.opt.rtp:prepend(vim.fs.dirname(dir))
package.path = dir .. "/?.lua;" .. package.path
vim.o.columns, vim.o.lines = 120, 40
vim.o.swapfile = false
local name = arg[1]
local ok, err = xpcall(function()
  dofile(dir .. "/cases/" .. name .. ".lua")
end, debug.traceback)
if not ok then
  io.stdout:write("ERROR: " .. tostring(err) .. "\n")
end
pcall(function()
  require("prigh").reset()
end)
io.stdout:flush()
os.exit(ok and 0 or 1)
