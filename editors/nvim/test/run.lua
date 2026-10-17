-- Headless test runner: nvim --headless -u NONE -l test/run.lua [case...]
-- Each case (test/cases/NAME.lua) runs in its own Neovim and prints a
-- transcript, compared with test/cases/NAME.expected. PROMOTE=1 re-records.
local dir = vim.fn.fnamemodify(debug.getinfo(1, "S").source:sub(2), ":p:h")
local cases = {}
if #arg > 0 then
  cases = { unpack(arg) }
else
  for name, kind in vim.fs.dir(dir .. "/cases") do
    if kind == "file" and name:match("%.lua$") then
      table.insert(cases, (name:gsub("%.lua$", "")))
    end
  end
  table.sort(cases)
end
local promote = os.getenv("PROMOTE") == "1"
local failed = {}
local jobs = {}
for _, name in ipairs(cases) do
  local home = vim.fn.tempname()
  vim.fn.mkdir(home, "p")
  jobs[name] = vim.system({ vim.v.progpath, "--headless", "-u", "NONE", "-i", "NONE", "-n", "-l", dir .. "/run_case.lua", name }, {
    text = true,
    cwd = home,
    env = { XDG_DATA_HOME = home .. "/data", XDG_STATE_HOME = home .. "/state", XDG_CONFIG_HOME = home .. "/config", XDG_CACHE_HOME = home .. "/cache" },
  })
end
for _, name in ipairs(cases) do
  local res = jobs[name]:wait(60000)
  local got = (res.stdout or "") .. (res.stderr ~= "" and ("STDERR: " .. res.stderr) or "")
  local path = dir .. "/cases/" .. name .. ".expected"
  local f = io.open(path, "r")
  local want = f and f:read("*a") or ""
  if f then
    f:close()
  end
  if got == want and res.code == 0 then
    print("ok   " .. name)
  else
    table.insert(failed, name)
    print("FAIL " .. name .. (res.code ~= 0 and (" (exit " .. res.code .. ")") or ""))
    if got ~= want then
      io.stdout:write(vim.text.diff(want, got, { ctxlen = 2 }) or "")
    end
    if promote then
      local o = assert(io.open(path, "w"))
      o:write(got)
      o:close()
      print("     promoted " .. path)
    end
  end
end
if #failed > 0 then
  print(string.format("%d/%d failed: %s", #failed, #cases, table.concat(failed, " ")))
  os.exit(1)
end
print(string.format("all %d passed", #cases))
