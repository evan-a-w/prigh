-- Completion in the prompt buffer: @file (fuzzy, project files) and
-- /command (the agent's available_commands_update). `items` is usable from
-- completion plugins; `func` is the buffer's 'completefunc'.
local M = {}

local cache = { cwd = nil, files = nil, at = 0 }

--- Project files relative to `cwd`: git ls-files, else a bounded walk.
function M.files(cwd)
  local now = vim.uv.now()
  if cache.cwd == cwd and cache.files and now - cache.at < 10000 then
    return cache.files
  end
  local files = {}
  local ok, res = pcall(function()
    return vim
      .system({ "git", "ls-files", "--cached", "--others", "--exclude-standard" }, { cwd = cwd, text = true })
      :wait(3000)
  end)
  if ok and res.code == 0 then
    for l in res.stdout:gmatch("[^\n]+") do
      table.insert(files, l)
    end
  else
    local skip = { [".git"] = true, node_modules = true, _build = true, _opam = true, target = true }
    for name, kind in
      vim.fs.dir(cwd, {
        depth = 8,
        skip = function(dir)
          return not skip[vim.fs.basename(dir)]
        end,
      })
    do
      if kind == "file" then
        table.insert(files, name)
        if #files >= 20000 then
          break
        end
      end
    end
  end
  table.sort(files)
  cache = { cwd = cwd, files = files, at = now }
  return files
end

function M.invalidate()
  cache.files = nil
end

local function oneline(s, n)
  s = (s or ""):gsub("\n.*", "")
  if vim.fn.strchars(s) > n then
    s = vim.fn.strcharpart(s, 0, n - 1) .. "…"
  end
  return s
end

--- Complete-items for `kind` ("file" | "command") matching `base` fuzzily.
function M.items(kind, base, ctx)
  local out = {}
  if kind == "file" then
    local files = M.files(ctx.cwd)
    local matches = base == "" and vim.list_slice(files, 1, 500) or vim.fn.matchfuzzy(files, base, { limit = 200 })
    for _, f in ipairs(matches) do
      table.insert(out, { word = f, abbr = f, menu = "[file]" })
    end
  else
    local cmds = ctx.commands or {}
    local by_name = {}
    local names = {}
    for _, c in ipairs(cmds) do
      by_name[c.name] = c
      table.insert(names, c.name)
    end
    local matches = base == "" and names or vim.fn.matchfuzzy(names, base)
    for _, n in ipairs(matches) do
      local c = by_name[n]
      local info = c.description or ""
      if c.input and c.input.hint then
        info = info .. "\n\nargument: " .. c.input.hint
      end
      table.insert(
        out,
        { word = n .. (c.input and " " or ""), abbr = "/" .. n, menu = oneline(c.description, 50), info = info }
      )
    end
  end
  return out
end

--- Where a completion starts on `line` before byte column `col` (0-based):
--- returns kind, start column (after the trigger character), base.
function M.context(line, col, lnum)
  local before = line:sub(1, col)
  local s = before:match("()@[^%s@]*$")
  if s and (s == 1 or before:sub(s - 1, s - 1):match("%s")) then
    return "file", s, before:sub(s + 1)
  end
  if lnum == 1 then
    local cmd = before:match("^/([%w_%-%.:]*)$")
    if cmd then
      return "command", 1, cmd
    end
  end
end

-- Set by the chat: returns { cwd, commands }.
M.ctx_provider = function()
  return { cwd = vim.fn.getcwd(), commands = {} }
end

function M.func(findstart, base)
  if findstart == 1 then
    local line = vim.api.nvim_get_current_line()
    local cur = vim.api.nvim_win_get_cursor(0)
    local kind, start = M.context(line, cur[2], cur[1])
    if not kind then
      return -3
    end
    M.pending_kind = kind
    return start
  end
  return M.items(M.pending_kind or "file", base, M.ctx_provider())
end

return M
