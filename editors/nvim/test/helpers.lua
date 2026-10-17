-- Test helpers: a transcript on stdout, the fake agent, and printers for what
-- the user sees (chat, winbar, buffers with review marks).
local T = {}

T.root = vim.fn.fnamemodify(debug.getinfo(1, "S").source:sub(2), ":p:h:h")
T.test_dir = T.root .. "/test"
T.tmp = vim.fs.normalize(vim.fn.tempname())
vim.fn.mkdir(T.tmp, "p")
T.tmp = vim.uv.fs_realpath(T.tmp)

local function scrub(s)
  s = s:gsub(vim.pesc(T.tmp), "<tmp>")
  return s
end

function T.out(...)
  local parts = {}
  for i = 1, select("#", ...) do
    parts[i] = tostring(select(i, ...))
  end
  io.stdout:write(scrub(table.concat(parts, " ")), "\n")
end

function T.section(name)
  T.out("")
  T.out("== " .. name)
end

--- JSON with sorted keys, for stable transcripts.
function T.json(v)
  local t = type(v)
  if v == nil or v == vim.NIL then
    return "null"
  elseif t == "string" then
    return vim.json.encode(v)
  elseif t == "number" or t == "boolean" then
    return tostring(v)
  elseif t == "table" then
    if getmetatable(v) == getmetatable(vim.empty_dict()) and next(v) == nil then
      return "{}"
    end
    if vim.islist(v) then
      local parts = {}
      for _, x in ipairs(v) do
        table.insert(parts, T.json(x))
      end
      return "[" .. table.concat(parts, ",") .. "]"
    end
    local keys = vim.tbl_keys(v)
    table.sort(keys)
    local parts = {}
    for _, k in ipairs(keys) do
      table.insert(parts, vim.json.encode(k) .. ":" .. T.json(v[k]))
    end
    return "{" .. table.concat(parts, ",") .. "}"
  end
  return tostring(v)
end

function T.wait(pred, what, ms)
  local ok = vim.wait(ms or 5000, pred, 5)
  if not ok then
    T.out("TIMEOUT waiting for " .. what)
    T.dump_log()
    error("timeout: " .. what)
  end
end

-- Scripts ----------------------------------------------------------------------

T.init_result = {
  protocolVersion = 1,
  agentCapabilities = {
    loadSession = true,
    promptCapabilities = { embeddedContext = true },
    sessionCapabilities = { list = vim.empty_dict() },
  },
  agentInfo = { name = "fake", version = "0" },
  authMethods = {},
}

T.new_result = {
  sessionId = "s1",
  modes = {
    currentModeId = "default",
    availableModes = {
      { id = "default", name = "Default" },
      { id = "plan", name = "Plan", description = "read-only planning" },
    },
  },
  configOptions = {
    {
      id = "model",
      name = "Model",
      category = "model",
      type = "select",
      currentValue = "sonnet",
      options = {
        { value = "sonnet", name = "claude-sonnet" },
        { value = "opus", name = "claude-opus" },
      },
    },
    {
      id = "thinking",
      name = "Thinking",
      category = "thought_level",
      type = "select",
      currentValue = "medium",
      options = {
        { value = "low", name = "low" },
        { value = "medium", name = "medium" },
        { value = "high", name = "high" },
      },
    },
  },
}

--- The usual start: initialize and session/new.
function T.handshake()
  return {
    { recv = "initialize", result = T.init_result },
    { recv = "session/new", result = T.new_result },
  }
end

local log_path
local log_seen = 0

--- Configure prigh against the fake agent running `script`.
function T.start(script, opts)
  local script_path = T.tmp .. "/script.json"
  log_path = T.tmp .. "/agent.log"
  local f = assert(io.open(script_path, "w"))
  f:write(vim.json.encode(script))
  f:close()
  local nvim = vim.v.progpath
  require("prigh").setup(vim.tbl_deep_extend("force", {
    cmd = { nvim, "--headless", "-u", "NONE", "-i", "NONE", "-n", "-l", T.test_dir .. "/fake_agent.lua", script_path, log_path },
    cwd = T.tmp,
    resume_last = false,
  }, opts or {}))
  return require("prigh").get()
end

--- What the agent received since the last call, one message per line.
function T.agent_log()
  local f = io.open(log_path or "", "r")
  if not f then
    return
  end
  local lines = {}
  for l in f:lines() do
    table.insert(lines, l)
  end
  f:close()
  for i = log_seen + 1, #lines do
    local ok, msg = pcall(vim.json.decode, lines[i])
    if ok and type(msg) == "table" then
      msg.jsonrpc = nil
      T.out("agent got: " .. T.json(msg))
    else
      T.out("agent: " .. lines[i])
    end
  end
  log_seen = #lines
end

--- Wait until the agent's log has a line matching `pattern`.
function T.wait_agent(pattern, what)
  T.wait(function()
    local f = io.open(log_path or "", "r")
    if not f then
      return false
    end
    local s = f:read("*a")
    f:close()
    return s:find(pattern) ~= nil
  end, what or pattern)
end

function T.dump_log()
  for _, l in ipairs(require("prigh.log").lines) do
    if not l:find(" send ") and not l:find(" recv ") then
      T.out("log: " .. l:gsub("^%S+ ", ""))
    end
  end
end

--- Stop the agent and wait for it to exit.
function T.finish()
  local agent = require("prigh").get()
  local conn = agent.conn
  agent:stop()
  if conn then
    T.wait(function()
      return conn.exited
    end, "agent exit")
  end
  T.agent_log()
end

-- Screens ----------------------------------------------------------------------

function T.chat()
  local _, chat = require("prigh").get()
  T.out("┌ chat")
  for _, l in ipairs(vim.api.nvim_buf_get_lines(chat.buf, 0, -1, false)) do
    T.out("│ " .. l)
  end
  T.out("└")
end

function T.winbar()
  local _, chat = require("prigh").get()
  if not chat:is_open() then
    return T.out("winbar: (chat closed)")
  end
  local wb = vim.wo[chat.win].winbar
  local s = vim.api.nvim_eval_statusline(wb, { winid = chat.win, use_winbar = true, maxwidth = 400 }).str
  T.out("winbar: " .. s)
  local ib = vim.api.nvim_eval_statusline(vim.wo[chat.input_win].winbar, {
    winid = chat.input_win,
    use_winbar = true,
    maxwidth = 400,
  }).str
  T.out("input bar: " .. ib)
end

--- A buffer with extmarks of `ns`: virtual lines as "  ┆ ...", highlighted
--- lines tagged with their group, eol virtual text and signs shown.
function T.buffer(buf, ns)
  local lines = vim.api.nvim_buf_get_lines(buf, 0, -1, false)
  local marks = vim.api.nvim_buf_get_extmarks(buf, ns, 0, -1, { details = true })
  local by_row = {}
  for _, m in ipairs(marks) do
    by_row[m[2]] = by_row[m[2]] or {}
    table.insert(by_row[m[2]], m[4])
  end
  local function virt(d)
    for _, vl in ipairs(d.virt_lines or {}) do
      local text, hl = {}, nil
      for _, chunk in ipairs(vl) do
        table.insert(text, chunk[1])
        hl = hl or chunk[2]
      end
      T.out(string.format("      ┆ %s  [%s]", table.concat(text), hl or ""))
    end
  end
  for row, l in ipairs(lines) do
    local ds = by_row[row - 1] or {}
    for _, d in ipairs(ds) do
      if d.virt_lines_above then
        virt(d)
      end
    end
    local tags, eol, sign = {}, {}, " "
    for _, d in ipairs(ds) do
      if d.line_hl_group then
        table.insert(tags, d.line_hl_group)
      end
      if d.sign_text then
        sign = vim.trim(d.sign_text)
      end
      for _, c in ipairs(d.virt_text or {}) do
        table.insert(eol, c[1])
      end
    end
    T.out(
      string.format(
        "%3d %s %s%s%s",
        row,
        sign,
        l,
        #tags > 0 and ("  [" .. table.concat(tags, ",") .. "]") or "",
        #eol > 0 and ("  ⟨" .. table.concat(eol, " ") .. "⟩") or ""
      )
    )
    for _, d in ipairs(ds) do
      if d.virt_lines and not d.virt_lines_above then
        virt(d)
      end
    end
  end
end

function T.feed(keys)
  vim.api.nvim_feedkeys(vim.api.nvim_replace_termcodes(keys, true, false, true), "xt", false)
end

-- vim.ui and vim.notify --------------------------------------------------------

local answers = {}

--- Answers for the next vim.ui.select/input calls: for select, the text of
--- the item to pick (as formatted) or nil to cancel; for input, the text.
function T.answer(...)
  for i = 1, select("#", ...) do
    table.insert(answers, { v = (select(i, ...)) })
  end
end

vim.ui.select = function(items, opts, cb)
  T.out("select: " .. (opts.prompt or ""):gsub("\n", "\n        "))
  local fmt = opts.format_item or tostring
  local a = table.remove(answers, 1)
  local picked
  for i, it in ipairs(items) do
    local s = fmt(it)
    T.out("  " .. s)
    if a and a.v and not picked and s:find(a.v, 1, true) then
      picked = i
    end
  end
  if not a then
    T.out("  (no answer scripted: cancelled)")
  end
  T.out("  -> " .. (picked and fmt(items[picked]) or "cancel"))
  cb(picked and items[picked] or nil, picked)
end

vim.ui.input = function(opts, cb)
  local a = table.remove(answers, 1)
  T.out(string.format("input: %s [default %q] -> %s", opts.prompt or "", opts.default or "", a and a.v and ("%q"):format(a.v) or "cancel"))
  cb(a and a.v or nil)
end

vim.notify = function(msg, level)
  T.out(string.format("notify%s: %s", level and level >= vim.log.levels.WARN and " (warn)" or "", msg))
end

return T
