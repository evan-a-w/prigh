-- Test helpers: a transcript on stdout, the fake agent, and printers for what
-- the user sees (chat, winbar, buffers with review marks).
local T = {}

T.root = vim.fn.fnamemodify(debug.getinfo(1, "S").source:sub(2), ":p:h:h")
T.test_dir = T.root .. "/test"
T.tmp = vim.fs.normalize(vim.fn.tempname())
vim.fn.mkdir(T.tmp, "p")
T.tmp = vim.uv.fs_realpath(T.tmp)
vim.cmd.cd(T.tmp)

local function scrub(s)
  s = s:gsub(vim.pesc(T.tmp), "<tmp>")
  return T.mask(s)
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
    cmd = {
      nvim,
      "--headless",
      "-u",
      "NONE",
      "-i",
      "NONE",
      "-n",
      "-l",
      T.test_dir .. "/fake_agent.lua",
      script_path,
      log_path,
    },
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
  T.out(
    string.format(
      "input: %s [default %q] -> %s",
      opts.prompt or "",
      opts.default or "",
      a and a.v and ("%q"):format(a.v) or "cancel"
    )
  )
  cb(a and a.v or nil)
end

vim.notify = function(msg, level)
  T.out(string.format("notify%s: %s", level and level >= vim.log.levels.WARN and " (warn)" or "", msg))
end

-- The real prigh ---------------------------------------------------------------

--- The prigh binary: $PRIGH_BIN, else the backend's build.
function T.prigh_bin()
  local bin = os.getenv("PRIGH_BIN")
  if not bin or bin == "" then
    bin = vim.fs.normalize(T.root .. "/../../backend/_build/default/bin/main.exe")
  end
  return bin
end

-- What went over the wire, for traffic(): { dir = "send"|"recv", msg }.
T.wire = {}
local wire_seen = 0

local function tap()
  local acp = require("prigh.acp")
  if acp._tapped then
    return
  end
  acp._tapped = true
  local start = acp.start
  acp.start = function(opts)
    local conn, err = start(opts)
    if conn then
      local send, on_line = conn._send, conn._on_line
      conn._send = function(self, msg)
        table.insert(T.wire, { dir = "send", msg = vim.deepcopy(msg) })
        return send(self, msg)
      end
      conn._on_line = function(self, line)
        local ok, msg = pcall(vim.json.decode, line, { luanil = { object = true, array = true } })
        table.insert(T.wire, { dir = "recv", msg = ok and msg or line })
        return on_line(self, line)
      end
    end
    return conn, err
  end
end

T.ids = {}

local session_count = 0

--- Masks what changes from run to run: session ids (prigh's are 16 hex
--- digits, named in order of appearance) and, against the real prigh, times.
function T.mask(s)
  s = s:gsub("%f[%x]" .. ("%x"):rep(16) .. "%f[^%x]", function(id)
    if not T.ids[id] then
      session_count = session_count + 1
      T.ids[id] = string.format("<session %d>", session_count)
    end
    return T.ids[id]
  end)
  if T.real then
    s = s:gsub("%d%d%d%d%-%d%d%-%d%dT[%d:%.]+Z?", "<time>")
    s = s:gsub("%d%d%d%d%-%d%d%-%d%d %d%d:%d%d", "<time>")
  end
  return s
end

--- Messages exchanged with the agent since the last call, filtered by
--- `pred(dir, msg)` (default: all but session/update chunks and a running
--- tool's output so far, which arrives in as many pieces as the process's
--- output was read in).
function T.traffic(pred)
  pred = pred
    or function(_, msg)
      local u = type(msg) == "table" and msg.params and msg.params.update
      if not u then
        return true
      end
      local streamed = u.sessionUpdate == "tool_call_update" and u.content and not u.status
      return not ((u.sessionUpdate or ""):match("_chunk$") or streamed)
    end
  for i = wire_seen + 1, #T.wire do
    local w = T.wire[i]
    if pred(w.dir, w.msg) then
      local msg = vim.deepcopy(w.msg)
      if type(msg) == "table" then
        msg.jsonrpc = nil
        if type(msg.result) == "table" and msg.result.agentInfo then
          msg.result.agentInfo.version = "<version>"
        end
      end
      T.out((w.dir == "send" and "→ " or "← ") .. T.json(msg))
    end
  end
  wire_seen = #T.wire
end

--- Skip what was exchanged so far.
function T.traffic_skip()
  wire_seen = #T.wire
end

--- Configure prigh.nvim against the real `prigh acp -faux-script`: the
--- model replies `replies` (Faux_provider's script: text, thinking,
--- tool_calls...), HOME a fresh directory with `opts.prigh_config` as
--- ~/.prigh/config.json and `opts.auth` as the credentials (fake keys: the
--- faux provider answers everything). No network.
function T.start_prigh(replies, opts)
  opts = opts or {}
  local bin = T.prigh_bin()
  if vim.fn.executable(bin) ~= 1 then
    T.out("no prigh binary at " .. bin .. ": build the backend (cd backend && dune build) or set PRIGH_BIN")
    error("no prigh binary")
  end
  tap()
  T.real = true
  local home = T.tmp .. "/home"
  vim.fn.mkdir(home .. "/.prigh", "p")
  if opts.prigh_config then
    local f = assert(io.open(home .. "/.prigh/config.json", "w"))
    f:write(vim.json.encode(opts.prigh_config))
    f:close()
  end
  if opts.auth then
    vim.fn.mkdir(home .. "/.config/prigh", "p")
    local af = assert(io.open(home .. "/.config/prigh/auth.json", "w"))
    af:write(vim.json.encode(opts.auth))
    af:close()
  end
  local script_path = T.tmp .. "/.faux.json"
  local f = assert(io.open(script_path, "w"))
  f:write(vim.json.encode(replies))
  f:close()
  local setup = vim.tbl_deep_extend("force", {
    cmd = { bin, "acp", "-faux-script", script_path, "-name", "testhost" },
    env = { HOME = home, XDG_CONFIG_HOME = home .. "/.config", TZ = "UTC" },
    cwd = T.tmp,
    resume_last = false,
  }, opts.setup or {})
  require("prigh").setup(setup)
  return require("prigh").get()
end

--- Open the chat on a new session of the real prigh and wait for it.
function T.open_prigh()
  local agent, chat = require("prigh").get()
  vim.cmd("Prigh")
  T.wait(function()
    return agent.session.id ~= nil and agent.session.prigh ~= nil and #agent.session.commands > 0
  end, "a session", 15000)
  T.mask(agent.session.id)
  return agent, chat
end

--- Wait for the running turn to end.
function T.turn_end(what)
  local agent = require("prigh").get()
  T.wait(function()
    return not agent.session.running
  end, what or "turn end", 15000)
end

return T
