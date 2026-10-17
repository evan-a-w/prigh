-- The ACP client: one agent process, one current session. UI-agnostic: the
-- view reads `agent.session` (prigh.session) and gets callbacks through `ui`:
--   ui.permission(entry)          a permission card was added and awaits an answer
--   ui.elicit(params, respond)    elicitation/create (form); respond(result)
--   ui.unqueue(prompts)           queued prompts given back after a cancel
--   ui.focus_input()              (optional)
local acp = require("prigh.acp")
local config = require("prigh.config")
local fs = require("prigh.fs")
local log = require("prigh.log")
local session_mod = require("prigh.session")
local render = require("prigh.render")

local M = {}

M.protocol_version = 1

local A = {}
A.__index = A

function M.new(ui)
  local cwd = config.options.cwd or vim.fn.getcwd()
  local self = setmetatable({
    ui = ui or {},
    cwd = vim.fs.normalize(vim.fn.fnamemodify(cwd, ":p")),
    conn_state = "stopped",
    caps = {},
    waiting = {}, -- callbacks waiting for a ready session
    permissions = {}, -- pending permission entries, oldest first
  }, A)
  self.session = session_mod.new(self.cwd)
  return self
end

-- Last session per directory -------------------------------------------------

local function state_file()
  return vim.fn.stdpath("data") .. "/prigh/last_sessions.json"
end

local function read_last()
  local f = io.open(state_file(), "r")
  if not f then
    return {}
  end
  local ok, t = pcall(vim.json.decode, f:read("*a"))
  f:close()
  return ok and type(t) == "table" and t or {}
end

function A:_remember(id)
  local t = read_last()
  t[self.cwd] = id
  vim.fn.mkdir(vim.fs.dirname(state_file()), "p")
  local f = io.open(state_file(), "w")
  if f then
    f:write(vim.json.encode(t))
    f:close()
  end
end

function A:last_session_id()
  return read_last()[self.cwd]
end

-- Connection ---------------------------------------------------------------

function A:_set_conn_state(st)
  self.conn_state = st
  self.session:emit("status")
end

function A:_notice(severity, title, description)
  return self.session:notice(severity, title, description)
end

function A:client_capabilities()
  return {
    fs = { readTextFile = true, writeTextFile = true },
    terminal = false,
    elicitation = { form = vim.empty_dict() },
    session = {
      notices = vim.empty_dict(),
      configOptions = { boolean = vim.empty_dict() },
    },
    _meta = { prigh = { state = true } },
  }
end

--- Start the agent and a session; `cb()` runs once a session is ready.
function A:ensure(cb)
  if self.conn_state == "ready" and self.session.id then
    if cb then
      cb()
    end
    return
  end
  if cb then
    table.insert(self.waiting, cb)
  end
  if self.conn_state == "starting" then
    return
  end
  self:_start(function()
    local last = config.options.resume_last and self.caps.loadSession and self:last_session_id()
    if last then
      self:load_session(last, function(err)
        if err then
          self:new_session()
        end
      end)
    else
      self:new_session()
    end
  end)
end

function A:_flush_waiting()
  local w = self.waiting
  self.waiting = {}
  for _, cb in ipairs(w) do
    cb()
  end
end

function A:_start(on_ready)
  self:_set_conn_state("starting")
  local conn, err = acp.start({
    cmd = config.options.cmd,
    cwd = self.cwd,
    env = config.options.env,
    on_request = function(method, params, respond)
      self:_on_request(method, params, respond)
    end,
    on_notification = function(method, params)
      self:_on_notification(method, params)
    end,
    on_exit = function(code, signal, stopping)
      if self.conn == conn then
        self:_on_exit(code, signal, stopping)
      end
    end,
  })
  if not conn then
    self:_set_conn_state("error")
    self:_notice("error", err)
    log.error(err)
    self.waiting = {}
    return
  end
  self.conn = conn
  conn:request("initialize", {
    protocolVersion = M.protocol_version,
    clientCapabilities = self:client_capabilities(),
    clientInfo = { name = "prigh.nvim", title = "prigh.nvim", version = "0.1.0" },
  }, function(e, result)
    if e then
      if e.code ~= acp.errors.disconnected then
        self:_set_conn_state("error")
        self:_notice("error", "initialize failed: " .. tostring(e.message), ":PrighLog shows the agent's output")
      end
      return
    end
    if result.protocolVersion ~= M.protocol_version then
      self:_notice(
        "error",
        string.format("the agent speaks ACP version %s; prigh.nvim speaks %d", tostring(result.protocolVersion), M.protocol_version),
        "update prigh and prigh.nvim to matching versions"
      )
      self:stop()
      return
    end
    self.caps = result.agentCapabilities or {}
    self.agent_info = result.agentInfo
    self.auth_methods = result.authMethods or {}
    self:_set_conn_state("ready")
    on_ready()
  end)
end

function A:_on_exit(code, signal, stopping)
  self.conn = nil
  local was_running = self.session.running
  self.session.running = false
  self:_cancel_permissions()
  self.waiting = {}
  if stopping then
    self:_set_conn_state("stopped")
    return
  end
  self:_set_conn_state("exited")
  local tail = log.last_stderr(5)
  local desc = (#tail > 0 and (table.concat(tail, "\n") .. "\n") or "")
    .. ":PrighRestart restarts it"
    .. (self.session.id and " and reloads this session" or "")
    .. " · :PrighLog shows the full log"
  self:_notice(
    "error",
    string.format(
      "%s exited unexpectedly (%s)%s",
      config.options.cmd[1],
      signal and signal ~= 0 and ("signal " .. signal) or ("code " .. tostring(code)),
      was_running and " during a turn" or ""
    ),
    desc
  )
end

function A:stop()
  if self.conn then
    local conn = self.conn
    self.conn = nil
    conn:stop()
  end
  self.session.running = false
  self:_set_conn_state("stopped")
end

--- Restart the agent; reload the current session if the agent can.
function A:restart()
  local id = self.session.id
  self:stop()
  self:_start(function()
    if id and self.caps.loadSession then
      self:load_session(id)
    else
      self:new_session()
    end
  end)
end

-- Sessions -----------------------------------------------------------------

function A:_session_result(result)
  local s = self.session
  if result.modes then
    s.modes = result.modes
  end
  if result.configOptions then
    s.config_options = result.configOptions
  end
  s:emit("status")
end

function A:_reset_session(id)
  self:_cancel_permissions()
  fs.reset()
  self.session:reset(id)
end

function A:new_session(cb)
  if self.conn_state ~= "ready" then
    return self:ensure(cb)
  end
  self.conn:request("session/new", { cwd = self.cwd, mcpServers = {} }, function(err, result)
    if err then
      local hint = err.code == -32000 and "the agent needs authentication: log in with prigh in a terminal, then :PrighRestart"
        or ":PrighLog shows the agent's output"
      self:_notice("error", "session/new failed: " .. tostring(err.message), hint)
      return
    end
    self:_reset_session(result.sessionId)
    self:_session_result(result)
    self:_remember(result.sessionId)
    self:_flush_waiting()
    if cb then
      cb()
    end
  end)
end

function A:load_session(id, cb)
  if not self.caps.loadSession then
    self:_notice("warning", "this agent cannot load sessions", "it did not advertise loadSession")
    return cb and cb({ message = "unsupported" })
  end
  self:_reset_session(id)
  self.session.loading = true
  self.conn:request("session/load", { sessionId = id, cwd = self.cwd, mcpServers = {} }, function(err, result)
    self.session.loading = false
    self.session:close_streams()
    if err then
      self.session.id = nil
      self:_notice("error", string.format("could not load session %s: %s", id, tostring(err.message)), "starting a new session instead")
      return cb and cb(err)
    end
    self:_session_result(result or {})
    self:_remember(id)
    self:_flush_waiting()
    if cb then
      cb()
    end
  end)
end

function A:can_list()
  return self.caps.sessionCapabilities and self.caps.sessionCapabilities.list ~= nil
end

--- cb(err, sessions) with all pages of session/list for this directory.
function A:list_sessions(cb)
  if not self:can_list() then
    return cb({ message = "this agent cannot list sessions (no sessionCapabilities.list)" })
  end
  local all = {}
  local function page(cursor)
    self.conn:request("session/list", { cwd = self.cwd, cursor = cursor }, function(err, result)
      if err then
        return cb(err)
      end
      vim.list_extend(all, result.sessions or {})
      if result.nextCursor and #all < 1000 then
        page(result.nextCursor)
      else
        cb(nil, all)
      end
    end)
  end
  page(nil)
end

-- Prompts ------------------------------------------------------------------

--- Send a prompt (ContentBlock[]); `display` is what the chat shows for it.
--- Queued while a turn is running.
function A:prompt(blocks, display)
  self:ensure(function()
    if self.session.running then
      table.insert(self.session.queue, { blocks = blocks, display = display })
      self.session:emit("status")
      return
    end
    self:_send_prompt(blocks, display)
  end)
end

function A:_send_prompt(blocks, display)
  local s = self.session
  s:close_streams()
  s:add({ kind = "user", text = display, closed = true, live = true })
  s:set_running(true)
  self.turn_start = vim.deepcopy(fs.changes)
  local sid = s.id
  self.conn:request("session/prompt", { sessionId = sid, prompt = blocks }, function(err, result)
    if s.id ~= sid then
      return
    end
    s:close_streams()
    s.running = false
    if err then
      if err.code ~= acp.errors.disconnected then
        self:_notice("error", "prompt failed: " .. tostring(err.message), ":PrighLog shows the agent's output")
      end
    else
      local reason = result and result.stopReason
      if reason and reason ~= "end_turn" then
        local why = {
          cancelled = "cancelled",
          max_tokens = "stopped: reached the token limit",
          max_turn_requests = "stopped: too many requests in one turn",
          refusal = "the agent refused to continue",
        }
        self:_notice(reason == "cancelled" and "info" or "warning", why[reason] or ("stopped: " .. reason))
      end
    end
    self:_turn_changes()
    s:emit("status")
    if #s.queue > 0 and self.conn_state == "ready" then
      local nxt = table.remove(s.queue, 1)
      self:_send_prompt(nxt.blocks, nxt.display)
    end
  end)
end

function A:_turn_changes()
  local changed = {}
  for _, c in ipairs(fs.changed_files()) do
    if not (self.turn_start and self.turn_start[c.path]) then
      table.insert(changed, vim.fn.fnamemodify(c.path, ":~:."))
    end
  end
  if #changed > 0 then
    local key = config.options.keymaps and (config.options.keymaps.prefix .. config.options.keymaps.review) or nil
    self:_notice(
      "info",
      string.format("%d file%s changed: %s", #changed, #changed == 1 and "" or "s", table.concat(changed, ", ")),
      ":PrighReview" .. (key and (" (" .. key .. ")") or "") .. " reviews the changes hunk by hunk"
    )
  end
end

function A:cancel()
  local s = self.session
  local queued = s.queue
  s.queue = {}
  self:_cancel_permissions()
  if s.running and self.conn then
    self.conn:notify("session/cancel", { sessionId = s.id })
  end
  s:emit("status")
  if #queued > 0 and self.ui.unqueue then
    self.ui.unqueue(queued)
  end
end

-- Config -------------------------------------------------------------------

function A:set_config_option(id, value, cb)
  local s = self.session
  local params = { sessionId = s.id, configId = id, value = value }
  if type(value) == "boolean" then
    params.type = "boolean"
  end
  self.conn:request("session/set_config_option", params, function(err, result)
    if err then
      self:_notice("error", string.format("could not set %s: %s", id, tostring(err.message)))
    elseif result and result.configOptions then
      s.config_options = result.configOptions
      s:emit("status")
    end
    if cb then
      cb(err)
    end
  end)
end

function A:set_mode(mode_id, cb)
  local s = self.session
  local opt = s:config_option("mode")
  if opt then
    return self:set_config_option(opt.id, mode_id, cb)
  end
  self.conn:request("session/set_mode", { sessionId = s.id, modeId = mode_id }, function(err)
    if err then
      self:_notice("error", string.format("could not switch to mode %s: %s", mode_id, tostring(err.message)))
    elseif s.modes then
      s.modes.currentModeId = mode_id
      s:emit("status")
    end
    if cb then
      cb(err)
    end
  end)
end

-- Incoming -------------------------------------------------------------------

function A:_on_notification(method, params)
  if method == "session/update" then
    local s = self.session
    if params.sessionId ~= s.id or not params.update then
      return
    end
    local u = params.update
    -- Live turns already show the prompt we sent.
    if u.sessionUpdate == "user_message_chunk" and s.running and not s.loading then
      return
    end
    s:apply(u)
  elseif method == "_prigh/state" then
    if params.sessionId and params.sessionId ~= self.session.id then
      return
    end
    self.session:set_prigh_state(params.state or params)
  elseif method == "elicitation/complete" then
    return
  else
    log.info("ignored notification " .. method)
  end
end

function A:_on_request(method, params, respond)
  if method == "fs/read_text_file" then
    respond(fs.read(params))
  elseif method == "fs/write_text_file" then
    respond(fs.write(params))
  elseif method == "session/request_permission" then
    self:_permission(params, respond)
  elseif method == "elicitation/create" then
    if params.mode ~= "form" then
      return respond(nil, { code = acp.errors.invalid_params, message = "prigh.nvim only supports form elicitation" })
    end
    if not self.ui.elicit then
      return respond({ action = "cancel" })
    end
    self.ui.elicit(params, respond)
  else
    respond(nil, { code = acp.errors.method_not_found, message = "prigh.nvim does not implement " .. method })
  end
end

-- Permissions ----------------------------------------------------------------

function A:_permission(params, respond)
  local s = self.session
  local tc = params.toolCall or {}
  local existing = tc.toolCallId and s.tools[tc.toolCallId]
  local diff_lines = {}
  for _, c in ipairs(tc.content or (existing and existing.content) or {}) do
    if c.type == "diff" then
      vim.list_extend(diff_lines, render.unified_diff_lines(c, self.cwd))
    end
  end
  local entry = s:add({
    kind = "permission",
    status = "pending",
    title = tc.title or (existing and existing.title) or "tool call",
    locations = tc.locations or (existing and existing.locations),
    options = params.options or {},
    diff_lines = diff_lines,
    respond = respond,
  })
  table.insert(self.permissions, entry)
  self.pending_permission = self.permissions[1]
  s:emit("status")
  if self.ui.permission and self.pending_permission == entry then
    self.ui.permission(entry)
  end
end

--- Answer a permission card: an optionId, or nil to cancel.
function A:answer_permission(entry, option_id)
  if entry.status ~= "pending" then
    return
  end
  if option_id then
    entry.status = "selected"
    entry.selected = option_id
    for _, o in ipairs(entry.options) do
      if o.optionId == option_id then
        entry.selected_name = o.name
      end
    end
    entry.respond({ outcome = { outcome = "selected", optionId = option_id } })
  else
    entry.status = "cancelled"
    entry.respond({ outcome = { outcome = "cancelled" } })
  end
  for i, e in ipairs(self.permissions) do
    if e == entry then
      table.remove(self.permissions, i)
      break
    end
  end
  self.pending_permission = self.permissions[1]
  self.session:changed(entry)
  self.session:emit("status")
  if self.pending_permission and self.ui.permission then
    self.ui.permission(self.pending_permission)
  end
end

function A:_cancel_permissions()
  local ps = self.permissions
  self.permissions = {}
  self.pending_permission = nil
  for _, e in ipairs(ps) do
    if e.status == "pending" then
      e.status = "cancelled"
      pcall(e.respond, { outcome = { outcome = "cancelled" } })
      if e.index and self.session.entries[e.index] == e then
        self.session:changed(e)
      end
    end
  end
end

return M
