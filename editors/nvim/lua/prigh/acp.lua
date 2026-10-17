-- JSON-RPC 2.0 over newline-delimited stdio with an ACP agent process.
-- Knows nothing about sessions or UI: see prigh.agent for the ACP client.
local log = require("prigh.log")

local M = {}

M.errors = {
  parse = -32700,
  invalid_request = -32600,
  method_not_found = -32601,
  invalid_params = -32602,
  internal = -32603,
  -- Ours: the request never got an answer because the agent went away.
  disconnected = -32099,
}

local Conn = {}
Conn.__index = Conn

local function truncate(s, n)
  if #s > n then
    return s:sub(1, n) .. string.format("… (%d bytes)", #s)
  end
  return s
end

local function traffic(dir, line)
  local ok, config = pcall(require, "prigh.config")
  if ok and config.options.log.traffic then
    log.add(dir, truncate(line, 400))
  end
end

--- Start the agent.
--- opts: cmd (list), cwd, env, on_request(method, params, respond),
---   on_notification(method, params), on_exit(code, signal)
--- Returns the connection, or nil and an error message saying what to do.
function M.start(opts)
  local cmd = opts.cmd
  if type(cmd) ~= "table" or #cmd == 0 then
    return nil, "prigh: `cmd` must be a non-empty list, e.g. { 'prigh', 'acp' }"
  end
  if vim.fn.executable(cmd[1]) ~= 1 then
    return nil,
      string.format(
        "%s not found: install prigh (see its README) or set require('prigh').setup{ cmd = { '/path/to/prigh', 'acp' } }",
        cmd[1]
      )
  end
  local self = setmetatable({
    next_id = 0,
    pending = {},
    partial = "",
    closed = false,
    opts = opts,
  }, Conn)
  log.info("starting: " .. table.concat(cmd, " "))
  local ok, proc = pcall(vim.system, cmd, {
    cwd = opts.cwd,
    env = opts.env,
    stdin = true,
    text = true,
    stdout = function(_, data)
      self:_on_stdout(data)
    end,
    stderr = function(_, data)
      if data then
        vim.schedule(function()
          log.stderr((data:gsub("\n$", "")))
        end)
      end
    end,
  }, function(res)
    vim.schedule(function()
      self:_on_exit(res.code, res.signal)
    end)
  end)
  if not ok then
    return nil, string.format("could not start %s: %s (set `cmd` in require('prigh').setup{})", cmd[1], proc)
  end
  self.proc = proc
  return self
end

function Conn:_on_stdout(data)
  if not data then
    return
  end
  local buf = self.partial .. data
  local lines = vim.split(buf, "\n", { plain = true })
  self.partial = table.remove(lines)
  for _, line in ipairs(lines) do
    if line ~= "" then
      vim.schedule(function()
        self:_on_line(line)
      end)
    end
  end
end

function Conn:_send(msg)
  if self.closed then
    return false
  end
  msg.jsonrpc = "2.0"
  local line = vim.json.encode(msg)
  traffic("send", line)
  local ok, err = pcall(self.proc.write, self.proc, line .. "\n")
  if not ok then
    log.error("write failed: " .. tostring(err))
  end
  return ok
end

local function decode(line)
  return pcall(vim.json.decode, line, { luanil = { object = true, array = true } })
end

function Conn:_on_line(line)
  traffic("recv", line)
  local ok, msg = decode(line)
  if not ok or type(msg) ~= "table" then
    log.error("agent sent invalid JSON: " .. truncate(line, 200))
    return
  end
  if msg.method then
    if msg.id ~= nil then
      self:_handle_request(msg)
    elseif self.opts.on_notification then
      local ok2, err = pcall(self.opts.on_notification, msg.method, msg.params or {})
      if not ok2 then
        log.error(string.format("handling %s: %s", msg.method, err))
      end
    end
  elseif msg.id ~= nil then
    local cb = self.pending[msg.id]
    self.pending[msg.id] = nil
    if cb then
      local ok2, err = pcall(cb, msg.error, msg.result)
      if not ok2 then
        log.error("response handler: " .. tostring(err))
      end
    end
  end
end

function Conn:_handle_request(msg)
  local answered = false
  local function respond(result, err)
    if answered then
      return
    end
    answered = true
    if err then
      self:_send({ id = msg.id, error = err })
    else
      self:_send({ id = msg.id, result = result == nil and vim.NIL or result })
    end
  end
  if not self.opts.on_request then
    return respond(nil, { code = M.errors.method_not_found, message = "method not found: " .. msg.method })
  end
  local ok, err = pcall(self.opts.on_request, msg.method, msg.params or {}, respond)
  if not ok then
    log.error(string.format("handling %s: %s", msg.method, err))
    respond(nil, { code = M.errors.internal, message = tostring(err) })
  end
end

--- Send a request; `cb(err, result)` runs on the main loop.
function Conn:request(method, params, cb)
  local id = self.next_id
  self.next_id = id + 1
  self.pending[id] = cb or function() end
  if not self:_send({ id = id, method = method, params = params or vim.empty_dict() }) then
    self.pending[id] = nil
    if cb then
      cb({ code = M.errors.disconnected, message = "agent is not running" })
    end
  end
  return id
end

function Conn:notify(method, params)
  return self:_send({ method = method, params = params or vim.empty_dict() })
end

function Conn:is_running()
  return not self.closed
end

function Conn:_on_exit(code, signal)
  if self.closed and self.exited then
    return
  end
  self.closed = true
  self.exited = true
  if self.partial ~= "" then
    self:_on_line(self.partial)
    self.partial = ""
  end
  log.info(string.format("agent exited (code %s, signal %s)", code, signal))
  local pending = self.pending
  self.pending = {}
  for _, cb in pairs(pending) do
    pcall(cb, { code = M.errors.disconnected, message = "agent exited" })
  end
  if self.opts.on_exit then
    self.opts.on_exit(code, signal, self.stopping)
  end
end

--- Close stdin (agents exit on EOF) and kill it if it lingers.
function Conn:stop()
  if self.closed then
    return
  end
  self.stopping = true
  self.closed = true
  pcall(self.proc.write, self.proc, nil)
  local proc = self.proc
  vim.defer_fn(function()
    if not self.exited then
      pcall(proc.kill, proc, 15)
    end
  end, 2000)
end

return M
