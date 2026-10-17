-- A scripted ACP agent for the tests:
--   nvim --headless -u NONE -l fake_agent.lua SCRIPT.json LOG
-- SCRIPT is a list of steps, run in order:
--   { recv = "method", result = {...} | error = {...} | hold = true }
--       read the next message from the client; it must be `method`. Requests
--       are answered with result/error, or held for a later `reply`.
--   { reply = "method", result = {...} }     answer a held request
--   { notify = "method", params = {...} }    send a notification
--   { update = {...} }                       session/update for session "s1"
--   { request = "method", params = {...} }   send a request, wait for the answer
--   { stderr = "text" }                      write to stderr
--   { exit = code }                          exit now
-- Every message received is appended to LOG; after the script, messages are
-- logged until stdin closes. A message that does not match exits with 3.
local script_path, log_path = arg[1], arg[2]
local f = assert(io.open(script_path))
local script = vim.json.decode(f:read("*a"), { luanil = { object = true, array = true } })
f:close()
local logf = assert(io.open(log_path, "w"))

local function log(s)
  logf:write(s, "\n")
  logf:flush()
end

local function send(msg)
  msg.jsonrpc = "2.0"
  io.stdout:write(vim.json.encode(msg), "\n")
  io.stdout:flush()
end

local function read()
  local line = io.read("l")
  if not line then
    return nil
  end
  local msg = vim.json.decode(line, { luanil = { object = true, array = true } })
  log(line)
  return msg
end

local held = {}
local next_id = 1000

for _, step in ipairs(script) do
  if step.recv then
    local msg = read()
    if not msg then
      log("EOF while waiting for " .. step.recv)
      os.exit(0)
    end
    if msg.method ~= step.recv then
      log(string.format("UNEXPECTED: wanted %s, got %s", step.recv, tostring(msg.method or "a response")))
      io.stderr:write("fake agent: unexpected message\n")
      os.exit(3)
    end
    if msg.id ~= nil then
      if step.hold then
        held[step.recv] = msg.id
      elseif step.error then
        send({ id = msg.id, error = step.error })
      else
        send({ id = msg.id, result = step.result or vim.empty_dict() })
      end
    end
  elseif step.reply then
    send({ id = held[step.reply], result = step.result or vim.empty_dict() })
    held[step.reply] = nil
  elseif step.notify then
    send({ method = step.notify, params = step.params or vim.empty_dict() })
  elseif step.update then
    send({ method = "session/update", params = { sessionId = step.session or "s1", update = step.update } })
  elseif step.request then
    local id = next_id
    next_id = next_id + 1
    send({ id = id, method = step.request, params = step.params or vim.empty_dict() })
    while true do
      local msg = read()
      if not msg then
        os.exit(0)
      end
      if msg.id == id and msg.method == nil then
        break
      end
    end
  elseif step.stderr then
    io.stderr:write(step.stderr, "\n")
    io.stderr:flush()
  elseif step.exit then
    os.exit(step.exit)
  end
end

while read() do
end
os.exit(0)
