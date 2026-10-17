-- The state of one ACP session, built from session/update notifications.
-- UI-agnostic: views subscribe with `on(fn)` and get ("entry", index),
-- ("status") or ("reset") events.
local M = {}

local S = {}
S.__index = S

function M.new(cwd)
  return setmetatable({
    cwd = cwd,
    id = nil,
    title = nil,
    entries = {},
    tools = {}, -- toolCallId -> entry
    modes = nil, -- { currentModeId, availableModes }
    config_options = nil, -- ConfigOption[]
    commands = {}, -- AvailableCommand[]
    usage = nil, -- { used, size, cost = { amount, currency } }
    prigh = nil, -- the _prigh/state notification
    running = false,
    queue = {}, -- prompts waiting for the current turn
    listeners = {},
  }, S)
end

function S:on(fn)
  table.insert(self.listeners, fn)
end

function S:emit(kind, idx)
  for _, fn in ipairs(self.listeners) do
    fn(kind, idx)
  end
end

--- Forget everything that belongs to the old session (keeps listeners).
function S:reset(id)
  local listeners = self.listeners
  local cwd = self.cwd
  for k in pairs(self) do
    self[k] = nil
  end
  local fresh = M.new(cwd)
  for k, v in pairs(fresh) do
    self[k] = v
  end
  self.listeners = listeners
  self.id = id
  self:emit("reset")
end

function S:add(entry)
  table.insert(self.entries, entry)
  entry.index = #self.entries
  self:emit("entry", entry.index)
  return entry
end

function S:changed(entry)
  self:emit("entry", entry.index)
end

function S:notice(severity, title, description)
  return self:add({ kind = "notice", severity = severity, title = title, description = description })
end

--- Text of a content block as shown in the chat.
function M.block_text(block)
  if not block then
    return ""
  end
  if block.type == "text" then
    return block.text or ""
  elseif block.type == "resource_link" then
    return string.format("[@%s](%s)", block.name or block.uri, block.uri)
  elseif block.type == "resource" then
    local r = block.resource or {}
    return string.format("[%s]", r.uri or "resource")
  elseif block.type == "image" then
    return "[image]"
  elseif block.type == "audio" then
    return "[audio]"
  end
  return string.format("[%s]", tostring(block.type))
end

function S:_append_chunk(kind, update)
  local last = self.entries[#self.entries]
  local text = M.block_text(update.content)
  if
    last
    and last.kind == kind
    and not last.closed
    and (update.messageId == nil or last.message_id == nil or last.message_id == update.messageId)
  then
    last.text = last.text .. text
    self:changed(last)
  else
    self:add({ kind = kind, text = text, message_id = update.messageId })
  end
end

-- ACP field -> entry field (`kind` names the entry type, so the tool's is `tool_kind`).
local tool_fields = {
  title = "title",
  kind = "tool_kind",
  status = "status",
  content = "content",
  locations = "locations",
  rawInput = "rawInput",
  rawOutput = "rawOutput",
}

function S:_tool(update)
  local id = update.toolCallId
  local e = self.tools[id]
  if not e then
    e = { kind = "tool", id = id, title = "tool", status = "pending" }
    self.tools[id] = e
    for f, ef in pairs(tool_fields) do
      if update[f] ~= nil then
        e[ef] = update[f]
      end
    end
    self:add(e)
    return e
  end
  for f, ef in pairs(tool_fields) do
    if update[f] ~= nil then
      e[ef] = update[f]
    end
  end
  self:changed(e)
  return e
end

function S:_plan(entries)
  for i = #self.entries, 1, -1 do
    local e = self.entries[i]
    if e.kind == "user" then
      break
    elseif e.kind == "plan" then
      e.items = entries
      self:changed(e)
      return
    end
  end
  self:add({ kind = "plan", items = entries })
end

--- Apply a session/update notification's `update`.
function S:apply(update)
  local u = update.sessionUpdate
  if u == "agent_message_chunk" then
    self:_append_chunk("agent", update)
  elseif u == "agent_thought_chunk" then
    self:_append_chunk("thought", update)
  elseif u == "user_message_chunk" then
    self:_append_chunk("user", update)
  elseif u == "tool_call" then
    self:_tool(update)
  elseif u == "tool_call_update" then
    self:_tool(update)
  elseif u == "plan" then
    self:_plan(update.entries or {})
  elseif u == "available_commands_update" then
    self.commands = update.availableCommands or {}
    self:emit("status")
  elseif u == "current_mode_update" then
    self.modes = self.modes or { availableModes = {} }
    self.modes.currentModeId = update.currentModeId or update.modeId
    self:emit("status")
  elseif u == "config_option_update" then
    self.config_options = update.configOptions
    self:emit("status")
  elseif u == "session_info_update" then
    if update.title ~= nil then
      self.title = update.title
    end
    self:emit("status")
  elseif u == "usage_update" then
    self.usage = { used = update.used, size = update.size, cost = update.cost }
    self:emit("status")
  elseif u == "notice" then
    self:notice(update.severity or "info", update.title or "", update.description)
  end
end

--- Close the current streamed message so the next chunk starts a new one.
function S:close_streams()
  local last = self.entries[#self.entries]
  if last and (last.kind == "agent" or last.kind == "thought" or last.kind == "user") then
    last.closed = true
  end
end

function S:set_prigh_state(st)
  self.prigh = st
  self:emit("status")
end

function S:set_running(running)
  self.running = running
  self:emit("status")
end

--- The config option of a category ("model", "mode", "thought_level"), if any.
function S:config_option(category)
  for _, o in ipairs(self.config_options or {}) do
    if o.category == category or (category == "thought_level" and o.id == "thinking") or o.id == category then
      return o
    end
  end
end

--- Flat list of the values of a select option.
function M.option_values(opt)
  local out = {}
  for _, v in ipairs(opt.options or {}) do
    if v.group and v.options then
      for _, vv in ipairs(v.options) do
        table.insert(out, vv)
      end
    else
      table.insert(out, v)
    end
  end
  return out
end

function M.option_value_name(opt)
  if opt.type == "boolean" then
    return opt.currentValue and "on" or "off"
  end
  for _, v in ipairs(M.option_values(opt)) do
    if v.value == opt.currentValue then
      return v.name
    end
  end
  return tostring(opt.currentValue)
end

function S:current_mode_name()
  local opt = self:config_option("mode")
  if opt then
    return M.option_value_name(opt)
  end
  if self.modes and self.modes.currentModeId then
    for _, m in ipairs(self.modes.availableModes or {}) do
      if m.id == self.modes.currentModeId then
        return m.name
      end
    end
    return self.modes.currentModeId
  end
end

return M
