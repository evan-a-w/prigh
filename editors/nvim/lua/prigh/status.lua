-- The state line: what the agent reports (ACP config options, modes, usage
-- and prigh's _prigh/state extension), as segments for the winbar and for
-- statusline plugins.
local session_mod = require("prigh.session")

local M = {}

local function tokens(n)
  if not n then
    return "?"
  end
  if n >= 1000000 then
    return string.format("%.1fM", n / 1000000)
  elseif n >= 1000 then
    return string.format("%dk", math.floor(n / 1000 + 0.5))
  end
  return tostring(n)
end

local function truncate(s, n)
  s = tostring(s)
  if vim.fn.strchars(s) > n then
    return vim.fn.strcharpart(s, 0, n - 1) .. "…"
  end
  return s
end

--- Segments: list of { text, hl }. `agent` gives the connection state.
function M.segments(agent)
  local segs = {}
  local function add(text, hl)
    if text and text ~= "" then
      table.insert(segs, { text = text, hl = hl or "PrighStatus" })
    end
  end
  local s = agent and agent.session
  if not agent or agent.conn_state ~= "ready" then
    add(
      agent and agent.conn_state or "stopped",
      agent and agent.conn_state == "exited" and "PrighError" or "PrighMuted"
    )
  end
  if not s then
    return segs
  end
  local p = s.prigh or {}
  local model_opt = s:config_option("model")
  add(p.model or (model_opt and session_mod.option_value_name(model_opt)), "PrighStatusModel")
  local thinking_opt = s:config_option("thought_level")
  local thinking = p.thinking or (thinking_opt and session_mod.option_value_name(thinking_opt))
  if thinking then
    add("thinking " .. tostring(thinking))
  end
  local mode = s:current_mode_name()
  if mode then
    add("mode " .. mode)
  end
  if p.approval_mode then
    add("approve " .. tostring(p.approval_mode))
  end
  if p.plan_mode then
    add("plan mode", "PrighWarning")
  end
  local used = p.context_tokens or (s.usage and s.usage.used)
  local size = p.context_window or (s.usage and s.usage.size)
  if used then
    local pct = size and size > 0 and string.format(" (%d%%)", math.floor(used * 100 / size + 0.5)) or ""
    add(string.format("ctx %s/%s%s", tokens(used), tokens(size), pct))
  end
  local cost = p.cost_usd
  if cost then
    add(string.format("$%.2f", cost))
  elseif s.usage and s.usage.cost and s.usage.cost.amount then
    local c = s.usage.cost
    add(string.format(c.currency == "USD" and "$%.2f" or ("%.2f " .. tostring(c.currency)), c.amount))
  end
  if s.running then
    add("● running", "PrighRunning")
  end
  local queued = #s.queue + (tonumber(p.queued) or 0)
  if queued > 0 then
    add(string.format("queued %d", queued), "PrighWarning")
  end
  if p.subagents and tonumber(p.subagents) and p.subagents > 0 then
    add(string.format("agents %d", p.subagents))
  end
  if p.jobs and tonumber(p.jobs) and p.jobs > 0 then
    add(string.format("jobs %d", p.jobs))
  end
  if p.reminders and tonumber(p.reminders) and p.reminders > 0 then
    add(string.format("reminders %d", p.reminders))
  end
  if p.goal and p.goal ~= vim.NIL and p.goal ~= "" then
    add("goal " .. truncate(p.goal, 30))
  end
  if agent.pending_permission then
    add("permission needed", "PrighPermission")
  end
  return segs
end

function M.text(agent)
  local parts = {}
  for _, seg in ipairs(M.segments(agent)) do
    table.insert(parts, seg.text)
  end
  return table.concat(parts, " · ")
end

local function esc(s)
  return (s:gsub("%%", "%%%%"))
end

--- Winbar for the chat window: title, then the state.
function M.winbar(agent)
  local s = agent and agent.session
  local title = (s and s.title) or "prigh"
  local parts = { "%#PrighWinbarTitle#" .. esc(truncate(title, 40)) }
  for _, seg in ipairs(M.segments(agent)) do
    table.insert(parts, "%#" .. seg.hl .. "#" .. esc(seg.text))
  end
  return table.concat(parts, "%#PrighMuted# · ") .. "%*"
end

return M
