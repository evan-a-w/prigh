-- Against the real prigh: the mode, model and thinking pickers (config
-- options) and the winbar following them; cancelling a running turn with a
-- queued prompt.
local T = require("helpers")
local agent, chat = T.start_prigh({
  { text = "Sleeping.", tool_calls = { { id = "s1", name = "bash", arguments = { command = "sleep 30" } } } },
  { text = "never shown" },
}, { auth = { deepseek = { type = "api_key", key = "sk-test" } } })

local function config_traffic()
  T.traffic(function(_, msg)
    local u = msg.params and msg.params.update
    return msg.method == "session/set_config_option"
      or (u and (u.sessionUpdate == "current_mode_update" or u.sessionUpdate == "config_option_update"))
      or (msg.result and msg.result.configOptions and not msg.result.sessionId)
  end)
end
local function current()
  local s = agent.session
  local out = {}
  for _, o in ipairs(s.config_options or {}) do
    table.insert(out, o.id .. "=" .. tostring(o.currentValue))
  end
  return table.concat(out, " ") .. " modes.current=" .. tostring(s.modes and s.modes.currentModeId)
end

T.open_prigh()
T.traffic_skip()
T.winbar()

T.section(":PrighMode: Plan")
T.answer("Plan")
vim.cmd("PrighMode")
T.wait(function()
  return agent.session.prigh.plan_mode == true
end, "plan mode")
config_traffic()
T.out(current())
T.winbar()

T.section(":PrighMode: back to Ask")
T.answer("Ask")
vim.cmd("PrighMode")
T.wait(function()
  return agent.session.prigh.approval_mode == "ask" and agent.session.prigh.plan_mode == false
end, "ask mode")
config_traffic()
T.out(current())
T.winbar()

T.section(":PrighModel: another model")
T.answer("deepseek/deepseek-v4-pro")
vim.cmd("PrighModel")
T.wait(function()
  return agent.session.prigh.model == "deepseek/deepseek-v4-pro"
end, "model")
config_traffic()
T.out(current())
T.winbar()

T.section(":PrighConfig thinking: high")
T.answer("high")
vim.cmd("PrighConfig thinking")
T.wait(function()
  return agent.session.prigh.thinking == "high"
end, "thinking")
config_traffic()
T.out(current())
T.winbar()

T.section("a long command; a prompt queued behind it")
T.answer("Auto —")
vim.cmd("PrighMode")
T.wait(function()
  return agent.session.prigh.approval_mode == "auto"
end, "auto mode")
vim.api.nvim_buf_set_lines(chat.input, 0, -1, false, { "sleep" })
chat:send()
T.wait(function()
  local t = agent.session.tools.s1
  return t and t.status == "in_progress"
end, "the command runs", 15000)
vim.api.nvim_buf_set_lines(chat.input, 0, -1, false, { "and then this" })
chat:send()
T.chat()
T.winbar()
T.traffic_skip()

T.section(":PrighCancel: the turn stops, the queued prompt goes back to the input")
vim.cmd("PrighCancel")
T.turn_end()
T.traffic(function(_, msg)
  return msg.method == "session/cancel"
    or (msg.result and msg.result.stopReason)
    or (msg.params and msg.params.update and msg.params.update.status)
end)
T.chat()
T.winbar()
T.out("input: " .. vim.inspect(vim.api.nvim_buf_get_lines(chat.input, 0, -1, false)))
T.finish()
