-- Resuming the last session of the directory, :PrighSessions (list, load
-- with replay, new), :PrighMode, :PrighModel, :PrighConfig and the winbar.
local T = require("helpers")
vim.fn.mkdir(vim.fn.stdpath("data") .. "/prigh", "p")
local f = assert(io.open(vim.fn.stdpath("data") .. "/prigh/last_sessions.json", "w"))
f:write(vim.json.encode({ [T.tmp] = "old" }))
f:close()

local configured = vim.deepcopy(T.new_result.configOptions)
configured[1].currentValue = "opus"
local script = {
  { recv = "initialize", result = T.init_result },
  { recv = "session/load", hold = true },
  {
    update = { sessionUpdate = "user_message_chunk", content = { type = "text", text = "earlier question" } },
    session = "old",
  },
  {
    update = { sessionUpdate = "agent_message_chunk", content = { type = "text", text = "earlier answer" } },
    session = "old",
  },
  {
    update = { sessionUpdate = "tool_call", toolCallId = "x", title = "ls", kind = "execute", status = "completed" },
    session = "old",
  },
  { reply = "session/load", result = { configOptions = T.new_result.configOptions, modes = T.new_result.modes } },
  { recv = "session/set_config_option", result = { configOptions = configured } },
  { recv = "session/set_mode" },
  { recv = "session/set_config_option", result = { configOptions = configured } },
  {
    recv = "session/list",
    result = {
      sessions = {
        { sessionId = "old", cwd = T.tmp, title = "Earlier work", updatedAt = "2026-10-01T10:00:00Z" },
        { sessionId = "older", cwd = T.tmp, title = "Older work", updatedAt = "2026-09-01T10:00:00Z" },
      },
      nextCursor = "p2",
    },
  },
  {
    recv = "session/list",
    result = { sessions = {
      { sessionId = "oldest", cwd = T.tmp, updatedAt = "2026-08-01T10:00:00Z" },
    } },
  },
  { recv = "session/load", hold = true },
  {
    update = { sessionUpdate = "user_message_chunk", content = { type = "text", text = "older question" } },
    session = "older",
  },
  { update = { sessionUpdate = "agent_message_chunk", content = { type = "text", text = "stale" } }, session = "old" },
  { reply = "session/load", result = vim.empty_dict() },
  { recv = "session/list", result = { sessions = {} } },
  { recv = "session/new", result = { sessionId = "fresh" } },
}
local agent, chat = T.start(script, { resume_last = true })

T.section("open: resumes the last session of this directory")
vim.cmd("Prigh")
T.wait(function()
  return agent.session.id == "old" and not agent.session.loading and agent.session.config_options
end, "load")
T.chat()
T.winbar()

T.section(":PrighModel")
T.answer("claude-opus")
vim.cmd("PrighModel")
T.wait(function()
  return agent.session:config_option("model").currentValue == "opus"
end, "model")
T.winbar()

T.section(":PrighMode (modes, no mode config option)")
T.answer("Plan")
vim.cmd("PrighMode")
T.wait(function()
  return agent.session.modes.currentModeId == "plan"
end, "mode")
T.winbar()

T.section(":PrighConfig")
T.answer("Thinking", "high")
vim.cmd("PrighConfig")
T.wait_agent("set_config_option.*high", "config")

T.section(":PrighSessions loads another session (all pages)")
T.answer("Older work")
vim.cmd("PrighSessions")
T.wait(function()
  return agent.session.id == "older" and not agent.session.loading
end, "load older")
T.chat()
T.winbar()

T.section(":PrighSessions → new")
T.answer("new session")
vim.cmd("PrighSessions")
T.wait(function()
  return agent.session.id == "fresh"
end, "new")
T.chat()
T.winbar()
T.out("last session: " .. agent:last_session_id())
T.finish()
