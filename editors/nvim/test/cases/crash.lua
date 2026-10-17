-- The agent dying mid-turn, a missing executable, and :PrighRestart
-- reloading the session.
local T = require("helpers")
local script = T.handshake()
vim.list_extend(script, {
  { recv = "session/prompt", hold = true },
  { update = { sessionUpdate = "agent_message_chunk", content = { type = "text", text = "Working" } } },
  { stderr = "panic: out of cheese" },
  { exit = 2 },
})
local agent, chat = T.start(script)
vim.cmd("Prigh")
T.wait(function()
  return agent.session.id
end, "session")
vim.api.nvim_buf_set_lines(chat.input, 0, -1, false, { "hello" })
chat:send()
T.wait(function()
  return agent.conn_state == "exited"
end, "exit")
T.chat()
T.winbar()

T.section(":PrighRestart reloads the session")
local script2 = {
  { recv = "initialize", result = T.init_result },
  { recv = "session/load", hold = true },
  { update = { sessionUpdate = "user_message_chunk", content = { type = "text", text = "hello" } } },
  { update = { sessionUpdate = "agent_message_chunk", content = { type = "text", text = "Working" } } },
  { reply = "session/load", result = { modes = T.new_result.modes } },
}
T.start(script2)
vim.cmd("PrighRestart")
T.wait(function()
  return agent.conn_state == "ready"
    and agent.session.id == "s1"
    and #agent.session.entries == 2
    and not agent.session.loading
end, "reload")
T.agent_log()
T.chat()
T.winbar()
T.finish()

T.section("missing executable")
require("prigh").reset()
require("prigh").setup({ cmd = { "prigh-does-not-exist", "acp" }, cwd = T.tmp })
vim.cmd("Prigh")
T.chat()
T.winbar()
