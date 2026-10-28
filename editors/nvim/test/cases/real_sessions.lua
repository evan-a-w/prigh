-- Against the real prigh: two sessions, :PrighSessions lists them
-- (session/list) and loads one (session/load replays its history),
-- :PrighRestart reloads it, and a new Neovim resumes the last one.
local T = require("helpers")
local replies = {
  { text = "First answer.", tool_calls = { { id = "c1", name = "bash", arguments = { command = "echo replayed" } } } },
  { text = "First done." },
  { thinking = "Second thoughts.", text = "Second answer." },
}
local agent, chat = T.start_prigh(replies)

local function send(text)
  vim.api.nvim_buf_set_lines(chat.input, 0, -1, false, { text })
  chat:send()
  T.turn_end()
end
local first
local function loaded()
  T.wait(function()
    return agent.session.id == first and not agent.session.loading and agent.session.prigh
  end, "the first session loaded", 15000)
end

T.section("session 1: a prompt with a tool")
T.open_prigh()
first = agent.session.id
send("the first session")
T.chat()

T.section("session 2 (:PrighNew)")
vim.cmd("PrighNew")
T.wait(function()
  return agent.session.id ~= first and agent.session.prigh
end, "new session")
T.mask(agent.session.id)
send("the second session")
T.chat()
T.traffic_skip()

T.section(":PrighSessions: pick the first, its history replayed")
T.answer("the first session")
vim.cmd("PrighSessions")
loaded()
T.traffic(function(_, msg)
  local u = msg.params and msg.params.update
  return (msg.method and msg.method:match("^session/[ls]")) or (msg.result and msg.result.sessions)
    or (msg.result and msg.result.modes)
    or (u and u.sessionUpdate:match("^tool_call"))
end)
T.chat()
T.winbar()
T.out("current is the first: " .. tostring(agent.session.id == first))

T.section(":PrighRestart: a new agent process reloads it")
local old_conn = agent.conn
vim.cmd("PrighRestart")
T.wait(function()
  return agent.conn ~= old_conn and agent.conn_state == "ready"
end, "restart", 15000)
loaded()
T.chat()

T.section("a new Neovim (resume_last): the last session comes back")
require("prigh").reset()
agent, chat = T.start_prigh(replies, { setup = { resume_last = true } })
vim.cmd("Prigh")
loaded()
T.out("current is the first: " .. tostring(agent.session.id == first))
T.chat()
T.finish()
