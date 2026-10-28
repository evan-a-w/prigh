-- Against the real `prigh acp -faux-script`: the handshake, a streamed
-- reply, a bash card, slash commands (/name, /budget, /goal) and the winbar
-- from _prigh/state, prigh's notices.
local T = require("helpers")
local agent, chat = T.start_prigh({
  { thinking = "The user says hi.", text = "Hello from prigh, streamed in pieces.", chunks = 4, usage = { input = 60000, output = 3000 } },
  { text = "Running it.", tool_calls = { { id = "c1", name = "bash", arguments = { command = "printf 'one\\ntwo\\nthree\\n'" } } } },
  { text = "Three lines.", usage = { input = 90000, output = 1000 } },
  { text = "On it.", tool_calls = { { id = "g1", name = "goal_complete", arguments = { summary = "said hello" } } } },
  { text = "Goal met." },
})

local function send(text)
  vim.api.nvim_buf_set_lines(chat.input, 0, -1, false, { text })
  vim.api.nvim_set_current_win(chat.input_win)
  T.feed("<CR>")
end

T.section("open: initialize, session/new, commands, _prigh/state")
T.open_prigh()
T.traffic()
T.chat()
T.winbar()

T.section("a streamed reply")
send("hi")
T.turn_end()
T.traffic(function(dir, msg)
  local u = msg.params and msg.params.update
  return dir == "send" or (u and u.sessionUpdate == "agent_message_chunk") or msg.result ~= nil
end)
T.chat()
T.winbar()

T.section("a bash card, expanded with <Tab>")
send("count")
T.turn_end()
T.traffic(function(_, msg)
  return msg.method == "session/update" and msg.params.update.sessionUpdate:match("^tool_call")
end)
T.chat()
vim.api.nvim_set_current_win(chat.win)
for i, l in ipairs(vim.api.nvim_buf_get_lines(chat.buf, 0, -1, false)) do
  if l:find("printf", 1, true) then
    vim.api.nvim_win_set_cursor(chat.win, { i, 0 })
    break
  end
end
T.feed("<Tab>")
T.chat()
T.winbar()

T.section("/name and /budget: the title and the budget in the winbar")
send("/name greetings")
T.turn_end()
send("/budget 5")
T.turn_end()
T.traffic(function(_, msg)
  local u = msg.params and msg.params.update
  return msg.method == "session/prompt" or (u and u.sessionUpdate == "session_info_update") or msg.result ~= nil
end)
T.chat()
T.winbar()

T.section("/goal: the goal in the winbar, prigh's notice when it is met")
send("/goal say hello")
T.wait(function()
  return agent.session.prigh and agent.session.prigh.goal ~= nil
end, "goal")
T.winbar()
T.turn_end()
T.wait(function()
  return agent.session.prigh.goal == nil
end, "goal met")
T.traffic(function(_, msg)
  return msg.method == "_prigh/notice"
end)
T.chat()
T.winbar()
T.finish()
