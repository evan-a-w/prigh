-- Prompts queue while a turn runs; <C-c> cancels the turn and gives the
-- queued prompts back to the input; a turn ends with the next queued one.
local T = require("helpers")
local script = T.handshake()
vim.list_extend(script, {
  { recv = "session/prompt", hold = true },
  { update = { sessionUpdate = "agent_message_chunk", content = { type = "text", text = "first answer" } } },
  { recv = "session/cancel" },
  { reply = "session/prompt", result = { stopReason = "cancelled" } },
  { recv = "session/prompt", hold = true },
  { update = { sessionUpdate = "agent_message_chunk", content = { type = "text", text = "second answer" } } },
  { reply = "session/prompt", result = { stopReason = "end_turn" } },
  { recv = "session/prompt", hold = true },
  { reply = "session/prompt", result = { stopReason = "max_tokens" } },
})
local agent, chat = T.start(script)
vim.cmd("Prigh")
T.wait(function()
  return agent.session.id
end, "session")
local function send(text)
  vim.api.nvim_buf_set_lines(chat.input, 0, -1, false, vim.split(text, "\n"))
  chat:send()
end

T.section("queue two prompts during a turn")
send("first")
T.wait(function()
  return #agent.session.entries == 2
end, "chunk")
send("second")
send("third\nwith two lines")
T.chat()
T.winbar()

T.section("<C-c>: cancel, queued prompts back in the input")
vim.api.nvim_set_current_win(chat.input_win)
T.feed("<C-c>")
T.wait(function()
  return not agent.session.running
end, "cancelled")
T.chat()
T.out("input: " .. vim.inspect(vim.api.nvim_buf_get_lines(chat.input, 0, -1, false)))
T.winbar()

T.section("a queued prompt is sent when the turn ends")
send("second")
T.wait(function()
  return agent.session.running
end, "running")
send("third")
T.wait(function()
  return not agent.session.running and #agent.session.queue == 0 and #agent.session.entries >= 7
end, "both turns")
T.chat()
T.agent_log()
T.finish()
