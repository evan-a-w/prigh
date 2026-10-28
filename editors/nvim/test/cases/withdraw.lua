-- The agent withdraws what it asked ($/cancel_request, e.g. answered in
-- another frontend): the card closes (answered -32800) and the next one gets
-- the keyboard; a withdrawn form is answered -32800 too. After session/cancel,
-- requests still coming are answered cancelled without a card. _prigh/notice
-- shows in the chat (only for this session).
local T = require("helpers")
local options = {
  { optionId = "allow", name = "Allow", kind = "allow_once" },
  { optionId = "reject", name = "Reject", kind = "reject_once" },
}
local function req(id, title, nowait)
  return {
    request = "session/request_permission",
    id = id,
    nowait = nowait,
    params = { sessionId = "s1", toolCall = { toolCallId = "t" .. id, title = title, kind = "execute" }, options = options },
  }
end
local script = T.handshake()
vim.list_extend(script, {
  { notify = "_prigh/notice", params = { sessionId = "s1", text = "tools now run on laptop" } },
  { notify = "_prigh/notice", params = { sessionId = "other", text = "not ours" } },
  { recv = "session/prompt", hold = true },
  req(7, "first", true),
  req(8, "second", true),
  { notify = "$/cancel_request", params = { requestId = 7 } },
  { await = 7 },
  { await = 8 },
  {
    request = "elicitation/create",
    id = 9,
    nowait = true,
    params = {
      sessionId = "s1",
      mode = "form",
      message = "Pick",
      requestedSchema = { type = "object", properties = { x = { type = "string", title = "X" } } },
    },
  },
  { notify = "$/cancel_request", params = { requestId = 9 } },
  { await = 9 },
  { notify = "$/cancel_request", params = { requestId = 999 } },
  { recv = "session/cancel" },
  req(10, "after cancel"),
  { reply = "session/prompt", result = { stopReason = "cancelled" } },
})
local agent, chat = T.start(script)
-- The form waits for the user: its first prompt never answers.
require("prigh.elicit").run = function(params)
  T.out("form shown: " .. params.message)
end
vim.cmd("Prigh")
T.wait(function()
  return agent.session.id and #agent.session.entries >= 1
end, "session")
T.chat()

T.section("two cards; the agent withdraws the first")
vim.api.nvim_buf_set_lines(chat.input, 0, -1, false, { "go" })
chat:send()
T.wait(function()
  return agent.pending_permission and agent.pending_permission.title == "second"
end, "second card")
T.wait_agent('"id":7', "the withdrawn card's answer")
T.agent_log()
T.chat()
T.winbar()

T.section("the second has the keyboard: a")
T.out("focus in chat: " .. tostring(vim.api.nvim_get_current_win() == chat.win))
T.feed("a")
T.wait_agent('"id":8', "answer")
T.agent_log()

T.section("a withdrawn form")
T.wait_agent('"id":9', "form answer")
T.agent_log()
T.chat()

T.section("cancel: what comes after is answered cancelled, no card")
vim.cmd("PrighCancel")
T.wait(function()
  return not agent.session.running
end, "turn end")
T.agent_log()
T.chat()
T.finish()
