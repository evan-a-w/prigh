-- initialize + session/new, a streamed turn with thoughts, tool calls, a
-- plan and a diff; the winbar from _prigh/state.
local T = require("helpers")
local script = T.handshake()
vim.list_extend(script, {
  {
    notify = "_prigh/state",
    params = {
      sessionId = "s1",
      model = "claude-sonnet",
      thinking = "medium",
      running = false,
      context_tokens = 1200,
      context_window = 200000,
      cost_usd = 0.0123,
      approval_mode = "ask",
      plan_mode = false,
      queued = { steer = 0, follow_up = 0 },
      subagents = {},
      jobs = 0,
      goal = vim.NIL,
      reminders = 0,
    },
  },
  {
    update = {
      sessionUpdate = "available_commands_update",
      availableCommands = { { name = "compact", description = "Compact the context" } },
    },
  },
  { recv = "session/prompt", hold = true },
  {
    update = {
      sessionUpdate = "agent_thought_chunk",
      content = { type = "text", text = "The user wants a fix.\nLet me look" },
    },
  },
  { update = { sessionUpdate = "agent_thought_chunk", content = { type = "text", text = " at the file." } } },
  { update = { sessionUpdate = "agent_message_chunk", content = { type = "text", text = "Looking at " } } },
  { update = { sessionUpdate = "agent_message_chunk", content = { type = "text", text = "`foo.ml`." } } },
  {
    update = {
      sessionUpdate = "plan",
      entries = {
        { content = "Read foo.ml", priority = "high", status = "in_progress" },
        { content = "Fix the bug", priority = "high", status = "pending" },
      },
    },
  },
  {
    update = {
      sessionUpdate = "tool_call",
      toolCallId = "t1",
      title = "Read foo.ml",
      kind = "read",
      status = "pending",
      locations = { { path = T.tmp .. "/foo.ml", line = 3 } },
    },
  },
  {
    update = {
      sessionUpdate = "tool_call_update",
      toolCallId = "t1",
      status = "completed",
      content = { { type = "content", content = { type = "text", text = "let x = 1\nlet y = 2\n" } } },
    },
  },
  {
    update = {
      sessionUpdate = "tool_call",
      toolCallId = "t2",
      title = "Edit foo.ml",
      kind = "edit",
      status = "in_progress",
      content = {
        {
          type = "diff",
          path = T.tmp .. "/foo.ml",
          oldText = "let x = 1\nlet y = 2\n",
          newText = "let x = 1\nlet y = 3\n",
        },
      },
    },
  },
  { update = { sessionUpdate = "tool_call_update", toolCallId = "t2", status = "failed" } },
  {
    update = {
      sessionUpdate = "plan",
      entries = {
        { content = "Read foo.ml", priority = "high", status = "completed" },
        { content = "Fix the bug", priority = "high", status = "in_progress" },
      },
    },
  },
  {
    notify = "_prigh/state",
    params = {
      sessionId = "s1",
      model = "claude-sonnet",
      thinking = "medium",
      running = true,
      context_tokens = 52000,
      context_window = 200000,
      cost_usd = 0.5,
      approval_mode = "ask",
      plan_mode = true,
      queued = { steer = 1, follow_up = 2 },
      subagents = {
        { id = "a1", label = "count files", running = true },
        { id = "a2", label = "say hello", running = true },
        { id = "a3", label = "done already", running = false },
      },
      jobs = 1,
      goal = "fix the bug",
      reminders = 1,
    },
  },
  { update = { sessionUpdate = "agent_message_chunk", content = { type = "text", text = "Done." } } },
  {
    update = { sessionUpdate = "usage_update", used = 52000, size = 200000, cost = { amount = 0.5, currency = "USD" } },
  },
  { update = { sessionUpdate = "session_info_update", title = "Fix the bug" } },
  { reply = "session/prompt", result = { stopReason = "end_turn" } },
})
local agent, chat = T.start(script)

T.section("open")
vim.cmd("Prigh")
T.wait(function()
  return agent.session.id == "s1" and agent.session.prigh ~= nil and #agent.session.commands > 0
end, "session")
T.agent_log()
T.winbar()

T.section("send a prompt")
vim.api.nvim_buf_set_lines(chat.input, 0, -1, false, { "fix the bug" })
vim.api.nvim_set_current_win(chat.input_win)
T.feed("<CR>")
T.out("input after send: " .. vim.inspect(vim.api.nvim_buf_get_lines(chat.input, 0, -1, false)))
T.wait(function()
  return agent.session.title == "Fix the bug" and not agent.session.running
end, "turn end")
T.agent_log()
T.chat()
T.winbar()
T.out("statusline: " .. require("prigh").statusline())

T.section("expand the thought and the tool output")
vim.api.nvim_set_current_win(chat.win)
local function goto_line(pat)
  for i, l in ipairs(vim.api.nvim_buf_get_lines(chat.buf, 0, -1, false)) do
    if l:find(pat, 1, true) then
      vim.api.nvim_win_set_cursor(chat.win, { i, 0 })
      return
    end
  end
  error("no line " .. pat)
end
goto_line("▸ Thinking")
T.feed("<Tab>")
goto_line("lines of output")
T.feed("<Tab>")
T.chat()
T.section("collapse again")
goto_line("▾ Thinking")
T.feed("<Tab>")
T.chat()
T.finish()
