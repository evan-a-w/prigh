-- session/request_permission: the card, each key, <CR> on an option, Esc,
-- a long diff in a float, and keys blocked while it is pending.
local T = require("helpers")
local options = {
  { optionId = "once", name = "Allow once", kind = "allow_once" },
  { optionId = "always", name = "Always allow", kind = "allow_always" },
  { optionId = "reject", name = "Reject", kind = "reject_once" },
}
local function req(title, content)
  return {
    request = "session/request_permission",
    params = {
      sessionId = "s1",
      toolCall = {
        toolCallId = "t" .. title,
        title = title,
        kind = "edit",
        content = content,
        locations = { { path = T.tmp .. "/a.txt" } },
      },
      options = options,
    },
  }
end
local small = { { type = "diff", path = T.tmp .. "/a.txt", oldText = "one\ntwo\n", newText = "one\n2\n" } }
local long_new = {}
for i = 1, 30 do
  long_new[i] = "line " .. i
end
local long =
  { { type = "diff", path = T.tmp .. "/a.txt", oldText = "", newText = table.concat(long_new, "\n") .. "\n" } }
local script = T.handshake()
vim.list_extend(script, {
  { recv = "session/prompt", hold = true },
  req("Edit a.txt", small),
  req("Write b", nil),
  req("Write c", nil),
  req("Write d", nil),
  req("Long edit", long),
  { reply = "session/prompt", result = { stopReason = "end_turn" } },
})
local agent, chat = T.start(script, { permission = { inline_diff_max = 5 } })
vim.cmd("Prigh")
T.wait(function()
  return agent.session.id
end, "session")
vim.api.nvim_buf_set_lines(chat.input, 0, -1, false, { "edit it" })
chat:send()

local function pending(title)
  T.wait(function()
    return agent.pending_permission and agent.pending_permission.title == title
  end, title)
end

T.section("card with a small diff")
pending("Edit a.txt")
T.chat()
T.winbar()
T.out(
  "focus in chat: " .. tostring(vim.api.nvim_get_current_win() == chat.win),
  "cursor row: " .. vim.api.nvim_win_get_cursor(0)[1]
)
T.feed("q")
T.feed("a")
T.wait_agent('"once"', "answer")
T.agent_log()

T.section("A: always")
pending("Write b")
T.feed("A")
T.wait_agent('"always"', "answer")
T.agent_log()

T.section("<CR> on the reject option")
pending("Write c")
vim.api.nvim_win_set_cursor(chat.win, { vim.api.nvim_win_get_cursor(chat.win)[1] + 2, 0 })
T.out("cursor on: " .. vim.api.nvim_get_current_line())
T.feed("<CR>")
T.wait_agent('"reject"', "answer")
T.agent_log()

T.section("Esc cancels")
pending("Write d")
T.feed("<Esc>")
T.wait_agent('"cancelled"', "answer")
T.agent_log()

T.section("long diff: cut inline, d opens the float")
pending("Long edit")
T.chat()
T.feed("d")
local fbuf = vim.api.nvim_get_current_buf()
T.out("float filetype: " .. vim.bo[fbuf].filetype .. ", lines: " .. vim.api.nvim_buf_line_count(fbuf))
T.feed("q")
T.out("back in chat: " .. tostring(vim.api.nvim_get_current_win() == chat.win))
T.feed("r")
T.wait(function()
  return not agent.session.running
end, "turn end")
T.agent_log()
T.chat()
T.section("keys after the cards are answered")
T.out("chat a maps to: " .. vim.fn.maparg("a", "n", false, true).desc)
T.finish()
