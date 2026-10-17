-- :PrighReview after the agent wrote files with fs/write_text_file: the
-- picker, reviewing each file, reverting rejected hunks and saving.
local T = require("helpers")
local function write(path, text)
  local f = assert(io.open(path, "w"))
  f:write(text)
  f:close()
end
local function read(path)
  local f = io.open(path)
  local s = f and f:read("*a") or "(missing)"
  if f then
    f:close()
  end
  return s
end
local a, b = T.tmp .. "/a.txt", T.tmp .. "/b.txt"
write(a, "1\n2\n3\n")
write(b, "x\ny\n")
local script = T.handshake()
vim.list_extend(script, {
  { recv = "session/prompt", hold = true },
  { request = "fs/write_text_file", params = { sessionId = "s1", path = a, content = "1\nTWO\n3\n4\n" } },
  { request = "fs/write_text_file", params = { sessionId = "s1", path = b, content = "x\nY\n" } },
  { request = "fs/write_text_file", params = { sessionId = "s1", path = a, content = "1\nTWO\n3\nfour\n" } },
  { reply = "session/prompt", result = { stopReason = "end_turn" } },
})
local agent, chat = T.start(script)
vim.cmd.edit(a)
vim.cmd("Prigh")
T.wait(function()
  return agent.session.id
end, "session")
vim.api.nvim_buf_set_lines(chat.input, 0, -1, false, { "change things" })
chat:send()
T.wait(function()
  return not agent.session.running
end, "turn end")
T.chat()

T.section(":PrighReview from the chat: pick b.txt, reject it")
vim.api.nvim_set_current_win(chat.win)
T.answer("b.txt")
vim.cmd("PrighReview")
local rb = vim.api.nvim_get_current_buf()
T.out(
  "reviewing " .. vim.api.nvim_buf_get_name(rb),
  "in chat window: " .. tostring(vim.api.nvim_get_current_win() == chat.win)
)
T.buffer(rb, require("prigh.review").ns)
T.answer("a.txt")
T.feed("gr")
T.out("b.txt disk: " .. vim.inspect(read(b)), "modified: " .. tostring(vim.bo[rb].modified))

T.section("the picker comes back with a.txt: accept one hunk, reject the other")
vim.wait(100, function()
  return false
end)
local ra = vim.api.nvim_get_current_buf()
T.buffer(ra, require("prigh.review").ns)
T.feed("ga")
T.feed("gr")
T.out("a.txt disk: " .. vim.inspect(read(a)))

T.section("nothing left")
vim.cmd("PrighReview")
T.finish()
