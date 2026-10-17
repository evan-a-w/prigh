-- fs/read_text_file reads unsaved buffers (line/limit) or the disk;
-- fs/write_text_file writes into a loaded buffer as one undo step and saves
-- it, or writes the disk.
local T = require("helpers")
local function write(path, text)
  local f = assert(io.open(path, "w"))
  f:write(text)
  f:close()
end
local function read(path)
  local f = io.open(path)
  if not f then
    return "(missing)"
  end
  local s = f:read("*a")
  f:close()
  return s
end
local open_path = T.tmp .. "/open.txt"
local disk_path = T.tmp .. "/disk.txt"
write(open_path, "one\ntwo\nthree\nfour\n")
write(disk_path, "d1\nd2\nd3\n")

local script = T.handshake()
vim.list_extend(script, {
  { recv = "session/prompt", hold = true },
  { request = "fs/read_text_file", params = { sessionId = "s1", path = open_path } },
  { request = "fs/read_text_file", params = { sessionId = "s1", path = open_path, line = 2, limit = 2 } },
  { request = "fs/read_text_file", params = { sessionId = "s1", path = disk_path, line = 3 } },
  { request = "fs/read_text_file", params = { sessionId = "s1", path = T.tmp .. "/nope.txt" } },
  {
    request = "fs/write_text_file",
    params = { sessionId = "s1", path = open_path, content = "ONE\ntwo (unsaved)\nthree\nFOUR\nfive\n" },
  },
  { request = "fs/write_text_file", params = { sessionId = "s1", path = disk_path, content = "d1\nD2\n" } },
  { request = "fs/write_text_file", params = { sessionId = "s1", path = T.tmp .. "/sub/new.txt", content = "new\n" } },
  { reply = "session/prompt", result = { stopReason = "end_turn" } },
})
local agent, chat = T.start(script)

-- The user has open.txt open with an unsaved change.
vim.cmd.edit(open_path)
local buf = vim.api.nvim_get_current_buf()
vim.api.nvim_buf_set_lines(buf, 1, 2, false, { "two (unsaved)" })
vim.cmd("let &ul = &ul")
vim.api.nvim_win_set_cursor(0, { 3, 1 })

vim.cmd("Prigh")
T.wait(function()
  return agent.session.id
end, "session")
vim.api.nvim_buf_set_lines(chat.input, 0, -1, false, { "go" })
chat:send()
T.wait(function()
  return not agent.session.running
end, "turn end")
T.agent_log()

T.section("after the writes")
T.out("open.txt buffer: " .. vim.inspect(vim.api.nvim_buf_get_lines(buf, 0, -1, false)))
T.out("open.txt modified: " .. tostring(vim.bo[buf].modified))
T.out("open.txt disk: " .. vim.inspect(read(open_path)))
T.out("disk.txt: " .. vim.inspect(read(disk_path)))
T.out("sub/new.txt: " .. vim.inspect(read(T.tmp .. "/sub/new.txt")))
T.out("disk.txt loaded: " .. tostring(vim.fn.bufloaded(disk_path) == 1))
local cursor_win = vim.fn.bufwinid(buf)
T.out("cursor in open.txt: " .. vim.inspect(vim.api.nvim_win_get_cursor(cursor_win)))
T.chat()

T.section("one undo reverts the agent's write")
vim.api.nvim_buf_call(buf, function()
  vim.cmd("silent undo")
end)
T.out("open.txt buffer: " .. vim.inspect(vim.api.nvim_buf_get_lines(buf, 0, -1, false)))
T.finish()
