-- Against the real prigh: its edit and write tools go through
-- fs/read_text_file and fs/write_text_file into Neovim's buffers (an unsaved
-- one included), the cards show the diffs, :PrighReview keeps one hunk and
-- reverts the other.
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
local foo = T.tmp .. "/foo.txt"
write(foo, "alpha\nbeta\ngamma\ndelta\nepsilon\n")
local agent, chat = T.start_prigh({
  {
    text = "Editing.",
    tool_calls = {
      {
        id = "e1",
        name = "edit",
        arguments = {
          path = "foo.txt",
          edits = { { old_text = "beta", new_text = "BETA" }, { old_text = "epsilon", new_text = "epsilon\nzeta" } },
        },
      },
    },
  },
  { text = "And a new file.", tool_calls = { { id = "w1", name = "write", arguments = { path = "new.txt", content = "fresh\n" } } } },
  { text = "Edited." },
})

T.section("foo.txt open with an unsaved change")
vim.cmd.edit(foo)
local buf = vim.api.nvim_get_current_buf()
vim.api.nvim_buf_set_lines(buf, 0, 1, false, { "ALPHA (unsaved)" })
T.out("modified: " .. tostring(vim.bo[buf].modified), "disk: " .. vim.inspect(read(foo)))
T.open_prigh()
T.traffic_skip()

T.section("the turn: the agent reads the buffer, writes it")
vim.api.nvim_buf_set_lines(chat.input, 0, -1, false, { "edit foo" })
chat:send()
T.turn_end()
T.traffic(function(_, msg)
  return msg.method and msg.method:match("^fs/")
    or (msg.result ~= nil and msg.id ~= nil and msg.method == nil and type(msg.result) == "table" and not msg.result.stopReason)
    or (msg.method == "session/update" and msg.params.update.status == "completed")
end)
T.chat()
T.out("buffer: " .. vim.inspect(vim.api.nvim_buf_get_lines(buf, 0, -1, false)))
T.out("modified: " .. tostring(vim.bo[buf].modified), "disk: " .. vim.inspect(read(foo)))
T.out("new.txt: " .. vim.inspect(read(T.tmp .. "/new.txt")))

T.section(":PrighReview: the picker, foo.txt's hunks")
T.answer("foo.txt")
vim.cmd("PrighReview")
local rb = vim.api.nvim_get_current_buf()
T.out("reviewing " .. vim.api.nvim_buf_get_name(rb))
T.buffer(rb, require("prigh.review").ns)

T.section("keep the first hunk (ga), revert the second (gr)")
T.answer("new.txt")
T.feed("ga")
T.feed("gr")
T.out("buffer: " .. vim.inspect(vim.api.nvim_buf_get_lines(buf, 0, -1, false)))
T.out("modified: " .. tostring(vim.bo[buf].modified), "disk: " .. vim.inspect(read(foo)))

T.section("then new.txt: reject it all")
vim.wait(100, function()
  return false
end)
local nb = vim.api.nvim_get_current_buf()
T.out("reviewing " .. vim.api.nvim_buf_get_name(nb))
T.buffer(nb, require("prigh.review").ns)
T.feed("gR")
T.out("new.txt: " .. vim.inspect(read(T.tmp .. "/new.txt")), "buffer loaded: " .. tostring(vim.api.nvim_buf_is_valid(nb)))
vim.cmd("PrighReview")
T.finish()
