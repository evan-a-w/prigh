-- prigh.review on its own: hunks as extmarks, moving, accepting and
-- rejecting, single-undo application, discard, edits during the review and
-- a proposed text.
local T = require("helpers")
local review = require("prigh.review")

local before = { "a", "b", "c", "d", "e", "f", "g", "h" }
local after = { "a", "B", "c", "d", "e", "f", "g", "h", "i" }
table.remove(after, 5) -- e deleted

local function setup(lines)
  vim.cmd("enew!")
  local buf = vim.api.nvim_get_current_buf()
  vim.api.nvim_buf_set_lines(buf, 0, -1, false, lines)
  vim.cmd("let &ul = &ul")
  return buf
end
local function done(res)
  T.out("on_done: " .. T.json(res))
end
local function lines(buf)
  return table.concat(vim.api.nvim_buf_get_lines(buf, 0, -1, false), " ")
end

T.section("open: three hunks")
local buf = setup(after)
review.open({ buf = buf, before = before, title = "x.txt", on_done = done })
T.buffer(buf, review.ns)
T.out("cursor: " .. vim.api.nvim_win_get_cursor(0)[1], "b:prigh_review = " .. tostring(vim.b[buf].prigh_review))

T.section("]h ]h [h")
T.feed("]h")
T.out("cursor: " .. vim.api.nvim_win_get_cursor(0)[1])
T.feed("]h")
T.out("cursor: " .. vim.api.nvim_win_get_cursor(0)[1])
T.feed("[h")
T.out("cursor: " .. vim.api.nvim_win_get_cursor(0)[1])

T.section("ga on hunk 1, gr on hunk 2 (cursor moves on)")
T.feed("gg")
T.feed("ga")
T.out("cursor: " .. vim.api.nvim_win_get_cursor(0)[1])
T.feed("gr")
T.buffer(buf, review.ns)

T.section("ga on the last hunk applies everything")
T.feed("ga")
T.buffer(buf, review.ns)
T.out(
  "ga mapped: " .. tostring(vim.fn.maparg("ga", "n") ~= ""),
  "b:prigh_review = " .. tostring(vim.b[buf].prigh_review)
)
vim.cmd("silent undo")
T.out("after one undo: " .. lines(buf))

T.section("gR rejects all: back to the old text, one undo step")
buf = setup(after)
review.open({ buf = buf, before = before, on_done = done })
T.feed("gR")
T.out("buffer: " .. lines(buf))
vim.cmd("silent undo")
T.out("after one undo: " .. lines(buf))

T.section("gA accepts all")
buf = setup(after)
review.open({ buf = buf, before = before, on_done = done })
T.feed("gA")
T.out("buffer: " .. lines(buf))

T.section("q discards: no change, marks and keys gone")
buf = setup(after)
vim.keymap.set("n", "q", "<cmd>echo 'mine'<cr>", { buffer = buf, desc = "user q" })
review.open({ buf = buf, before = before, on_done = done })
T.feed("gr")
T.feed("q")
T.out("buffer: " .. lines(buf), "marks: " .. #vim.api.nvim_buf_get_extmarks(buf, review.ns, 0, -1, {}))
T.out("user's q restored: " .. vim.fn.maparg("q", "n", false, true).desc)

T.section("edits during the review re-anchor the hunks")
buf = setup(after)
review.open({ buf = buf, before = before, on_done = done })
T.feed("gr") -- hunk 1 (b -> B) rejected
vim.api.nvim_buf_set_lines(buf, 0, 0, false, { "inserted by the user" })
vim.api.nvim_exec_autocmds("TextChanged", { buffer = buf })
T.buffer(buf, review.ns)
T.section("insert mode pauses")
vim.api.nvim_exec_autocmds("TextChangedI", { buffer = buf })
T.out(review.get(buf):summary())
vim.api.nvim_exec_autocmds("InsertLeave", { buffer = buf })
T.out(review.get(buf):summary())
T.feed("gA")
T.out("buffer: " .. lines(buf))

T.section("proposed text: buffer has the old text, review shows the new")
buf = setup(before)
review.open({ buf = buf, before = before, after = after, on_done = done })
T.buffer(buf, review.ns)
T.feed("ga")
T.feed("gr")
T.feed("ga")
T.out("buffer: " .. lines(buf))
vim.cmd("silent undo")
T.out("after one undo: " .. lines(buf))

T.section("proposed text discarded")
buf = setup(before)
review.open({ buf = buf, before = before, after = after, on_done = done })
T.feed("<Esc>")
T.out("buffer: " .. lines(buf))

T.section("no changes")
buf = setup(before)
review.open({ buf = buf, before = before, on_done = done })
