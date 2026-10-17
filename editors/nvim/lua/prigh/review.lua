-- Hunk review of a change in a buffer, independent of ACP.
--
-- The buffer holds the new text while reviewing; the old text of each hunk is
-- shown as virtual lines above it and the new lines are highlighted. Decisions
-- (ga/gr, gA/gR) are only marks until every hunk is decided: then the rejected
-- hunks are reverted in one batch, as a single undo step. q/<Esc> leaves the
-- buffer as it was before the review started. Edits to the buffer during the
-- review re-diff against the old text, keeping decisions on unchanged hunks;
-- insert mode pauses the review until InsertLeave.
--
--   local r = require("prigh.review").open({
--     buf = buf,
--     before = old_lines,      -- the base text
--     after = new_lines,       -- optional: proposed text to put in the buffer
--     title = "src/foo.ml",
--     on_done = function(result) end, -- { status = "applied"|"discarded", accepted, rejected }
--   })
local diff = require("prigh.diff")

local M = {}

M.ns = vim.api.nvim_create_namespace("prigh.review")

M.keys = {
  next = "]h",
  prev = "[h",
  accept = "ga",
  reject = "gr",
  accept_all = "gA",
  reject_all = "gR",
  discard = { "q", "<Esc>" },
}

local active = {} -- buf -> review

local R = {}
R.__index = R

local function set_hl()
  local links = {
    PrighReviewAdd = "DiffAdd",
    PrighReviewDelete = "DiffDelete",
    PrighReviewAccepted = "Comment",
    PrighReviewRejected = "DiffDelete",
    PrighReviewHeader = "Title",
  }
  for k, v in pairs(links) do
    vim.api.nvim_set_hl(0, k, { link = v, default = true })
  end
end

local function key_of(h)
  return h.a_start .. "\0" .. table.concat(h.a_lines, "\n") .. "\0" .. table.concat(h.b_lines, "\n")
end

function M.get(buf)
  return active[buf or vim.api.nvim_get_current_buf()]
end

function M.open(opts)
  set_hl()
  local buf = opts.buf
  if active[buf] then
    active[buf]:discard()
  end
  local self = setmetatable({
    buf = buf,
    before = opts.before or {},
    title = opts.title or vim.api.nvim_buf_get_name(buf),
    on_done = opts.on_done,
    decisions = {},
    hunks = {},
    paused = false,
    saved_maps = {},
    augroup = vim.api.nvim_create_augroup("prigh.review." .. buf, { clear = true }),
  }, R)
  self.original = vim.api.nvim_buf_get_lines(buf, 0, -1, false)
  if opts.after then
    local cur = self.original
    if table.concat(cur, "\n") ~= table.concat(opts.after, "\n") then
      require("prigh.fs").set_buffer_lines(buf, opts.after)
      self.joined = true
    end
  end
  active[buf] = self
  vim.b[buf].prigh_review = true
  self:_map()
  vim.api.nvim_create_autocmd({ "TextChanged" }, {
    group = self.augroup,
    buffer = buf,
    callback = function()
      self.paused = false
      self:refresh()
    end,
  })
  vim.api.nvim_create_autocmd("TextChangedI", {
    group = self.augroup,
    buffer = buf,
    callback = function()
      self.paused = true
      self:render()
    end,
  })
  vim.api.nvim_create_autocmd("InsertLeave", {
    group = self.augroup,
    buffer = buf,
    callback = function()
      if self.paused then
        self.paused = false
        self:refresh()
      end
    end,
  })
  vim.api.nvim_create_autocmd({ "BufUnload", "BufWipeout" }, {
    group = self.augroup,
    buffer = buf,
    callback = function()
      self:_close("discarded")
    end,
  })
  self:refresh()
  if #self.hunks == 0 then
    self:_close("applied")
    return self
  end
  self:goto(1)
  return self
end

function R:refresh()
  local cur = vim.api.nvim_buf_get_lines(self.buf, 0, -1, false)
  self.hunks = diff.hunks(self.before, cur)
  for _, h in ipairs(self.hunks) do
    h.decision = self.decisions[key_of(h)]
  end
  self:render()
end

function R:counts()
  local acc, rej, und = 0, 0, 0
  for _, h in ipairs(self.hunks) do
    if h.decision == "accept" then
      acc = acc + 1
    elseif h.decision == "reject" then
      rej = rej + 1
    else
      und = und + 1
    end
  end
  return acc, rej, und
end

function R:summary()
  local acc, rej, und = self:counts()
  local s = string.format(
    "review %s: %d hunk%s, %d accepted, %d rejected, %d left · ]h/[h move · ga/gr accept/reject · gA/gR all · q discard",
    self.title ~= "" and vim.fn.fnamemodify(self.title, ":~:.") or "[No Name]",
    #self.hunks,
    #self.hunks == 1 and "" or "s",
    acc,
    rej,
    und
  )
  if self.paused then
    s = s .. " · paused while inserting"
  end
  return s
end

function R:render()
  local buf = self.buf
  vim.api.nvim_buf_clear_namespace(buf, M.ns, 0, -1)
  local nlines = vim.api.nvim_buf_line_count(buf)
  vim.api.nvim_buf_set_extmark(buf, M.ns, 0, 0, {
    virt_lines = { { { self:summary(), "PrighReviewHeader" } } },
    virt_lines_above = true,
  })
  for i, h in ipairs(self.hunks) do
    local add_hl, del_hl, sign = "PrighReviewAdd", "PrighReviewDelete", "+"
    local label = "ga accept · gr reject"
    if h.decision == "accept" then
      add_hl, del_hl, sign, label = nil, "PrighReviewAccepted", "✓", "accepted"
    elseif h.decision == "reject" then
      add_hl, del_hl, sign, label = "PrighReviewRejected", "PrighReviewAdd", "✗", "rejected"
    end
    local virt = {}
    for _, l in ipairs(h.a_lines) do
      table.insert(virt, { { l, del_hl } })
    end
    local header = { string.format("hunk %d/%d: %s", i, #self.hunks, label), "PrighReviewHeader" }
    local anchor, above = h.b_start, true
    if anchor >= nlines then
      anchor, above = math.max(nlines - 1, 0), false
    end
    if #virt > 0 then
      vim.api.nvim_buf_set_extmark(buf, M.ns, anchor, 0, {
        virt_lines = virt,
        virt_lines_above = above,
      })
    end
    vim.api.nvim_buf_set_extmark(buf, M.ns, anchor, 0, {
      virt_text = { header },
      virt_text_pos = "eol",
      sign_text = #h.b_lines == 0 and "-" or nil,
      sign_hl_group = del_hl,
    })
    for j = 0, #h.b_lines - 1 do
      vim.api.nvim_buf_set_extmark(buf, M.ns, h.b_start + j, 0, {
        line_hl_group = add_hl,
        sign_text = sign,
        sign_hl_group = add_hl or "PrighReviewAccepted",
      })
    end
  end
end

--- The hunk under the cursor (or the next one after it).
function R:current()
  local win = vim.fn.bufwinid(self.buf)
  local row = (win ~= -1 and vim.api.nvim_win_get_cursor(win)[1] or 1) - 1
  for i, h in ipairs(self.hunks) do
    local last = h.b_start + math.max(#h.b_lines, 1) - 1
    if row <= last then
      return i
    end
  end
  return #self.hunks > 0 and #self.hunks or nil
end

function R:goto(i)
  local h = self.hunks[i]
  local win = vim.fn.bufwinid(self.buf)
  if not h or win == -1 then
    return
  end
  local row = math.min(h.b_start, vim.api.nvim_buf_line_count(self.buf) - 1)
  vim.api.nvim_win_set_cursor(win, { row + 1, 0 })
end

function R:move(delta)
  local win = vim.fn.bufwinid(self.buf)
  if win == -1 or #self.hunks == 0 then
    return
  end
  local row = vim.api.nvim_win_get_cursor(win)[1] - 1
  if delta > 0 then
    for i, h in ipairs(self.hunks) do
      if h.b_start > row then
        return self:goto(i)
      end
    end
    self:goto(1)
  else
    for i = #self.hunks, 1, -1 do
      if self.hunks[i].b_start < row then
        return self:goto(i)
      end
    end
    self:goto(#self.hunks)
  end
end

function R:decide(i, decision)
  local h = self.hunks[i]
  if not h then
    return
  end
  h.decision = decision
  self.decisions[key_of(h)] = decision
  local _, _, und = self:counts()
  if und == 0 then
    return self:apply()
  end
  self:render()
  -- Move on to the next undecided hunk.
  for k = 1, #self.hunks do
    local j = (i + k - 1) % #self.hunks + 1
    if not self.hunks[j].decision then
      return self:goto(j)
    end
  end
end

function R:accept(i)
  self:decide(i or self:current(), "accept")
end

function R:reject(i)
  self:decide(i or self:current(), "reject")
end

function R:decide_all(decision)
  for _, h in ipairs(self.hunks) do
    if not h.decision then
      h.decision = decision
      self.decisions[key_of(h)] = decision
    end
  end
  self:apply()
end

--- Revert the rejected hunks in one undo step and close.
function R:apply()
  local rejected = {}
  for _, h in ipairs(self.hunks) do
    if h.decision == "reject" then
      table.insert(rejected, h)
    end
  end
  local buf = self.buf
  vim.api.nvim_clear_autocmds({ group = self.augroup })
  if #rejected > 0 then
    vim.api.nvim_buf_call(buf, function()
      if not self.joined then
        vim.o.undolevels = vim.o.undolevels
      end
      for i = #rejected, 1, -1 do
        local h = rejected[i]
        if self.joined or i < #rejected then
          vim.cmd("silent! undojoin")
        end
        vim.api.nvim_buf_set_lines(buf, h.b_start, h.b_start + #h.b_lines, false, h.a_lines)
      end
    end)
  end
  self:_close("applied", #self.hunks - #rejected, #rejected)
end

--- Leave the buffer as it was before the review.
function R:discard()
  if self.joined and vim.api.nvim_buf_is_valid(self.buf) then
    vim.api.nvim_clear_autocmds({ group = self.augroup })
    require("prigh.fs").set_buffer_lines(self.buf, self.original, true)
  end
  self:_close("discarded")
end

function R:_close(status, accepted, rejected)
  if self.closed then
    return
  end
  self.closed = true
  active[self.buf] = nil
  pcall(vim.api.nvim_del_augroup_by_id, self.augroup)
  if vim.api.nvim_buf_is_valid(self.buf) then
    vim.api.nvim_buf_clear_namespace(self.buf, M.ns, 0, -1)
    vim.b[self.buf].prigh_review = nil
    self:_unmap()
  end
  if self.on_done then
    self.on_done({ status = status, accepted = accepted or 0, rejected = rejected or 0 })
  end
end

function R:_map()
  local buf = self.buf
  local function map(lhs, fn, desc)
    local old = vim.fn.maparg(lhs, "n", false, true)
    if old and old.buffer == 1 then
      table.insert(self.saved_maps, old)
    end
    table.insert(self.saved_maps, { lhs = lhs, ours = true })
    vim.keymap.set("n", lhs, fn, { buffer = buf, nowait = true, desc = "prigh review: " .. desc })
  end
  local function each(lhs, fn, desc)
    for _, l in ipairs(type(lhs) == "table" and lhs or { lhs }) do
      map(l, fn, desc)
    end
  end
  each(M.keys.next, function()
    self:move(1)
  end, "next hunk")
  each(M.keys.prev, function()
    self:move(-1)
  end, "previous hunk")
  each(M.keys.accept, function()
    self:accept()
  end, "accept hunk")
  each(M.keys.reject, function()
    self:reject()
  end, "reject hunk")
  each(M.keys.accept_all, function()
    self:decide_all("accept")
  end, "accept all remaining hunks")
  each(M.keys.reject_all, function()
    self:decide_all("reject")
  end, "reject all remaining hunks")
  each(M.keys.discard, function()
    self:discard()
  end, "discard the review")
end

function R:_unmap()
  for _, m in ipairs(self.saved_maps) do
    if m.ours then
      pcall(vim.keymap.del, "n", m.lhs, { buffer = self.buf })
    end
  end
  for _, m in ipairs(self.saved_maps) do
    if not m.ours then
      vim.api.nvim_buf_call(self.buf, function()
        vim.fn.mapset("n", false, m)
      end)
    end
  end
  self.saved_maps = {}
end

return M
