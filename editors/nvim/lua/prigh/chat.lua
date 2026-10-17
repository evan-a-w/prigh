-- The chat view: a transcript buffer (filetype prigh, markdown highlighting)
-- with the prompt buffer below it. Renders prigh.session entries; owns the
-- keyboard for permission cards.
local config = require("prigh.config")
local render = require("prigh.render")
local status = require("prigh.status")
local context = require("prigh.context")
local complete = require("prigh.complete")

local M = {}

M.ns = vim.api.nvim_create_namespace("prigh.chat")

local C = {}
C.__index = C

local hl_links = {
  PrighUserHeading = "Title",
  PrighAgentHeading = "Special",
  PrighHeading = "Title",
  PrighThought = "Comment",
  PrighToolTitle = "Function",
  PrighMuted = "Comment",
  PrighPending = "Comment",
  PrighRunning = "DiagnosticWarn",
  PrighDone = "DiagnosticOk",
  PrighFailed = "DiagnosticError",
  PrighError = "DiagnosticError",
  PrighWarning = "DiagnosticWarn",
  PrighNotice = "DiagnosticInfo",
  PrighDiffAdd = "Added",
  PrighDiffDelete = "Removed",
  PrighDiffHunk = "Special",
  PrighDiffHeader = "Title",
  PrighLocation = "Directory",
  PrighPermission = "DiagnosticWarn",
  PrighPermissionDone = "Comment",
  PrighPermissionOption = "Normal",
  PrighKey = "Special",
  PrighStatus = "WinBar",
  PrighStatusModel = "WinBar",
  PrighWinbarTitle = "WinBar",
}

function M.setup_highlights()
  for k, v in pairs(hl_links) do
    vim.api.nvim_set_hl(0, k, { link = v, default = true })
  end
end

-- Every chat binding, for the mappings and for the help (g?).
M.chat_keys = {
  { "n", "q", "close", "close the chat" },
  { "n", "<Tab>", "toggle", "expand/collapse the card under the cursor" },
  { "n", "<CR>", "open", "open the location under the cursor" },
  { "n", "i", "insert", "write a prompt" },
  { "n", "a", "insert", "write a prompt" },
  { "n", "o", "insert", "write a prompt" },
  { "n", "<C-c>", "cancel", "cancel the turn (queued prompts return to the input)" },
  { "n", "g?", "help", "show these keys" },
}

M.input_keys = {
  { "n", "<CR>", "send", "send the prompt (queued while a turn runs)" },
  { { "n", "i" }, "<C-s>", "send", "send the prompt" },
  { { "n", "i" }, "<C-c>", "cancel", "cancel the turn" },
  { "n", "q", "close", "close the chat" },
  { "n", "g?", "help", "show these keys" },
  { "i", "@", "complete_file", "insert @ and complete a project file" },
  { "i", "/", "complete_command", "at the start: complete a slash command" },
}

M.permission_help = "a/A/r/R (or 1-9) choose · <CR> option under cursor · d full diff · <Esc> cancel"

function M.new(agent)
  M.setup_highlights()
  local self = setmetatable({
    agent = agent,
    attachments = {},
    blocks = {}, -- line count per entry
    footer_count = 0,
  }, C)
  self:_create_buffers()
  agent.session:on(function(kind, idx)
    self:_on_event(kind, idx)
  end)
  complete.ctx_provider = function()
    return { cwd = agent.cwd, commands = agent.session.commands }
  end
  return self
end

-- Buffers and windows --------------------------------------------------------

local function scratch(name, ft)
  local buf = vim.fn.bufnr(name)
  if buf ~= -1 then
    vim.api.nvim_buf_delete(buf, { force = true })
  end
  buf = vim.api.nvim_create_buf(false, true)
  vim.api.nvim_buf_set_name(buf, name)
  vim.bo[buf].bufhidden = "hide"
  vim.bo[buf].swapfile = false
  vim.bo[buf].filetype = ft
  pcall(vim.treesitter.language.register, "markdown", ft)
  pcall(vim.treesitter.start, buf, "markdown")
  return buf
end

function C:_create_buffers()
  self.buf = scratch("prigh://chat", "prigh")
  vim.bo[self.buf].modifiable = false
  self.input = scratch("prigh://input", "prigh_input")
  vim.bo[self.input].completefunc = "v:lua.require'prigh.complete'.func"
  vim.bo[self.input].omnifunc = "v:lua.require'prigh.complete'.func"
  self:_keymaps()
  self:_render_all()
end

function C:is_open()
  return self.win and vim.api.nvim_win_is_valid(self.win)
end

local function size(v, total)
  return v < 1 and math.floor(total * v) or v
end

function C:open()
  if self:is_open() then
    if not (self.input_win and vim.api.nvim_win_is_valid(self.input_win)) then
      self:_open_input()
    end
    return
  end
  local o = config.options.chat
  local cfg = { split = o.position, win = -1 }
  if o.position == "left" or o.position == "right" then
    cfg.width = size(o.width, vim.o.columns)
  else
    cfg.height = size(o.height, vim.o.lines)
  end
  self.win = vim.api.nvim_open_win(self.buf, true, cfg)
  local wo = vim.wo[self.win]
  wo.wrap = true
  wo.linebreak = true
  wo.number = false
  wo.relativenumber = false
  wo.signcolumn = "no"
  wo.foldcolumn = "0"
  wo.spell = false
  wo.winfixwidth = true
  self:_open_input()
  self:update_bars()
  self:_scroll_end()
end

function C:_open_input()
  self.input_win = vim.api.nvim_open_win(self.input, true, {
    split = "below",
    win = self.win,
    height = config.options.chat.input_height,
  })
  local wo = vim.wo[self.input_win]
  wo.wrap = true
  wo.linebreak = true
  wo.number = false
  wo.relativenumber = false
  wo.signcolumn = "no"
  wo.winfixheight = true
  self:update_bars()
end

function C:close()
  for _, w in ipairs({ self.input_win, self.win }) do
    if w and vim.api.nvim_win_is_valid(w) then
      if #vim.api.nvim_tabpage_list_wins(0) > 1 then
        vim.api.nvim_win_close(w, true)
      else
        vim.cmd("enew")
      end
    end
  end
  self.win, self.input_win = nil, nil
end

function C:toggle()
  if self:is_open() then
    self:close()
  else
    self:open()
    self:focus_input()
  end
end

function C:focus_input(insert)
  if not self:is_open() then
    self:open()
  end
  if self.input_win and vim.api.nvim_win_is_valid(self.input_win) then
    vim.api.nvim_set_current_win(self.input_win)
    if insert then
      vim.cmd("startinsert!")
    end
  end
end

-- Rendering ------------------------------------------------------------------

function C:_opts()
  local o = config.options
  return {
    tool_output_lines = o.chat.tool_output_lines,
    diff_max_lines = o.chat.diff_max_lines,
    thoughts_collapsed = o.chat.thoughts_collapsed,
    inline_diff_max = o.permission.inline_diff_max,
    cwd = self.agent.cwd,
  }
end

function C:_start_row(idx)
  local row = 0
  for i = 1, idx - 1 do
    row = row + (self.blocks[i] or 0)
  end
  return row
end

function C:_following()
  if not self:is_open() then
    return true
  end
  if vim.api.nvim_get_current_win() ~= self.win then
    return true
  end
  local cur = vim.api.nvim_win_get_cursor(self.win)[1]
  return cur >= vim.api.nvim_buf_line_count(self.buf) - 1
end

function C:_scroll_end()
  if self:is_open() then
    local n = vim.api.nvim_buf_line_count(self.buf)
    vim.api.nvim_win_set_cursor(self.win, { n, 0 })
  end
end

function C:_put(start, old_count, out)
  local buf = self.buf
  vim.bo[buf].modifiable = true
  vim.api.nvim_buf_clear_namespace(buf, M.ns, start, start + old_count)
  vim.api.nvim_buf_set_lines(buf, start, start + old_count, false, out.lines)
  vim.bo[buf].modifiable = false
  for _, h in ipairs(out.hls) do
    local row = start + h.row
    if h.col_start then
      vim.api.nvim_buf_set_extmark(buf, M.ns, row, h.col_start, {
        end_col = math.min(h.col_end, #out.lines[h.row + 1]),
        hl_group = h.group,
        priority = 210,
      })
    else
      vim.api.nvim_buf_set_extmark(buf, M.ns, row, 0, { line_hl_group = h.group, priority = 200 })
    end
  end
end

function C:_render_entry(idx)
  local entries = self.agent.session.entries
  local e = entries[idx]
  local out = render.entry(e, entries[idx - 1], self:_opts())
  local start = self:_start_row(idx)
  local old = self.blocks[idx] or 0
  self:_put(start, old, out)
  self.blocks[idx] = #out.lines
end

function C:_render_footer()
  local out = render.footer(self.agent.session.queue)
  local start = self:_start_row(#self.blocks + 1)
  self:_put(start, self.footer_count, out)
  self.footer_count = #out.lines
  -- A buffer always has a line: drop the placeholder once there is content.
  local want = start + #out.lines
  if want > 0 and vim.api.nvim_buf_line_count(self.buf) > want then
    vim.bo[self.buf].modifiable = true
    vim.api.nvim_buf_set_lines(self.buf, want, -1, false, {})
    vim.bo[self.buf].modifiable = false
  end
end

function C:_render_all()
  vim.bo[self.buf].modifiable = true
  vim.api.nvim_buf_clear_namespace(self.buf, M.ns, 0, -1)
  vim.api.nvim_buf_set_lines(self.buf, 0, -1, false, {})
  vim.bo[self.buf].modifiable = false
  self.blocks = {}
  self.footer_count = 1 -- the empty buffer's line
  for i = 1, #self.agent.session.entries do
    self:_render_entry(i)
  end
  self:_render_footer()
end

function C:_on_event(kind, idx)
  if not vim.api.nvim_buf_is_valid(self.buf) then
    return
  end
  local follow = self:_following()
  if self.permission_entry and self.permission_entry.status ~= "pending" then
    self:_end_permission()
  end
  if kind == "reset" then
    self:_render_all()
  elseif kind == "entry" then
    -- Entries before idx that were never rendered first (keeps rows right).
    for i = #self.blocks + 1, idx - 1 do
      self:_render_entry(i)
    end
    self:_render_entry(idx)
  end
  self:_render_footer()
  self:update_bars()
  if follow then
    self:_scroll_end()
  end
end

function C:input_bar()
  local parts = { "%#PrighMuted#<CR> send · <C-c> cancel · @ file · / command · g? keys" }
  for _, a in ipairs(self.attachments) do
    table.insert(parts, "%#PrighLocation#📎 " .. a.label:gsub("%%", "%%%%"))
  end
  local q = #self.agent.session.queue
  if q > 0 then
    table.insert(parts, string.format("%%#PrighWarning#%d queued", q))
  end
  return table.concat(parts, "%#PrighMuted# · ")
end

function C:update_bars()
  if self:is_open() then
    vim.wo[self.win].winbar = status.winbar(self.agent)
  end
  if self.input_win and vim.api.nvim_win_is_valid(self.input_win) then
    vim.wo[self.input_win].winbar = self:input_bar()
  end
end

--- The entry at chat row `row` (0-based) and the row within it.
function C:entry_at(row)
  local start = 0
  for i, n in ipairs(self.blocks) do
    if row < start + n then
      return self.agent.session.entries[i], row - start
    end
    start = start + n
  end
end

-- Actions --------------------------------------------------------------------

function C:toggle_at_cursor()
  local row = vim.api.nvim_win_get_cursor(0)[1] - 1
  local e = self:entry_at(row)
  if not e then
    return
  end
  if e.kind == "thought" then
    local collapsed = e.expanded == nil and config.options.chat.thoughts_collapsed or e.expanded == false
    e.expanded = collapsed
  else
    e.expanded = not e.expanded
  end
  self:_render_entry(e.index)
  self:_render_footer()
end

function C:open_at_cursor()
  local line = vim.api.nvim_get_current_line()
  local path, lnum = line:match("^%s*↳ (.-):(%d+)$")
  if not path then
    path = line:match("^%s*↳ (.-)$")
  end
  if not path then
    return
  end
  path = vim.fn.expand(path)
  if path:sub(1, 1) ~= "/" then
    path = self.agent.cwd .. "/" .. path
  end
  local target
  for _, w in ipairs(vim.api.nvim_tabpage_list_wins(0)) do
    if w ~= self.win and w ~= self.input_win and vim.api.nvim_win_get_config(w).relative == "" then
      target = w
      break
    end
  end
  if target then
    vim.api.nvim_set_current_win(target)
  else
    vim.cmd("leftabove vsplit")
  end
  vim.cmd.edit(vim.fn.fnameescape(path))
  if lnum then
    pcall(vim.api.nvim_win_set_cursor, 0, { tonumber(lnum), 0 })
  end
end

function C:prompt_text()
  return table.concat(vim.api.nvim_buf_get_lines(self.input, 0, -1, false), "\n")
end

function C:send()
  local text = vim.trim(self:prompt_text())
  if text == "" and #self.attachments == 0 then
    vim.notify("prigh: nothing to send: type a prompt first", vim.log.levels.WARN)
    return
  end
  local session = self.agent.session
  local cmd = text:match("^/([%w_%-%.:]+)")
  if cmd and #session.commands > 0 and not text:match("^/[^%s]*/") then
    local known = false
    local names = {}
    for _, c in ipairs(session.commands) do
      known = known or c.name == cmd
      table.insert(names, c.name)
    end
    if not known then
      local close = vim.fn.matchfuzzy(names, cmd, { limit = 3 })
      vim.notify(
        string.format(
          "prigh: unknown command /%s%s (type / for the list)",
          cmd,
          #close > 0 and (": did you mean /" .. table.concat(close, ", /") .. "?") or ""
        ),
        vim.log.levels.WARN
      )
      return
    end
  end
  local blocks = {}
  if text ~= "" then
    table.insert(blocks, { type = "text", text = text })
  end
  vim.list_extend(blocks, context.mentions(text, self.agent.cwd))
  local display = text
  for _, a in ipairs(self.attachments) do
    table.insert(blocks, a.block)
    display = display .. (display == "" and "" or "\n") .. "📎 " .. a.label
  end
  self.attachments = {}
  vim.api.nvim_buf_set_lines(self.input, 0, -1, false, {})
  self:update_bars()
  self.agent:prompt(blocks, display)
end

function C:attach(a)
  table.insert(self.attachments, a)
  self:update_bars()
  vim.notify("prigh: attached " .. a.label .. " (sent with the next prompt)")
end

function C:clear_attachments()
  self.attachments = {}
  self:update_bars()
end

--- Give queued prompts back to the input after a cancel.
function C:unqueue(prompts)
  local lines = vim.api.nvim_buf_get_lines(self.input, 0, -1, false)
  if #lines == 1 and lines[1] == "" then
    lines = {}
  end
  for _, p in ipairs(prompts) do
    vim.list_extend(lines, vim.split(p.display, "\n", { plain = true }))
  end
  vim.api.nvim_buf_set_lines(self.input, 0, -1, false, lines)
  vim.notify(string.format("prigh: %d queued prompt%s moved back to the input", #prompts, #prompts == 1 and "" or "s"))
end

function C:help()
  local lines = { "Chat window" }
  local function add(keys)
    for _, k in ipairs(keys) do
      local modes = type(k[1]) == "table" and table.concat(k[1], "") or k[1]
      table.insert(lines, string.format("  %-3s %-8s %s", modes, k[2], k[4]))
    end
  end
  add(M.chat_keys)
  table.insert(lines, "Prompt")
  add(M.input_keys)
  table.insert(lines, "Permission cards")
  table.insert(lines, "  " .. M.permission_help)
  table.insert(lines, "Press q or <Esc> to close")
  local buf = vim.api.nvim_create_buf(false, true)
  vim.api.nvim_buf_set_lines(buf, 0, -1, false, lines)
  vim.bo[buf].modifiable = false
  local width = 0
  for _, l in ipairs(lines) do
    width = math.max(width, vim.fn.strdisplaywidth(l))
  end
  local win = vim.api.nvim_open_win(buf, true, {
    relative = "editor",
    width = math.min(width + 2, vim.o.columns - 4),
    height = #lines,
    row = math.floor((vim.o.lines - #lines) / 2),
    col = math.floor((vim.o.columns - width) / 2),
    border = "rounded",
    title = " prigh keys ",
  })
  for _, k in ipairs({ "q", "<Esc>" }) do
    vim.keymap.set("n", k, function()
      vim.api.nvim_win_close(win, true)
    end, { buffer = buf, nowait = true })
  end
  return buf
end

-- Keymaps --------------------------------------------------------------------

function C:_action(name)
  local actions = {
    close = function()
      self:close()
    end,
    toggle = function()
      self:toggle_at_cursor()
    end,
    open = function()
      self:open_at_cursor()
    end,
    insert = function()
      self:focus_input(true)
    end,
    cancel = function()
      self.agent:cancel()
    end,
    help = function()
      self:help()
    end,
    send = function()
      vim.cmd("stopinsert")
      self:send()
    end,
    complete_file = function()
      local col = vim.api.nvim_win_get_cursor(0)[2]
      local prev = vim.api.nvim_get_current_line():sub(col, col)
      if col == 0 or prev:match("%s") then
        return "@<C-x><C-u>"
      end
      return "@"
    end,
    complete_command = function()
      local col = vim.api.nvim_win_get_cursor(0)[2]
      local row = vim.api.nvim_win_get_cursor(0)[1]
      if row == 1 and col == 0 then
        return "/<C-x><C-u>"
      end
      return "/"
    end,
  }
  return actions[name]
end

function C:_keymaps()
  local function set(buf, keys)
    for _, k in ipairs(keys) do
      local expr = k[3] == "complete_file" or k[3] == "complete_command"
      vim.keymap.set(
        k[1],
        k[2],
        self:_action(k[3]),
        { buffer = buf, nowait = true, expr = expr, desc = "prigh: " .. k[4] }
      )
    end
  end
  set(self.buf, M.chat_keys)
  set(self.input, M.input_keys)
end

-- Permission cards -------------------------------------------------------------

function C:_permission_maps(entry)
  local agent = self.agent
  local maps = {}
  local function answer(id)
    return function()
      self:_end_permission()
      agent:answer_permission(entry, id)
    end
  end
  for i, ko in ipairs(render.permission_option_keys(entry.options)) do
    maps[ko.key] = answer(ko.option.optionId)
    maps[tostring(i)] = maps[tostring(i)] or answer(ko.option.optionId)
  end
  maps["<Esc>"] = answer(nil)
  maps["<C-c>"] = function()
    self:_end_permission()
    agent:cancel()
  end
  maps["<CR>"] = function()
    local row = vim.api.nvim_win_get_cursor(0)[1] - 1
    local e, rel = self:entry_at(row)
    local opt = e == entry and entry.option_rows and entry.option_rows[rel + 1]
    if not opt then
      vim.notify("prigh: move the cursor to an option, or press " .. M.permission_help, vim.log.levels.WARN)
      return
    end
    answer(opt.optionId)()
  end
  maps["d"] = function()
    self:diff_float(entry)
  end
  local blocked = function()
    vim.notify("prigh: answer the permission request first: " .. M.permission_help, vim.log.levels.WARN)
  end
  for _, k in ipairs({ "q", "i", "a", "o", "<Tab>", "<C-s>", "g?" }) do
    maps[k] = maps[k] or blocked
  end
  return maps
end

function C:permission(entry)
  self:open()
  vim.cmd("stopinsert")
  self.permission_entry = entry
  local maps = self:_permission_maps(entry)
  self.permission_lhs = {}
  for _, buf in ipairs({ self.buf, self.input }) do
    for lhs, fn in pairs(maps) do
      vim.keymap.set("n", lhs, fn, { buffer = buf, nowait = true, desc = "prigh: permission" })
    end
  end
  for lhs in pairs(maps) do
    table.insert(self.permission_lhs, lhs)
  end
  vim.api.nvim_set_current_win(self.win)
  local start = self:_start_row(entry.index)
  local first_opt
  for rel in pairs(entry.option_rows or {}) do
    first_opt = math.min(first_opt or rel, rel)
  end
  vim.api.nvim_win_set_cursor(self.win, { start + (first_opt or 1), 0 })
end

function C:_end_permission()
  if not self.permission_lhs then
    return
  end
  for _, buf in ipairs({ self.buf, self.input }) do
    for _, lhs in ipairs(self.permission_lhs) do
      pcall(vim.keymap.del, "n", lhs, { buffer = buf })
    end
  end
  self.permission_lhs = nil
  self.permission_entry = nil
  self:_keymaps()
end

--- The permission card's full diff in a float.
function C:diff_float(entry)
  local lines = entry.diff_lines or {}
  if #lines == 0 then
    vim.notify("prigh: this request has no diff", vim.log.levels.INFO)
    return
  end
  local buf = vim.api.nvim_create_buf(false, true)
  vim.api.nvim_buf_set_lines(buf, 0, -1, false, lines)
  vim.bo[buf].filetype = "diff"
  vim.bo[buf].modifiable = false
  local w = math.floor(vim.o.columns * 0.8)
  local h = math.min(#lines, math.floor(vim.o.lines * 0.8))
  local win = vim.api.nvim_open_win(buf, true, {
    relative = "editor",
    width = w,
    height = h,
    row = math.floor((vim.o.lines - h) / 2),
    col = math.floor((vim.o.columns - w) / 2),
    border = "rounded",
    title = " " .. (entry.title or "diff") .. " · q close ",
  })
  for _, k in ipairs({ "q", "<Esc>" }) do
    vim.keymap.set("n", k, function()
      vim.api.nvim_win_close(win, true)
    end, { buffer = buf, nowait = true })
  end
  return buf
end

return M
