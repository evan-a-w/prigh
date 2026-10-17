-- Session entries -> lines and highlights for the chat buffer. Pure: no
-- buffer access, so it is easy to test and to port.
local diff = require("prigh.diff")

local M = {}

M.status_icon = {
  pending = "○",
  in_progress = "◐",
  completed = "✓",
  failed = "✗",
}

M.status_hl = {
  pending = "PrighPending",
  in_progress = "PrighRunning",
  completed = "PrighDone",
  failed = "PrighFailed",
}

local severity_hl = { error = "PrighError", warning = "PrighWarning" }
local severity_icon = { error = "✗", warning = "⚠", info = "ℹ" }

-- Keys for permission options, by kind; anything else gets a digit.
M.permission_keys = {
  allow_once = "a",
  allow_always = "A",
  reject_once = "r",
  reject_always = "R",
}

local Out = {}
Out.__index = Out

local function new_out()
  return setmetatable({ lines = {}, hls = {} }, Out)
end

function Out:line(text, hl)
  -- Buffer lines cannot contain newlines.
  text = (text or ""):gsub("\n", " ")
  table.insert(self.lines, text)
  if hl then
    table.insert(self.hls, { row = #self.lines - 1, group = hl })
  end
end

function Out:span(col_start, col_end, hl)
  table.insert(self.hls, { row = #self.lines - 1, col_start = col_start, col_end = col_end, group = hl })
end

function Out:text(text, hl, indent)
  for _, l in ipairs(vim.split(text or "", "\n", { plain = true })) do
    self:line((indent or "") .. l, hl)
  end
end

--- `path` relative to `cwd` (the session's directory) when inside it.
function M.rel(path, cwd)
  if cwd and path:sub(1, #cwd + 1) == cwd .. "/" then
    return path:sub(#cwd + 2)
  end
  return vim.fn.fnamemodify(path, ":~:.")
end

local function trim_trailing_blank(text)
  return (text:gsub("%s+$", ""))
end

function M.unified_diff_lines(d, cwd)
  local old = d.oldText
  local lines = diff.unified(old or "", d.newText or "")
  local path = d.path and M.rel(d.path, cwd) or "file"
  table.insert(lines, 1, (old == nil and "+++ new file " or "+++ ") .. path)
  return lines
end

local function diff_block(out, lines, max, expanded, indent)
  out:line(indent .. "```diff")
  local shown = expanded and #lines or math.min(#lines, max)
  for i = 1, shown do
    local l = lines[i]
    local hl
    local c = l:sub(1, 1)
    if l:sub(1, 3) == "+++" or l:sub(1, 3) == "---" then
      hl = "PrighDiffHeader"
    elseif c == "+" then
      hl = "PrighDiffAdd"
    elseif c == "-" then
      hl = "PrighDiffDelete"
    elseif c == "@" then
      hl = "PrighDiffHunk"
    end
    out:line(indent .. l, hl)
  end
  out:line(indent .. "```")
  if shown < #lines then
    out:line(string.format("%s▸ %d more diff lines · <Tab> expand", indent, #lines - shown), "PrighMuted")
  end
end

local function tool_content(out, e, opts)
  local text_lines = {}
  for _, c in ipairs(e.content or {}) do
    if c.type == "diff" then
      diff_block(out, M.unified_diff_lines(c, opts.cwd), opts.diff_max_lines, e.expanded, "  ")
    elseif c.type == "terminal" then
      out:line("  [terminal " .. tostring(c.terminalId) .. "]", "PrighMuted")
    elseif c.type == "content" and c.content then
      local t = trim_trailing_blank(require("prigh.session").block_text(c.content))
      if t ~= "" then
        vim.list_extend(text_lines, vim.split(t, "\n", { plain = true }))
      end
    end
  end
  if #text_lines == 0 then
    return
  end
  local shown = e.expanded and #text_lines or math.min(#text_lines, opts.tool_output_lines)
  if shown > 0 then
    out:line("  ```")
    for i = 1, shown do
      out:line("  " .. text_lines[i])
    end
    out:line("  ```")
  end
  if shown < #text_lines then
    local more = #text_lines - shown
    out:line(
      string.format(
        "  ▸ %d %sline%s of output · <Tab> expand",
        more,
        shown > 0 and "more " or "",
        more == 1 and "" or "s"
      ),
      "PrighMuted"
    )
  end
end

local function locations(out, e, cwd)
  for _, loc in ipairs(e.locations or {}) do
    local p = M.rel(loc.path, cwd)
    out:line("  ↳ " .. p .. (loc.line and (":" .. loc.line) or ""), "PrighLocation")
  end
end

local function tool(out, e, opts)
  local status = e.status or "pending"
  local icon = M.status_icon[status] or "•"
  local head = string.format("%s %s", icon, e.title or "tool")
  local meta = string.format(" · %s%s", e.tool_kind and (e.tool_kind .. " · ") or "", status:gsub("_", " "))
  out:line(head .. meta, "PrighToolTitle")
  out:span(0, #icon, M.status_hl[status] or "PrighMuted")
  out:span(#head, #head + #meta, "PrighMuted")
  locations(out, e, opts.cwd)
  tool_content(out, e, opts)
end

local function plan(out, e)
  out:line("Plan", "PrighHeading")
  for _, item in ipairs(e.items or {}) do
    local icon = M.status_icon[item.status] or "○"
    out:line(string.format("  %s %s", icon, item.content or ""), item.status == "completed" and "PrighMuted" or nil)
  end
end

local function thought(out, e, opts)
  local text = trim_trailing_blank(e.text or "")
  local lines = vim.split(text, "\n", { plain = true })
  local collapsed = e.expanded == nil and opts.thoughts_collapsed or e.expanded == false
  if collapsed then
    local first = lines[1] or ""
    if #first > 60 then
      first = vim.fn.strcharpart(first, 0, 60) .. "…"
    end
    out:line(string.format("▸ Thinking: %s (%d line%s · <Tab> expand)", first, #lines, #lines == 1 and "" or "s"), "PrighThought")
  else
    out:line("▾ Thinking", "PrighThought")
    for _, l in ipairs(lines) do
      out:line("  " .. l, "PrighThought")
    end
  end
end

--- Key -> option for a permission request.
function M.permission_option_keys(options)
  local used, out = {}, {}
  for i, o in ipairs(options or {}) do
    local k = M.permission_keys[o.kind]
    if not k or used[k] then
      k = tostring(i)
    end
    used[k] = true
    table.insert(out, { key = k, option = o })
  end
  return out
end

local function permission(out, e, opts)
  local title = e.title or "tool call"
  if e.status ~= "pending" then
    local outcome = e.status == "cancelled" and "cancelled" or (e.selected_name or e.selected or "?")
    out:line(string.format("🔐 Permission: %s → %s", title, outcome), "PrighPermissionDone")
    return
  end
  out:line("🔐 Permission needed: " .. title, "PrighPermission")
  locations(out, e, opts.cwd)
  if e.diff_lines and #e.diff_lines > 0 then
    diff_block(out, e.diff_lines, opts.inline_diff_max, false, "  ")
    if #e.diff_lines > opts.inline_diff_max then
      out.lines[#out.lines] = string.format("  ▸ %d more diff lines · d full diff", #e.diff_lines - opts.inline_diff_max)
    end
  end
  e.option_rows = {}
  for _, ko in ipairs(M.permission_option_keys(e.options)) do
    out:line(string.format("  [%s] %s", ko.key, ko.option.name), "PrighPermissionOption")
    out:span(2, 4 + #ko.key, "PrighKey")
    e.option_rows[#out.lines] = ko.option
  end
  out:line("  [Esc] cancel · <CR> option under cursor · j/k move", "PrighMuted")
end

--- Render entry `e`; `prev` is the entry before it (for headings).
function M.entry(e, prev, opts)
  local out = new_out()
  if e.kind == "user" then
    out:line("## You", "PrighUserHeading")
    out:text(trim_trailing_blank(e.text or ""))
  elseif e.kind == "agent" then
    if not prev or prev.kind == "user" then
      out:line("## prigh", "PrighAgentHeading")
    end
    out:text(trim_trailing_blank(e.text or ""))
  elseif e.kind == "thought" then
    if not prev or prev.kind == "user" then
      out:line("## prigh", "PrighAgentHeading")
    end
    thought(out, e, opts)
  elseif e.kind == "tool" then
    tool(out, e, opts)
  elseif e.kind == "plan" then
    plan(out, e)
  elseif e.kind == "notice" then
    local icon = severity_icon[e.severity] or "•"
    local t = icon .. " " .. (e.title or "")
    if e.description and e.description ~= "" then
      t = t .. " — " .. e.description
    end
    out:text(t, severity_hl[e.severity] or "PrighNotice")
  elseif e.kind == "permission" then
    permission(out, e, opts)
  end
  out:line("")
  return out
end

--- Lines after the last entry: queued prompts.
function M.footer(queue)
  local out = new_out()
  for _, q in ipairs(queue or {}) do
    local first = vim.split(q.display or "", "\n", { plain = true })[1]
    out:line("⏸ queued: " .. first, "PrighMuted")
  end
  return out
end

return M
