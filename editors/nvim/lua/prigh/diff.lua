-- Line diffs shared by the chat (unified text) and the hunk review.
local M = {}

local function join(lines)
  if #lines == 0 then
    return ""
  end
  return table.concat(lines, "\n") .. "\n"
end

function M.split(text)
  if text == nil or text == "" then
    return {}
  end
  local lines = vim.split(text, "\n", { plain = true })
  if lines[#lines] == "" then
    table.remove(lines)
  end
  return lines
end

--- Hunks turning `a` into `b` (lists of lines). Each hunk:
---   a_start: 0-based index in `a` of the first replaced line (or insertion point)
---   a_lines: removed lines;  b_start/b_lines: the same for `b`
function M.hunks(a, b)
  local out = {}
  for _, h in ipairs((vim.text.diff or vim.diff)(join(a), join(b), { result_type = "indices" })) do
    local sa, ca, sb, cb = unpack(h)
    local a0 = ca == 0 and sa or sa - 1
    local b0 = cb == 0 and sb or sb - 1
    table.insert(out, {
      a_start = a0,
      a_lines = vim.list_slice(a, a0 + 1, a0 + ca),
      b_start = b0,
      b_lines = vim.list_slice(b, b0 + 1, b0 + cb),
    })
  end
  return out
end

--- Unified diff lines (without the ---/+++ header).
function M.unified(old_text, new_text, ctx)
  local function nl(s)
    s = s or ""
    return (s == "" or s:sub(-1) == "\n") and s or s .. "\n"
  end
  local d = (vim.text.diff or vim.diff)(nl(old_text), nl(new_text), { ctxlen = ctx or 3 })
  local lines = vim.split(d or "", "\n", { plain = true })
  if lines[#lines] == "" then
    table.remove(lines)
  end
  return lines
end

return M
