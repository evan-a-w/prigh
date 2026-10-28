-- fs/read_text_file and fs/write_text_file against Neovim's buffers, and the
-- record of what the agent changed (for :PrighReview).
local config = require("prigh.config")

local M = {}

-- path -> { before = lines | nil (file did not exist), order = n }
M.changes = {}
local order = 0

local function norm(path)
  return vim.fs.normalize(vim.fn.fnamemodify(path, ":p"))
end

local function loaded_buf(path)
  local want = norm(path)
  for _, buf in ipairs(vim.api.nvim_list_bufs()) do
    if vim.api.nvim_buf_is_loaded(buf) and vim.bo[buf].buftype == "" then
      local name = vim.api.nvim_buf_get_name(buf)
      if name ~= "" and norm(name) == want then
        return buf
      end
    end
  end
end
M.loaded_buf = loaded_buf

local function buf_lines(buf)
  return vim.api.nvim_buf_get_lines(buf, 0, -1, false)
end

local function split_text(text)
  local lines = vim.split(text, "\n", { plain = true })
  if lines[#lines] == "" then
    table.remove(lines)
  end
  return lines
end

local function read_disk(path)
  local f = io.open(path, "rb")
  if not f then
    return nil
  end
  local s = f:read("*a")
  f:close()
  return s
end

--- Current text of `path` as lines: the buffer if loaded, else the disk.
function M.current_lines(path)
  local buf = loaded_buf(path)
  if buf then
    return buf_lines(buf)
  end
  local s = read_disk(path)
  return s and split_text(s) or nil
end

--- fs/read_text_file. `line` is 1-based; `limit` caps the number of lines.
function M.read(params)
  local path = params.path
  local lines = M.current_lines(path)
  if not lines then
    return nil, { code = -32002, message = "file not found: " .. tostring(path) }
  end
  local first = math.max(params.line or 1, 1)
  local last = #lines
  if params.limit then
    last = math.min(last, first + params.limit - 1)
  end
  local out = {}
  for i = first, last do
    table.insert(out, lines[i])
  end
  local text = table.concat(out, "\n")
  if #out > 0 and (last < #lines or not loaded_buf(path) or vim.bo[loaded_buf(path)].eol) then
    text = text .. "\n"
  end
  return { content = text }
end

local function record(path, before)
  if M.changes[path] then
    return
  end
  order = order + 1
  M.changes[path] = { before = before, order = order }
end

function M.save(buf)
  local cmd = config.options.fs.write_autocmds and "silent keepalt write!" or "silent keepalt noautocmd write!"
  local ok, err = pcall(vim.api.nvim_buf_call, buf, function()
    vim.cmd(cmd)
  end)
  return ok, err
end

--- Replace the buffer's lines with `lines` as one undo step, touching only
--- the hunks that differ (keeps marks, folds and the cursor in place).
--- With `join`, the change joins the previous undo step instead.
function M.set_buffer_lines(buf, lines, join)
  local old = buf_lines(buf)
  local a = table.concat(old, "\n") .. "\n"
  local b = table.concat(lines, "\n") .. "\n"
  local hunks = (vim.text.diff or vim.diff)(a, b, { result_type = "indices" })
  if #hunks == 0 then
    return
  end
  vim.api.nvim_buf_call(buf, function()
    if not join then
      -- Start a new undo block so the agent's edit is not merged into the user's.
      vim.o.undolevels = vim.o.undolevels
    end
    for i = #hunks, 1, -1 do
      local sa, ca, sb, cb = unpack(hunks[i])
      local start = ca == 0 and sa or sa - 1
      local repl = {}
      local first_b = cb == 0 and sb or sb - 1
      for j = 1, cb do
        repl[j] = lines[first_b + j]
      end
      if join or i < #hunks then
        vim.cmd("silent! undojoin")
      end
      vim.api.nvim_buf_set_lines(buf, start, start + ca, false, repl)
    end
  end)
end

--- fs/write_text_file.
function M.write(params)
  local path = params.path
  local content = params.content or ""
  local lines = split_text(content)
  local buf = loaded_buf(path)
  if buf then
    record(path, buf_lines(buf))
    M.set_buffer_lines(buf, lines)
    if config.options.fs.save_after_write then
      local ok, err = M.save(buf)
      if not ok then
        return nil, { code = -32603, message = string.format("wrote the buffer but :write failed: %s", err) }
      end
    end
    return vim.empty_dict()
  end
  local old = read_disk(path)
  record(path, old and split_text(old) or nil)
  vim.fn.mkdir(vim.fs.dirname(path), "p")
  local f, err = io.open(path, "wb")
  if not f then
    return nil, { code = -32603, message = "cannot write " .. path .. ": " .. tostring(err) }
  end
  f:write(content)
  f:close()
  return vim.empty_dict()
end

--- Changed files in the order they were first written.
function M.changed_files()
  local out = {}
  for path, c in pairs(M.changes) do
    table.insert(out, { path = path, before = c.before, order = c.order })
  end
  table.sort(out, function(x, y)
    return x.order < y.order
  end)
  return out
end

function M.forget(path)
  M.changes[path] = nil
end

function M.reset()
  M.changes = {}
end

return M
