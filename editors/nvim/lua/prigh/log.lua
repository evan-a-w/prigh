-- In-memory log of the agent's stderr, protocol traffic and client errors.
local M = {}

M.lines = {}
local listeners = {}

local function max_lines()
  local ok, config = pcall(require, "prigh.config")
  return ok and config.options.log.max_lines or 5000
end

function M.add(kind, msg)
  local stamp = os.date("%H:%M:%S")
  for _, l in ipairs(vim.split(msg, "\n", { plain = true })) do
    table.insert(M.lines, string.format("%s %-6s %s", stamp, kind, l))
  end
  local over = #M.lines - max_lines()
  if over > 0 then
    M.lines = vim.list_slice(M.lines, over + 1)
  end
  for _, f in ipairs(listeners) do
    f()
  end
end

function M.info(msg)
  M.add("info", msg)
end

function M.error(msg)
  M.add("error", msg)
end

function M.stderr(msg)
  M.add("stderr", msg)
end

--- Last `n` stderr lines, without the prefix.
function M.last_stderr(n)
  local out = {}
  for i = #M.lines, 1, -1 do
    local l = M.lines[i]:match("^%S+ stderr (.*)$")
    if l then
      table.insert(out, 1, l)
      if #out >= n then
        break
      end
    end
  end
  return out
end

function M.clear()
  M.lines = {}
end

--- :PrighLog: a scratch buffer that follows the log.
function M.open()
  local name = "prigh://log"
  local buf = vim.fn.bufnr(name)
  if buf == -1 then
    buf = vim.api.nvim_create_buf(false, true)
    vim.api.nvim_buf_set_name(buf, name)
    vim.bo[buf].bufhidden = "hide"
    vim.bo[buf].filetype = "prighlog"
    vim.keymap.set("n", "q", "<cmd>close<cr>", { buffer = buf, desc = "prigh: close log" })
    table.insert(listeners, function()
      if vim.api.nvim_buf_is_valid(buf) then
        vim.schedule(function()
          M.render(buf)
        end)
      end
    end)
  end
  M.render(buf)
  vim.cmd("botright split")
  vim.api.nvim_win_set_buf(0, buf)
  vim.cmd("normal! G")
  return buf
end

function M.render(buf)
  if not vim.api.nvim_buf_is_valid(buf) then
    return
  end
  vim.bo[buf].modifiable = true
  vim.api.nvim_buf_set_lines(buf, 0, -1, false, #M.lines > 0 and M.lines or { "(log is empty)" })
  vim.bo[buf].modifiable = false
end

return M
