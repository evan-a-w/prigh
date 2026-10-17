-- Building prompt content blocks: attachments (buffer, selection,
-- diagnostics) and @file mentions.
local M = {}

local function uri(path)
  return vim.uri_from_fname(vim.fs.normalize(vim.fn.fnamemodify(path, ":p")))
end

local function rel(path)
  return vim.fn.fnamemodify(path, ":~:.")
end

local function fenced(label, lines, ft)
  return string.format("%s\n```%s\n%s\n```", label, ft or "", table.concat(lines, "\n"))
end

--- An attachment: { label = shown in the input bar, block = ContentBlock }.
--- `embedded`: the agent accepts embedded resources (promptCapabilities.embeddedContext).
function M.buffer(buf, embedded)
  local path = vim.api.nvim_buf_get_name(buf)
  if path == "" then
    return nil, "this buffer has no file name: save it first or select the text and :'<,'>PrighAdd"
  end
  local lines = vim.api.nvim_buf_get_lines(buf, 0, -1, false)
  local text = table.concat(lines, "\n") .. "\n"
  local block
  if embedded then
    block = { type = "resource", resource = { uri = uri(path), text = text, mimeType = "text/plain" } }
  else
    block = { type = "text", text = fenced(rel(path) .. ":", lines, vim.bo[buf].filetype) }
  end
  return { label = rel(path), block = block }
end

--- Lines first..last (1-based, inclusive) of `buf`.
function M.selection(buf, first, last, embedded)
  local path = vim.api.nvim_buf_get_name(buf)
  local name = path ~= "" and rel(path) or ("[buffer " .. buf .. "]")
  local lines = vim.api.nvim_buf_get_lines(buf, first - 1, last, false)
  local label = string.format("%s:%d-%d", name, first, last)
  local block
  if embedded and path ~= "" then
    block = {
      type = "resource",
      resource = {
        uri = string.format("%s#L%d-L%d", uri(path), first, last),
        text = table.concat(lines, "\n") .. "\n",
        mimeType = "text/plain",
      },
    }
  else
    block = { type = "text", text = fenced(label .. ":", lines, vim.bo[buf].filetype) }
  end
  return { label = label, block = block }
end

local severity = { "error", "warning", "info", "hint" }

--- Diagnostics of `buf` (optionally only lines first..last) as a text block.
function M.diagnostics(buf, first, last)
  local path = vim.api.nvim_buf_get_name(buf)
  local name = path ~= "" and rel(path) or ("[buffer " .. buf .. "]")
  local out = {}
  local diags = vim.diagnostic.get(buf)
  table.sort(diags, function(a, b)
    return a.lnum < b.lnum or (a.lnum == b.lnum and a.col < b.col)
  end)
  for _, d in ipairs(diags) do
    local l = d.lnum + 1
    if not first or (l >= first and l <= last) then
      table.insert(
        out,
        string.format(
          "%s:%d:%d: %s: %s%s",
          name,
          l,
          d.col + 1,
          severity[d.severity] or "?",
          d.message:gsub("\n", " "),
          d.source and (" [" .. d.source .. "]") or ""
        )
      )
    end
  end
  if #out == 0 then
    return nil, string.format("no diagnostics in %s%s", name, first and string.format(":%d-%d", first, last) or "")
  end
  return {
    label = string.format("diagnostics %s (%d)", name, #out),
    block = { type = "text", text = "Diagnostics:\n" .. table.concat(out, "\n") },
  }
end

--- resource_link blocks for the @path mentions in `text` that name files.
function M.mentions(text, cwd)
  local out, seen = {}, {}
  for path in text:gmatch("@([^%s]+)") do
    path = path:gsub("[%.,;:!?%)]+$", "")
    local abs = path:sub(1, 1) == "/" and path or (cwd .. "/" .. path)
    abs = vim.fs.normalize(abs)
    if not seen[abs] and vim.uv.fs_stat(abs) then
      seen[abs] = true
      table.insert(out, { type = "resource_link", uri = vim.uri_from_fname(abs), name = path })
    end
  end
  return out
end

return M
