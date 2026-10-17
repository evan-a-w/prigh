-- elicitation/create (form mode) as a sequence of vim.ui.select / vim.ui.input
-- prompts, then a review step (submit, edit again, decline, cancel). Esc on
-- any prompt cancels.
local M = {}

local function fields(schema)
  local props = schema.properties or {}
  local names, seen = {}, {}
  for _, n in ipairs(schema.required or {}) do
    if props[n] and not seen[n] then
      table.insert(names, n)
      seen[n] = true
    end
  end
  local rest = {}
  for n in pairs(props) do
    if not seen[n] then
      table.insert(rest, n)
    end
  end
  table.sort(rest)
  vim.list_extend(names, rest)
  local required = {}
  for _, n in ipairs(schema.required or {}) do
    required[n] = true
  end
  local out = {}
  for _, n in ipairs(names) do
    table.insert(out, { name = n, schema = props[n], required = required[n] })
  end
  return out
end

local function show(v)
  if v == nil then
    return "(empty)"
  elseif type(v) == "table" then
    return table.concat(v, ", ")
  end
  return tostring(v)
end

local function ask(field, current, done)
  local s = field.schema or {}
  local label = (s.title or field.name) .. (field.required and "" or " (optional)")
  if s.description then
    label = label .. ": " .. s.description
  end
  local enum = s.enum or (s.oneOf and vim.tbl_map(function(o)
    return o.const
  end, s.oneOf))
  if s.type == "boolean" then
    vim.ui.select({ "yes", "no" }, { prompt = label }, function(choice)
      if choice == nil then
        return done(nil, true)
      end
      done(choice == "yes")
    end)
  elseif enum then
    local names = {}
    for i, v in ipairs(enum) do
      names[i] = s.oneOf and s.oneOf[i].title or (s.enumNames and s.enumNames[i]) or tostring(v)
    end
    vim.ui.select(names, { prompt = label }, function(_, idx)
      if idx == nil then
        return done(nil, true)
      end
      done(enum[idx])
    end)
  else
    local default = current
    if default == nil then
      default = s.default
    end
    vim.ui.input({ prompt = label .. ": ", default = default ~= nil and tostring(default) or "" }, function(text)
      if text == nil then
        return done(nil, true)
      end
      if text == "" then
        if field.required then
          vim.notify("prigh: " .. field.name .. " is required", vim.log.levels.WARN)
          return ask(field, current, done)
        end
        return done(nil)
      end
      if s.type == "number" or s.type == "integer" then
        local n = tonumber(text)
        if not n or (s.type == "integer" and n ~= math.floor(n)) then
          vim.notify(string.format("prigh: %s must be a%s %s", field.name, s.type == "integer" and "n" or "", s.type), vim.log.levels.WARN)
          return ask(field, current, done)
        end
        return done(n)
      end
      done(text)
    end)
  end
end

--- params: elicitation/create params; respond(result).
function M.run(params, respond)
  local fs = fields(params.requestedSchema or {})
  local values = {}
  local function review()
    local summary = { params.message or "The agent asks:" }
    for _, f in ipairs(fs) do
      table.insert(summary, string.format("  %s: %s", f.name, show(values[f.name])))
    end
    vim.ui.select({ "Submit", "Edit again", "Decline", "Cancel" }, {
      prompt = table.concat(summary, "\n"),
    }, function(choice)
      if choice == "Submit" then
        local content = vim.empty_dict()
        for k, v in pairs(values) do
          content[k] = v
        end
        respond({ action = "accept", content = content })
      elseif choice == "Edit again" then
        M._step(fs, 1, values, review, respond)
      elseif choice == "Decline" then
        respond({ action = "decline" })
      else
        respond({ action = "cancel" })
      end
    end)
  end
  if params.message then
    vim.notify("prigh asks: " .. params.message)
  end
  M._step(fs, 1, values, review, respond)
end

function M._step(fs, i, values, finish, respond)
  if i > #fs then
    return finish()
  end
  local f = fs[i]
  ask(f, values[f.name], function(v, cancelled)
    if cancelled then
      return respond({ action = "cancel" })
    end
    values[f.name] = v
    M._step(fs, i + 1, values, finish, respond)
  end)
end

return M
