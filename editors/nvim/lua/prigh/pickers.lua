-- vim.ui.select pickers: sessions, modes, config options, changed files.
local session_mod = require("prigh.session")
local fs = require("prigh.fs")

local M = {}

local function rel(path, chat)
  return require("prigh.render").rel(path, chat.agent.cwd)
end

local function notify(msg, level)
  vim.notify("prigh: " .. msg, level or vim.log.levels.INFO)
end

function M.sessions(agent, chat)
  agent:ensure(function()
    if not agent:can_list() then
      notify("this agent cannot list sessions; :PrighNew starts a new one", vim.log.levels.WARN)
      return
    end
    agent:list_sessions(function(err, sessions)
      if err then
        return notify("session/list failed: " .. tostring(err.message) .. " (:PrighLog)", vim.log.levels.ERROR)
      end
      table.sort(sessions, function(a, b)
        return (a.updatedAt or "") > (b.updatedAt or "")
      end)
      local items = { { new = true } }
      vim.list_extend(items, sessions)
      vim.ui.select(items, {
        prompt = "prigh sessions in " .. vim.fn.fnamemodify(agent.cwd, ":~"),
        format_item = function(s)
          if s.new then
            return "+ new session"
          end
          local mark = s.sessionId == agent.session.id and "● " or "  "
          return string.format("%s%s · %s · %s", mark, s.title or "(untitled)", s.updatedAt or "?", s.sessionId)
        end,
      }, function(choice)
        if not choice then
          return
        end
        if choice.new then
          agent:new_session()
        else
          agent:load_session(choice.sessionId)
        end
        chat:open()
      end)
    end)
  end)
end

local function select_value(agent, opt)
  local values = session_mod.option_values(opt)
  if opt.type == "boolean" then
    values = { { value = true, name = "on" }, { value = false, name = "off" } }
  end
  vim.ui.select(values, {
    prompt = opt.name .. (opt.description and (" — " .. opt.description) or ""),
    format_item = function(v)
      local mark = v.value == opt.currentValue and "● " or "  "
      return mark .. v.name .. (v.description and (" — " .. v.description) or "")
    end,
  }, function(choice)
    if choice and choice.value ~= opt.currentValue then
      agent:set_config_option(opt.id, choice.value)
    end
  end)
end

function M.mode(agent)
  agent:ensure(function()
    local s = agent.session
    local opt = s:config_option("mode")
    if opt then
      return select_value(agent, opt)
    end
    local modes = s.modes and s.modes.availableModes or {}
    if #modes == 0 then
      return notify("this agent has no session modes", vim.log.levels.WARN)
    end
    vim.ui.select(modes, {
      prompt = "prigh mode",
      format_item = function(m)
        local mark = m.id == s.modes.currentModeId and "● " or "  "
        return mark .. m.name .. (m.description and (" — " .. m.description) or "")
      end,
    }, function(choice)
      if choice then
        agent:set_mode(choice.id)
      end
    end)
  end)
end

--- Pick the value of the option of `category` (e.g. "model"), or of any option.
function M.config(agent, category)
  agent:ensure(function()
    local s = agent.session
    if category then
      local opt = s:config_option(category)
      if not opt then
        return notify(
          string.format("the agent has no %s option; :PrighConfig lists what it has", category),
          vim.log.levels.WARN
        )
      end
      return select_value(agent, opt)
    end
    local opts = vim.tbl_filter(function(o)
      return o.type == "select" or o.type == "boolean"
    end, s.config_options or {})
    if #opts == 0 then
      return notify("the agent has no config options", vim.log.levels.WARN)
    end
    vim.ui.select(opts, {
      prompt = "prigh config",
      format_item = function(o)
        return string.format("%s: %s", o.name, session_mod.option_value_name(o))
      end,
    }, function(choice)
      if choice then
        select_value(agent, choice)
      end
    end)
  end)
end

-- Review ---------------------------------------------------------------------

local function stats(c)
  local now = fs.current_lines(c.path) or {}
  local add, del = 0, 0
  for _, h in ipairs(require("prigh.diff").hunks(c.before or {}, now)) do
    add = add + #h.b_lines
    del = del + #h.a_lines
  end
  return add, del
end

local function target_window(chat)
  local cur = vim.api.nvim_get_current_win()
  local function usable(w)
    return w ~= chat.win and w ~= chat.input_win and vim.api.nvim_win_get_config(w).relative == ""
  end
  if usable(cur) then
    return cur
  end
  for _, w in ipairs(vim.api.nvim_tabpage_list_wins(0)) do
    if usable(w) then
      return w
    end
  end
  vim.api.nvim_set_current_win(chat.win)
  vim.cmd("leftabove vsplit")
  return vim.api.nvim_get_current_win()
end

function M.review_file(c, chat, on_done)
  local win = target_window(chat)
  vim.api.nvim_set_current_win(win)
  vim.cmd.edit(vim.fn.fnameescape(c.path))
  local buf = vim.api.nvim_get_current_buf()
  require("prigh.review").open({
    buf = buf,
    before = c.before or {},
    title = rel(c.path, chat),
    on_done = function(res)
      if res.status == "applied" then
        if res.rejected > 0 and require("prigh.config").options.fs.save_after_write then
          fs.save(buf)
        end
        fs.forget(c.path)
        notify(
          string.format(
            "%s: kept %d hunk%s, reverted %d",
            rel(c.path, chat),
            res.accepted,
            res.accepted == 1 and "" or "s",
            res.rejected
          )
        )
      end
      if on_done then
        on_done(res)
      end
    end,
  })
end

--- :PrighReview: pick one of the files the agent changed, review its hunks.
function M.review(chat)
  local files = fs.changed_files()
  if #files == 0 then
    return notify("the agent has not changed any files since the last review")
  end
  local function next_file(res)
    if res.status == "applied" and #fs.changed_files() > 0 then
      vim.schedule(function()
        M.review(chat)
      end)
    end
  end
  if #files == 1 then
    return M.review_file(files[1], chat, next_file)
  end
  vim.ui.select(files, {
    prompt = "prigh: review changes",
    format_item = function(c)
      local add, del = stats(c)
      return string.format("%s  +%d −%d%s", rel(c.path, chat), add, del, c.before == nil and " (new)" or "")
    end,
  }, function(choice)
    if choice then
      M.review_file(choice, chat, next_file)
    end
  end)
end

return M
