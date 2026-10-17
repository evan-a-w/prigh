-- prigh.nvim: a Neovim client for prigh (or any agent) over the Agent Client
-- Protocol. See :help prigh.
local config = require("prigh.config")

local M = {}

local state = {} -- agent, chat

--- The agent and the chat view, created on first use.
function M.get()
  if not state.agent then
    local agent_mod = require("prigh.agent")
    local chat_mod = require("prigh.chat")
    local chat
    local agent = agent_mod.new({
      permission = function(entry)
        chat:permission(entry)
      end,
      elicit = function(params, respond)
        require("prigh.elicit").run(params, respond)
      end,
      unqueue = function(prompts)
        chat:unqueue(prompts)
      end,
    })
    chat = chat_mod.new(agent)
    state.agent, state.chat = agent, chat
  end
  return state.agent, state.chat
end

--- Stop the agent and drop the chat (used by tests and :PrighRestart!).
function M.reset()
  if state.agent then
    state.agent:stop()
  end
  if state.chat then
    state.chat:close()
  end
  state = {}
end

-- Commands -------------------------------------------------------------------

function M.toggle()
  local agent, chat = M.get()
  chat:toggle()
  agent:ensure()
end

function M.open()
  local agent, chat = M.get()
  chat:open()
  agent:ensure()
end

function M.new_session()
  local agent, chat = M.get()
  chat:open()
  agent:new_session()
end

function M.sessions()
  local agent, chat = M.get()
  require("prigh.pickers").sessions(agent, chat)
end

function M.cancel()
  local agent = M.get()
  agent:cancel()
end

function M.restart()
  local agent, chat = M.get()
  chat:open()
  if agent.conn_state == "stopped" then
    return agent:ensure()
  end
  agent:restart()
end

function M.mode()
  require("prigh.pickers").mode((M.get()))
end

function M.model()
  require("prigh.pickers").config((M.get()), "model")
end

function M.config_options(category)
  require("prigh.pickers").config((M.get()), category)
end

function M.review()
  local _, chat = M.get()
  require("prigh.pickers").review(chat)
end

function M.log()
  require("prigh.log").open()
end

local function embedded()
  local agent = M.get()
  local pc = agent.caps.promptCapabilities
  -- Before the agent answered initialize we cannot know: assume embedded.
  return agent.conn_state ~= "ready" or (pc and pc.embeddedContext) or false
end

--- :PrighAdd [diagnostics|clear]: attach to the next prompt the current
--- buffer, or with a range the lines, or the diagnostics (of the range).
function M.add(opts)
  opts = opts or {}
  local agent, chat = M.get()
  local _ = agent
  local buf = vim.api.nvim_get_current_buf()
  if buf == chat.buf or buf == chat.input then
    return vim.notify("prigh: :PrighAdd attaches a file buffer: run it from the buffer to attach", vim.log.levels.WARN)
  end
  local what = opts.args or ""
  local ranged = opts.range and opts.range > 0
  local a, err
  if what == "clear" then
    chat:clear_attachments()
    return vim.notify("prigh: attachments cleared")
  elseif what == "diagnostics" then
    if ranged then
      a, err = require("prigh.context").diagnostics(buf, opts.line1, opts.line2)
    else
      a, err = require("prigh.context").diagnostics(buf)
    end
  elseif what == "" then
    if ranged then
      a, err = require("prigh.context").selection(buf, opts.line1, opts.line2, embedded())
    else
      a, err = require("prigh.context").buffer(buf, embedded())
    end
  else
    return vim.notify(
      string.format("prigh: unknown :PrighAdd argument %q: use :PrighAdd, :PrighAdd diagnostics or :PrighAdd clear", what),
      vim.log.levels.ERROR
    )
  end
  if not a then
    return vim.notify("prigh: " .. err, vim.log.levels.WARN)
  end
  chat:attach(a)
end

--- For statusline plugins: e.g. lualine_x = { require("prigh").statusline }.
function M.statusline()
  if not state.agent then
    return ""
  end
  return require("prigh.status").text(state.agent)
end

-- Setup ------------------------------------------------------------------------

M.commands = {
  { "Prigh", M.toggle, {}, "toggle the chat" },
  { "PrighNew", M.new_session, {}, "start a new session" },
  { "PrighSessions", M.sessions, {}, "pick a session to load" },
  {
    "PrighAdd",
    function(o)
      M.add(o)
    end,
    {
      range = true,
      nargs = "?",
      complete = function()
        return { "diagnostics", "clear" }
      end,
    },
    "attach the buffer, the range or diagnostics",
  },
  { "PrighCancel", M.cancel, {}, "cancel the running turn" },
  { "PrighMode", M.mode, {}, "pick the session mode" },
  { "PrighModel", M.model, {}, "pick the model" },
  {
    "PrighConfig",
    function(o)
      M.config_options(o.args ~= "" and o.args or nil)
    end,
    { nargs = "?" },
    "pick a config option",
  },
  { "PrighReview", M.review, {}, "review the agent's changes hunk by hunk" },
  { "PrighLog", M.log, {}, "show the agent's log" },
  { "PrighRestart", M.restart, {}, "restart the agent" },
}

function M.create_commands()
  for _, c in ipairs(M.commands) do
    local opts = vim.tbl_extend("force", { desc = "prigh: " .. c[4] }, c[3])
    vim.api.nvim_create_user_command(c[1], function(o)
      c[2](o)
    end, opts)
  end
end

local function set_keymaps()
  local k = config.options.keymaps
  if not k then
    return
  end
  local p = k.prefix
  local function map(mode, key, rhs, desc)
    if key then
      vim.keymap.set(mode, p .. key, rhs, { desc = "prigh: " .. desc })
    end
  end
  map("n", k.toggle, M.toggle, "toggle chat")
  map("n", k.new, M.new_session, "new session")
  map("n", k.sessions, M.sessions, "sessions")
  map("n", k.add, "<cmd>PrighAdd<cr>", "attach buffer")
  map("x", k.add, ":PrighAdd<cr>", "attach selection")
  map("n", k.diagnostics, "<cmd>PrighAdd diagnostics<cr>", "attach diagnostics")
  map("x", k.diagnostics, ":PrighAdd diagnostics<cr>", "attach diagnostics of the selection")
  map("n", k.clear_context, "<cmd>PrighAdd clear<cr>", "clear attachments")
  map("n", k.cancel, M.cancel, "cancel turn")
  map("n", k.mode, M.mode, "mode")
  map("n", k.model, M.model, "model")
  map("n", k.config, M.config_options, "config options")
  map("n", k.review, M.review, "review changes")
  map("n", k.log, M.log, "log")
end

function M.setup(opts)
  config.setup(opts)
  M.create_commands()
  set_keymaps()
end

return M
