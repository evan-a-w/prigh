-- Against the real prigh in approval mode ask: permission cards answered
-- with a (allow), r (reject) and A (always); a card withdrawn by prigh
-- ($/cancel_request) when switching to Auto answers it; an ask_user form
-- (elicitation/create).
local T = require("helpers")
local function bash(id, command, text)
  return { text = text, tool_calls = { { id = id, name = "bash", arguments = { command = command } } } }
end
local agent, chat = T.start_prigh({
  bash("c1", "echo first", "One."),
  { text = "first done" },
  bash("c2", "echo second", "Two."),
  { text = "second done" },
  bash("c3", "echo third", "Three."),
  { text = "third done" },
  bash("c4", "touch fourth", "Four."),
  { text = "fourth done" },
  {
    text = "Asking.",
    tool_calls = {
      {
        id = "q1",
        name = "ask_user",
        arguments = {
          questions = {
            {
              question = "Which database?",
              header = "Storage",
              options = { { label = "Postgres", description = "what prod uses" }, { label = "SQLite" } },
            },
            { question = "Anything else?" },
          },
        },
      },
    },
  },
  { text = "Thanks." },
}, { prigh_config = { approval_mode = "ask", ask_user_timeout_seconds = 0 } })

local function send(text)
  vim.api.nvim_buf_set_lines(chat.input, 0, -1, false, { text })
  chat:send()
end
local function card()
  T.wait(function()
    return agent.pending_permission ~= nil
  end, "a permission card", 15000)
  T.traffic(function(_, msg)
    return msg.method == "session/request_permission"
  end)
  T.chat()
  T.winbar()
end
local function answered()
  T.turn_end()
  T.traffic(function(dir, msg)
    return dir == "send" and msg.result and msg.result.outcome
  end)
  T.chat()
end

T.open_prigh()
T.traffic_skip()

T.section("a: allow")
send("first")
card()
T.out("focus in chat: " .. tostring(vim.api.nvim_get_current_win() == chat.win))
T.feed("a")
answered()

T.section("r: reject")
vim.cmd("PrighNew")
T.wait(function()
  return #agent.session.entries == 0 and agent.session.prigh
end, "new session")
send("second")
card()
T.feed("r")
answered()

T.section("A: always")
vim.cmd("PrighNew")
T.wait(function()
  return #agent.session.entries == 0 and agent.session.prigh
end, "new session")
send("third")
card()
T.feed("A")
answered()

T.section("switching to Auto answers the card: prigh withdraws it")
vim.cmd("PrighNew")
T.wait(function()
  return #agent.session.entries == 0 and agent.session.prigh
end, "new session")
send("fourth")
card()
T.answer("Auto —")
vim.cmd("PrighMode")
T.turn_end()
T.traffic(function(_, msg)
  return msg.method == "session/set_config_option" or msg.method == "$/cancel_request" or (msg.error ~= nil)
end)
T.chat()
T.winbar()
T.out("keys back: " .. tostring(vim.fn.maparg("a", "n", false, true).desc))

T.section("ask_user: an elicitation form")
T.answer("Postgres", "", "nothing", "Submit")
send("ask")
T.turn_end()
T.traffic(function(_, msg)
  return msg.method == "elicitation/create" or (msg.result and msg.result.action)
end)
T.chat()
T.finish()
