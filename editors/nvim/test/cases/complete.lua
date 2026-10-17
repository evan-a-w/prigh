-- @ file completion (fuzzy over the project's files), / command completion
-- from available_commands_update, the trigger mappings, and what a prompt
-- with mentions sends; unknown commands are refused with suggestions.
local T = require("helpers")
for _, p in ipairs({ "src/main.ml", "src/util.ml", "README.md", "test/test_main.ml" }) do
  vim.fn.mkdir(vim.fs.dirname(T.tmp .. "/" .. p), "p")
  local f = assert(io.open(T.tmp .. "/" .. p, "w"))
  f:write("x\n")
  f:close()
end
local script = T.handshake()
vim.list_extend(script, {
  {
    update = {
      sessionUpdate = "available_commands_update",
      availableCommands = {
        { name = "compact", description = "Compact the context" },
        { name = "goal", description = "Set a goal for the session", input = { hint = "what to achieve" } },
        { name = "model", description = "Switch the model" },
      },
    },
  },
  { recv = "session/prompt", result = { stopReason = "end_turn" } },
  { recv = "session/prompt", result = { stopReason = "end_turn" } },
})
local agent, chat = T.start(script)
vim.cmd("Prigh")
T.wait(function()
  return #agent.session.commands > 0
end, "commands")
local complete = require("prigh.complete")

-- The cursor at the end of `line` in insert mode.
local function at(line)
  local kind, start, base = complete.context(line, #line, 1)
  if not kind then
    return T.out(string.format("%q: no completion", line))
  end
  T.out(string.format("%q: %s from column %d, base %q", line, kind, start, base))
  complete.pending_kind = kind
  for _, it in ipairs(complete.func(0, base)) do
    T.out(string.format("  %-22s %s", it.abbr or it.word, it.menu or ""))
  end
end

T.section("@ completion")
at("look at @")
at("look at @mn")
at("look at @src/u")
at("email me@")
T.section("/ completion")
at("/")
at("/go")
at("x /go")

T.section("trigger mappings")
vim.api.nvim_set_current_win(chat.input_win)
vim.api.nvim_buf_set_lines(chat.input, 0, -1, false, { "" })
vim.api.nvim_win_set_cursor(chat.input_win, { 1, 0 })
T.out("@ at start ->", vim.inspect(vim.fn.maparg("@", "i", false, true).callback()))
T.out("/ at start ->", vim.inspect(vim.fn.maparg("/", "i", false, true).callback()))
vim.api.nvim_buf_set_lines(chat.input, 0, -1, false, { "ab" })
vim.api.nvim_win_set_cursor(chat.input_win, { 1, 1 })
T.out("/ after text ->", vim.inspect(vim.fn.maparg("/", "i", false, true).callback()))
T.out("@ after a letter ->", vim.inspect(vim.fn.maparg("@", "i", false, true).callback()))
vim.api.nvim_buf_set_lines(chat.input, 0, -1, false, { "a b" })
vim.api.nvim_win_set_cursor(chat.input_win, { 1, 2 })
T.out("@ after a space ->", vim.inspect(vim.fn.maparg("@", "i", false, true).callback()))

T.section("unknown command")
vim.api.nvim_buf_set_lines(chat.input, 0, -1, false, { "/gaol make it work" })
chat:send()
T.out("input kept: " .. vim.inspect(vim.api.nvim_buf_get_lines(chat.input, 0, -1, false)))

T.section("send a command and a prompt with mentions")
vim.api.nvim_buf_set_lines(chat.input, 0, -1, false, { "/goal make it work" })
chat:send()
T.wait(function()
  return not agent.session.running
end, "turn")
vim.api.nvim_buf_set_lines(chat.input, 0, -1, false, { "compare @src/main.ml with @README.md, not @missing.ml" })
chat:send()
T.wait(function()
  return not agent.session.running
end, "turn")
T.agent_log()
T.chat()
T.finish()
