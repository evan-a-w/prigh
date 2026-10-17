-- :PrighAdd attaches the buffer (unsaved text), a range, diagnostics; the
-- input bar shows them; they go with the next prompt as embedded resources
-- (or text when the agent lacks embeddedContext). Notices are rendered.
local T = require("helpers")
local path = T.tmp .. "/code.lua"
local f = assert(io.open(path, "w"))
f:write("local a = 1\nlocal b = 2\nlocal c = 3\n")
f:close()
local no_embed = vim.deepcopy(T.init_result)
no_embed.agentCapabilities.promptCapabilities = vim.empty_dict()
local script = T.handshake()
vim.list_extend(script, {
  {
    update = {
      sessionUpdate = "notice",
      severity = "warning",
      title = "MCP server unavailable",
      description = "Continuing without it.",
    },
  },
  { recv = "session/prompt", result = { stopReason = "end_turn" } },
})
local agent, chat = T.start(script)
vim.cmd.edit(path)
local buf = vim.api.nvim_get_current_buf()
vim.api.nvim_buf_set_lines(buf, 0, 1, false, { "local a = 'unsaved'" })
local ns = vim.api.nvim_create_namespace("test")
vim.diagnostic.set(ns, buf, {
  { lnum = 1, col = 6, message = "unused variable b", severity = vim.diagnostic.severity.WARN, source = "lua_ls" },
  { lnum = 2, col = 0, message = "syntax error", severity = vim.diagnostic.severity.ERROR },
})
vim.cmd("Prigh")
T.wait(function()
  return agent.conn_state == "ready" and #agent.session.entries > 0
end, "session")
vim.cmd("wincmd t") -- back to code.lua

T.section("attach")
vim.cmd("PrighAdd")
vim.cmd("2,3PrighAdd")
vim.cmd("PrighAdd diagnostics")
vim.cmd("3PrighAdd diagnostics")
vim.cmd("1PrighAdd diagnostics")
vim.cmd("PrighAdd bogus")
T.winbar()

T.section("from the chat buffer")
vim.api.nvim_set_current_win(chat.win)
vim.cmd("PrighAdd")

T.section("send")
vim.api.nvim_buf_set_lines(chat.input, 0, -1, false, { "review these" })
chat:send()
T.wait(function()
  return not agent.session.running
end, "turn")
T.agent_log()
T.chat()
T.winbar()

T.section(":PrighAdd clear")
vim.cmd("wincmd t")
vim.cmd("PrighAdd")
vim.cmd("PrighAdd clear")
T.winbar()

T.section("without embeddedContext: text blocks")
local ctx = require("prigh.context")
T.out(T.json(ctx.buffer(buf, false)))
T.out(T.json(ctx.selection(buf, 2, 2, false)))
T.finish()
