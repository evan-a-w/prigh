-- elicitation/create (form): fields asked in order, reviewed, submitted;
-- edit again; decline; Esc cancels; url mode refused.
local T = require("helpers")
local schema = {
  type = "object",
  properties = {
    strategy = { type = "string", enum = { "conservative", "aggressive" }, title = "Strategy" },
    name = { type = "string", description = "Branch name", default = "fix" },
    tries = { type = "integer" },
    push = { type = "boolean" },
  },
  required = { "strategy", "name" },
}
local function ask()
  return {
    request = "elicitation/create",
    params = { sessionId = "s1", mode = "form", message = "How should I proceed?", requestedSchema = schema },
  }
end
local script = T.handshake()
vim.list_extend(script, {
  ask(),
  ask(),
  ask(),
  ask(),
  {
    request = "elicitation/create",
    params = { sessionId = "s1", mode = "url", elicitationId = "e1", url = "https://example.com", message = "Log in" },
  },
})
-- Fields come in order: required (strategy, name), then the rest by name.
-- 1: submit, with a bad integer asked again
T.answer("aggressive", "my-branch", "yes", "x", "3", "Submit")
-- 2: a required field left empty is asked again; edit again, then submit
T.answer("conservative", "", "fix-2", "no", "", "Edit again", "aggressive", "b", "no", "1", "Submit")
-- 3: decline
T.answer("conservative", "n", "no", "", "Decline")
-- 4: Esc on the first field cancels
T.answer(nil)
local agent = T.start(script)
vim.cmd("Prigh")
T.wait_agent("only supports form", "the url request answered")
T.finish()
