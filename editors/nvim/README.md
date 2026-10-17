# prigh.nvim

A Neovim client for prigh, speaking the
[Agent Client Protocol](https://agentclientprotocol.com) to `prigh acp` (any
ACP agent works). A chat beside your code with streamed answers, tool cards,
diffs, plans and permission cards; the agent reads your unsaved buffers and
edits them in place; a hunk-by-hunk review of everything it changed.

Neovim 0.10+ (tested on 0.12), no other plugins.

## Install

lazy.nvim:

```lua
{
  "evan-a-w/prigh",
  config = function(plugin)
    vim.opt.rtp:append(plugin.dir .. "/editors/nvim")
    require("prigh").setup({})
  end,
}
```

packer:

```lua
use({
  "evan-a-w/prigh",
  rtp = "editors/nvim",
  config = function() require("prigh").setup({}) end,
})
```

Or from a checkout: `vim.opt.rtp:append("~/src/prigh/editors/nvim")`. Run
`:helptags ALL` once for `:help prigh`, and `:checkhealth prigh`.

`prigh` must be on `PATH` (or set `cmd`). To use a remote backend (see the
root README's "Remote backend"):

```lua
require("prigh").setup({
  cmd = { "prigh", "acp", "-connect", "server:7777", "-token", "sekrit" },
})
```

## Use

| Command | Default key | |
|---|---|---|
| `:Prigh` | `<leader>pp` | toggle the chat (starts the agent, resumes this directory's last session) |
| `:PrighNew` | `<leader>pn` | new session |
| `:PrighSessions` | `<leader>ps` | pick a session to load (history replayed) |
| `:[range]PrighAdd` | `<leader>pa` | attach the buffer (unsaved text) or the selection to the next prompt |
| `:PrighAdd diagnostics` | `<leader>pd` | attach the diagnostics |
| `:PrighAdd clear` | `<leader>px` | drop the attachments |
| `:PrighCancel` | `<leader>pc` | cancel the turn |
| `:PrighMode` / `:PrighModel` / `:PrighConfig` | `<leader>pm` / `pM` / `po` | pickers for the mode, model, any config option |
| `:PrighReview` | `<leader>pr` | review the agent's file changes hunk by hunk |
| `:PrighLog` | `<leader>pl` | the agent's stderr and the protocol traffic |
| `:PrighRestart` | | restart the agent, reload the session |

In the prompt: `<CR>` (normal) or `<C-s>` sends (queued while a turn runs),
`<C-c>` cancels (queued prompts come back), `@` completes project files
(fuzzy), `/` at the start completes the agent's slash commands. In the chat:
`<Tab>` expands thoughts and tool output, `<CR>` opens a location, `g?` lists
the keys.

Permission requests appear as cards in the chat and own the keyboard: `a`
allow once, `A` always, `r` reject, `R` always reject, `<CR>` the option under
the cursor, `d` the full diff, `<Esc>` cancel.

Review: `]h`/`[h` move, `ga`/`gr` accept/reject a hunk, `gA`/`gR` all; when
all are decided the rejected ones are reverted as one undo step and the file
is saved. `q` leaves without changes.

The chat's winbar shows the model, thinking, mode, context, cost, running
turn, queue, subagents, jobs, goal (prigh's `_prigh/state`); for lualine use
`require("prigh").statusline`.

Everything else (options, highlights, auto-save plugins, the Lua API) is in
[`doc/prigh.txt`](doc/prigh.txt).

## Options

```lua
require("prigh").setup({
  cmd = { "prigh", "acp" },
  env = nil,
  cwd = nil,                 -- default :pwd
  resume_last = true,
  keymaps = { prefix = "<leader>p" }, -- or false; each key can be false
  chat = { position = "right", width = 0.4, input_height = 6,
           tool_output_lines = 0, diff_max_lines = 40, thoughts_collapsed = true },
  permission = { inline_diff_max = 20 },
  fs = { save_after_write = true, write_autocmds = false },
  log = { max_lines = 5000, traffic = true },
})
```

## Design

The protocol and the UI are separate, so other editors can follow the same
structure:

| Module | |
|---|---|
| `acp.lua` | JSON-RPC 2.0 over newline-delimited stdio: spawn, requests/notifications both ways, exit |
| `agent.lua` | the ACP client: initialize, sessions, prompts and queue, fs and permission requests; UI through callbacks |
| `session.lua` | one session's state from `session/update` (entries, tools, plan, modes, config, usage, `_prigh/state`) |
| `fs.lua` | `fs/read_text_file` / `fs/write_text_file` against buffers, and the record of changed files |
| `review.lua`, `diff.lua` | the hunk review, independent of ACP |
| `render.lua`, `status.lua` | entries → lines and highlights; the state line (pure) |
| `chat.lua`, `pickers.lua`, `elicit.lua`, `complete.lua`, `context.lua` | the Neovim UI |

The client advertises `fs.readTextFile`, `fs.writeTextFile`,
`elicitation.form`, `session.notices`, boolean config options and
`_meta.prigh.state` (it understands the `_prigh/state` notification).

## Tests

```
editors/nvim/test/run.sh            # all cases; or run.sh NAME...
PROMOTE=1 editors/nvim/test/run.sh  # re-record the .expected files
nix build .#checks.x86_64-linux.nvim-plugin
```

Each case in `test/cases/` runs in its own headless Neovim against
`test/fake_agent.lua`, a scripted ACP agent that logs what the client sent,
and prints a transcript (chat buffer, winbar, buffers with review marks,
pickers, the agent's received messages) compared with `NAME.expected`.
