# prigh

A coding agent: OCaml backend (Eio, cohttp; Anthropic, OpenAI, OpenAI
Codex/ChatGPT and DeepSeek providers) and a TypeScript terminal frontend. No
plugins; tools and subagents are built in.

## Quick start

```
./prigh                 # builds backend + frontend if needed, then starts the TUI here
./prigh --cwd ~/proj    # ... in another directory
```

## Build

```
# backend (opam switch "prigh": OCaml 5.3, core, eio, cohttp-eio, jsonaf)
cd backend && eval $(opam env --switch=prigh) && dune build && dune build @runtest

# frontend
cd frontend && npm install --ignore-scripts && npm test
```

## Use

Log in to at least one provider (credentials go to
`~/.config/prigh/auth.json`, same shape as pi's `auth.json`):

```
backend/_build/default/bin/main.exe login anthropic        # Claude Pro/Max (browser OAuth)
backend/_build/default/bin/main.exe login anthropic -method api_key
backend/_build/default/bin/main.exe login openai-codex     # ChatGPT Plus/Pro (browser OAuth)
backend/_build/default/bin/main.exe login openai           # OPENAI_API_KEY
backend/_build/default/bin/main.exe login deepseek         # DEEPSEEK_API_KEY
backend/_build/default/bin/main.exe auth                   # status; logout <provider> to remove
```

The same is available inside the TUI as `/login [provider] [api_key|oauth]`,
`/logout <provider>` and `/auth`. A stored credential wins; otherwise
`ANTHROPIC_OAUTH_TOKEN`/`ANTHROPIC_API_KEY`, `OPENAI_API_KEY` and
`DEEPSEEK_API_KEY` are used. OAuth tokens are refreshed automatically
(under a cross-process lock) when they are within five minutes of expiry.
Model ids can be qualified as `provider/id` (e.g. `openai-codex/gpt-5.5`).

Then:

```
node frontend/dist/src/main.js                 # interactive TUI in the current directory
node frontend/dist/src/main.js --faux          # scripted provider, no API calls
backend/_build/default/bin/main.exe run "explain this repo"    # headless
backend/_build/default/bin/main.exe sessions   # list saved sessions
backend/_build/default/bin/main.exe serve      # JSON-lines RPC on stdio
```

TUI: Enter sends (or steers while a run is active), Alt+Enter/Ctrl+J inserts a
newline, Esc aborts, Tab completes `/commands`, Up/Down browse history,
Ctrl+C twice quits. `/help` lists commands (`/model`, `/thinking`, `/compact`,
`/new`, `/sessions`, `/switch`, `/fork`, ...).

Sessions are JSONL trees under `~/.prigh/sessions/`. Project instructions are
read from `AGENTS.md`/`CLAUDE.md` files between `/` and the working
directory, plus `~/.prigh/AGENTS.md`.

## Layout

- `backend/lib` — `Agent_loop` (stream, run tools, repeat), `Agent`
  (session, queues, abort, compaction), `Tools` (bash, read, write, edit, ls,
  grep, find) and `Tool_subagent`, providers (`Anthropic`, `Openai_responses`,
  `Deepseek`, picked per request by `Provider_router`), auth (`Auth_store`,
  `Provider_auth`, `Oauth_anthropic`, `Oauth_openai_codex`, `Login_manager`),
  `Session` JSONL log, `Rpc_server`.
- `backend/test` — expect tests, driven by `Faux_provider` and an in-process
  HTTP server; no network.
- `frontend/src` — `client.ts` (RPC), `tui/` (editor, keys, render, app).
- `ARCHITECTURE.md` — how the pieces fit together; `PLAN.md` — milestone status.
# prigh
