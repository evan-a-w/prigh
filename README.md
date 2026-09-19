# prigh

A coding agent: OCaml backend (Eio, cohttp, DeepSeek) and a TypeScript
terminal frontend. No plugins; tools and subagents are built in.

## Build

```
# backend (opam switch "prigh": OCaml 5.3, core, eio, cohttp-eio, jsonaf)
cd backend && eval $(opam env --switch=prigh) && dune build && dune build @runtest

# frontend
cd frontend && npm install --ignore-scripts && npm test
```

## Use

Set `DEEPSEEK_API_KEY` (or put `{"deepseek": "<key>"}` in
`~/.config/prigh/auth.json`), then:

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
  grep, find) and `Tool_subagent`, `Deepseek` provider, `Session` JSONL log,
  `Rpc_server`.
- `backend/test` — expect tests, driven by `Faux_provider` and an in-process
  HTTP server; no network.
- `frontend/src` — `client.ts` (RPC), `tui/` (editor, keys, render, app).
- `PLAN.md` — design and milestone status.
# prigh
