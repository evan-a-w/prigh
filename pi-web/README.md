# pi-web — pi's web UI on the prigh backend

An alternative browser frontend: a copy of [pi](https://github.com/earendil-works/pi)'s
`packages/web` (Preact + TypeScript, vite) pointed at the prigh backend. It is
completely separate from the Bonsai web frontend in `tui/web-app` and has its
own Nix flake.

The TypeScript still speaks **pi's RPC protocol** (one JSON object per
WebSocket frame: `prompt`, `get_state`, `message_update` with the accumulated
message, `tool_execution_*`, `extension_ui_request` dialogs, ...). The
translation to prigh's protocol happens in the backend, in OCaml
(`backend/lib/pi_rpc.ml` over `pi_protocol.ml`), which `prigh serve -pi-web`
exposes on its `/ws`. That keeps the frontend close to upstream and makes the
whole mapping an expect test (`backend/test/test_pi_rpc.ml`).

## Run

```
nix run ./pi-web -- -faux -cwd ~/proj     # from the repository root: backend + site, opens a browser
nix run ./pi-web -- -token sekrit          # any `prigh serve` options
nix build ./pi-web#site                    # just the static site (dist/)
nix flake check ./pi-web                   # type-check, unit tests, wrapper test, browser e2e (Linux)

./prigh -pi-web -faux                      # development: npm build + the dune-built backend
```

`PRIGH_PI_WEB_LISTEN` (default `127.0.0.1:7789`) and `PRIGH_PI_WEB_ROOT`
override the listen address and the assets; `prigh serve -pi-web HOST:PORT
[-pi-web-root DIR]` is the underlying command. The page connects to its own
origin's `/ws`; `?backend=ws://host:port/ws` points it elsewhere,
`?session=ID` joins a session (the app keeps the current session id in the
address bar so a reload rejoins it), `?token=` is remembered in
`localStorage` and stripped from the URL, and a refused connection shows a
connect form asking for the token.

## Development

```
cd pi-web
npm install
npm run check                 # tsc
npm test                      # vitest: connection.ts, sessions.ts, tool-args.ts, chat-items.ts
npm run build                 # dist/
npm run dev                   # vite dev server on :5173, proxies /ws to a backend on :7789:
                              #   ../backend/_build/default/bin/main.exe serve -pi-web 127.0.0.1:7789 -faux

PLAYWRIGHT_MODULE=.../playwright/index.mjs PLAYWRIGHT_BROWSERS_PATH=... ./e2e/pi_web.sh   # browser e2e
UPDATE=1 ./e2e/pi_web.sh      # re-record e2e/pi_web.expected
```

## What changed from pi's `packages/web`

Kept verbatim (or nearly): `style.css`, the chat list and message views,
markdown rendering, the editor with slash-command autocomplete, the model and
fork pickers, dialogs and toasts, the status strip, the agents rail, theme
loading (pi's `dark.json`/`light.json` are shipped in `public/theme/`).

Removed, because they need pi-server or a pi-only feature: the xterm
terminal and TUI views, the file explorer, the subagents run-history panel,
the dashboard session list, snippets, the service worker.

Changed:

- `client.ts` — the token, session and frontend name travel in the WebSocket
  URL's query string (`connection.ts`); `prigh_hello_failed` stops
  reconnecting and shows the connect form.
- `state.ts` — trimmed to the events the backend sends; sessions come from
  the backend's `list_sessions` / `switch_session` (prigh additions to the
  protocol) and fill the sidebar; `/new`, `/fork`, `/clone`, `/cd`,
  `/model`, `/thinking`, `/name`, `/session`, `/export`, `/copy`, `/compact`
  are handled client-side as in pi; `/login`, `/logout`, `/auth`,
  `/sessions`, `/switch`, `/host`, `/help` are sent as prompts and run by the
  backend, which answers with custom chat messages and `select`/`input`
  dialogs.
- `tool-execution.tsx` — diffs for prigh's `edit` (`edits: [{old_text,
  new_text}]`) and `write` (`content`) arguments (`tool-args.ts`).
- `sidebar.tsx` — prigh sessions (name / description / first prompt, age,
  message count), click to switch, "New".

## Backend mapping (summary)

| pi | prigh |
|---|---|
| `prompt` (`streamingBehavior: steer`/`followUp`) | `prompt` / `steer` / `follow_up` |
| `get_state`, `get_messages`, `get_session_stats` | `get_state`, `get_messages`, `session_stats` (messages get index timestamps; pi keys them by role + timestamp) |
| `set_model {provider, modelId}` | `set_model {model: provider/id}` |
| `set_thinking_level`, `cycle_thinking_level`, `get_available_thinking_levels` | `set_thinking` (`minimal`→`low`, `medium`→`on`, `xhigh`→`max`) |
| `bash` | `shell` (recorded in the session, so the transcript shows it) |
| `get_fork_messages`, `fork {entryId}` | `get_entries`, `fork {at: parent}` |
| `export_html` | `export {format: markdown}` |
| `terminal_*`, `tui_*` | not supported |
| `message_update {message}` | `message_update {partial}` |
| `tool_execution_start/update/end` | `tool_start/output/end` (output accumulated) |
| `extension_ui_request confirm` | `tool_confirm` ↔ `tool_confirm_respond` |
| `extension_ui_request input/select`, `notify` | login prompts (`auth_respond`), notices, progress |
| `setWidget` `PI_SUBAGENT_ASYNC_JSON:` (agents rail) | `subagent_start` / nested `subagent` / `subagent_end` |
| `session_reloaded`, `session_info_changed`, `thinking_level_changed`, `agent_settled` | derived from `state` events |
| `compaction_end` | `compacted` |
