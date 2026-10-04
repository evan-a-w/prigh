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
[-pi-web-root DIR]` is the underlying command. The wrapper also sets
`PRIGH_WEB_ROOT`, so adding `-web HOST:PORT` makes the same backend serve the
Bonsai web UI on a second port, with both UIs sharing its sessions:

```
PRIGH_PI_WEB_LISTEN=0.0.0.0:7789 nix run ./pi-web -- -web 0.0.0.0:7777 -token sekrit -cwd ~/dev/prigh
```

The two UIs cannot share a port: pi-web's `/ws` speaks pi's protocol, the
Bonsai UI's `/ws` speaks prigh's, so pointing pi-web at a `-web` listener via
`?backend=` does not work. The page connects to its own
origin's `/ws`; `?backend=ws://host:port/ws` points it elsewhere,
`?session=ID` joins a session (the app keeps the current session id in the
address bar so a reload rejoins it), and `?user=` / `?token=` are remembered
in `localStorage` and stripped from the URL.

## Signing in

A refused connection shows a sign-in form with a **User name** and a
**Password**. The password is the server's token (`prigh serve -token`); the
user name picks the token namespace on servers that have them and is left
empty otherwise. Both are remembered in `localStorage`
(`prigh-pi-web:user`, `prigh-pi-web:token`) and sent as `user` and `token` in
the query string of the `/ws` and `/terminal` WebSocket URLs. Signing in as
a different user drops the previous user's `?session=` from the address bar.
While the password field has focus, a "Caps Lock is on" warning appears when
Caps Lock is active.

**Sign out** (top right, next to the signed-in user name; or `/signout`)
forgets the stored user name and password, closes the connection, drops
`?session=` and shows the sign-in form again. The other things pi-web keeps
in `localStorage` (theme, whether the agents panel is open) are browser
preferences, shared by all users.

## Provider (OAuth) login

`/login anthropic` (or `openai-codex`) opens one dialog with the
authorization link (opens in a new tab), a **Copy link** button and a field
for the code or the full redirect URL. The backend's separate pieces (the
`login` chat message carrying the link, the `auth-*` input prompt, the
progress/failure/success notifications) are folded into that dialog
(`provider-login.ts`): the link is not added to the chat, progress and
errors show in the dialog, it closes on success, and the prompt disappears
if the browser redirect delivers the code first.

## Development

```
cd pi-web
npm install
npm run check                 # tsc
npm test                      # vitest: connection.ts, sessions.ts, tool-args.ts, chat-items.ts,
                              #   the sign-in form and the provider login dialog (happy-dom)
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

Removed, because they need pi-server or a pi-only feature: the TUI view,
the file explorer, the subagents run-history panel,
the dashboard session list, snippets, the service worker.

Changed:

- `client.ts` — the user name, token, session and frontend name travel in
  the WebSocket URL's query string (`connection.ts`); `prigh_hello_failed`
  stops reconnecting and shows the sign-in form (`login-view.tsx`).
- `state.ts` — trimmed to the events the backend sends; sessions come from
  the backend's `list_sessions` / `switch_session` (prigh additions to the
  protocol) and fill the sidebar; `/new`, `/fork`, `/clone`, `/cd`,
  `/model`, `/thinking`, `/name`, `/session`, `/export`, `/copy`, `/compact`
  are handled client-side as in pi (plus `/signout`); `/login`, `/logout`, `/auth`,
  `/sessions`, `/switch`, `/host`, `/help` are sent as prompts and run by the
  backend, which answers with custom chat messages and `select`/`input`
  dialogs.
- `tool-execution.tsx` — diffs for prigh's `edit` (`edits: [{old_text,
  new_text}]`) and `write` (`content`) arguments (`tool-args.ts`).
- `sidebar.tsx` — prigh sessions (name / description / first prompt, age,
  message count), click to switch, "New".
- `terminal-panel.tsx` — the topbar's Terminal button opens the Bonsai web
  UI's terminal (`tui/web-bin/terminal.js` and its vendored xterm.js, copied
  into `dist/xterm/` by `vite.config.ts`; `$PRIGH_TERMINAL_ASSETS` overrides
  the directory) on the backend's `/terminal` WebSocket, keyed by the
  current session, so both UIs share one shell per session.

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
| `terminal_*` | not used: the panel talks to `/terminal` (`Terminals`) directly |
| `tui_*` | not supported |
| `message_update {message}` | `message_update {partial}` |
| `tool_execution_start/update/end` | `tool_start/output/end` (output accumulated) |
| `extension_ui_request confirm` | `tool_confirm` ↔ `tool_confirm_respond` |
| `extension_ui_request input/select`, `notify` | login prompts (`auth_respond`), notices, progress |
| `setWidget` `PI_SUBAGENT_ASYNC_JSON:` (agents rail) | `subagent_start` / nested `subagent` / `subagent_end` |
| `session_reloaded`, `session_info_changed`, `thinking_level_changed`, `agent_settled` | derived from `state` events |
| `compaction_end` | `compacted` |
