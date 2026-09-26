# prigh architecture

prigh is an agentic coding harness split into an OCaml backend (`backend/`,
all the logic) and an OCaml frontend (`tui/`, Bonsai on OxCaml: rendering,
input and UI state only) that runs in a terminal or a browser. There is no
plugin system: tools, subagents, providers and slash commands are compiled in.

```
 terminal ── frontend (prigh-tui, Bonsai_term) ── JSON lines on stdio or TCP ── backend `prigh serve` (OCaml, Eio)
 browser ─── frontend (main.bc.js, Bonsai_web) ── JSON lines on a WebSocket ──┘  (`-web`: Web_server serves the page and /ws)
                │                                                      │
                └─ `prigh tool-host` (local tools)                     ├─ Rpc_server: clients ↔ sessions, tool hosts
                                                                       ├─ Agent (one per session) ── Agent_loop ── Provider_router ── Anthropic / Openai_responses / Deepseek ── HTTPS (SSE)
                                                                       │                │                 └─ Provider_auth ── Auth_store (~/.config/prigh/auth.json)
                                                                       │                └─ Tools (bash, read, write, edit, ls, grep, find, subagent) ── on the backend or a tool host
                                                                       ├─ Session (JSONL tree under ~/.prigh/sessions/)
                                                                       └─ Login_manager ── Oauth_anthropic / Oauth_openai_codex ── loopback callback server + browser
```

## Process model and concurrency

The frontend either spawns `prigh serve` and talks to it over stdin/stdout,
or connects over TCP to a backend started with `prigh serve -listen
HOST:PORT` (possibly on another machine). Either way it exchanges one JSON
object per line. Requests carry `{id, method, params}`; the backend answers
with `{type:"response", id, ok, result|error}` and pushes `{type:"event",
event, ...}` for everything that happens asynchronously (streaming deltas,
tool output, state changes, login prompts). The same binary also has headless
subcommands (`run`, `sessions`, `login`, `logout`, `auth`) that use the same
library modules without the RPC layer, and `tool-host`, the worker a
frontend spawns on its own machine to run tools there.

One backend serves many sessions and many clients. Every session is an
`Agent` (loaded on demand, dropped from memory when idle with no clients;
the JSONL file is the durable state) and every client is attached to exactly
one session at a time: its requests act on that session and it receives that
session's events. Several frontends can attach to the same session and each
sees the same stream. A session keeps running when its clients go away, and
a client sends `hello` first (name, cwd, whether it can run tools, an
optional session id or path, the `-token` if the backend requires one).

### Tool hosts

Each session has an *active tool host*: where its `on_host` tools (bash,
read, write, edit, ls, grep, find) and `!cmd` shells run. It is either the
backend itself or a connected client that advertised `tools: true` in
`hello`; the subagent tool always runs in the backend but its tool calls
follow the same active host. Tools default to the frontend: a tool-capable
client takes over when it attaches unless the user pinned a host with
`set_active_host` (`/host` in the TUI). The session cwd is a property of the
host, so switching hosts switches the cwd (and `/cd` validates the directory
on the host). When the active host is a client, `Agent.host_exec` emits a
`tool_exec` event to that client only and waits on a promise; the client
answers with `tool_exec_output` (streamed chunks, fanned out to everyone as
`tool_output`) and `tool_exec_result`; an abort sends `tool_exec_cancel`.
Disconnecting the active host fails its in-flight calls with `[tool host
disconnected]` and later calls with "not connected", so the run continues
and the model sees the error; the TUI shows `tools:offline` until another
host is chosen. The frontend does not implement any tools: it proxies
`tool_exec` to a local `prigh tool-host` process (`Tool_host` in the backend
runs `Host_ops.execute`, the same code path the backend uses for itself,
plus four pseudo-tools: `$resolve_dir` for `/cd` and `/host`, `$read_file`
for prompt attachments, `$list_paths` for `@` completion (`Path_listing`:
`fd` or a bounded `readdir` under the session cwd, so completion always
reflects the machine the tools run on), and `$instructions`, called when a session's system
prompt is first built and by every subagent, so that `AGENTS.md`/`CLAUDE.md`
come from the host's cwd ancestors and the host's own `~/.prigh/`).

The backend is direct-style Eio code. Every I/O function takes `~env`
(`Eio_unix.Stdenv.base`), and long-running work runs in fibers forked into a
switch: the RPC reader loop, the current agent run, and at most one login
flow. Cancellation is explicit rather than exception-based: a
`Cancellation.t` is a resolvable promise, and `Cancellation.protect` races
work against it with `Fiber.first`. HTTP requests, subprocesses and OAuth
prompts all accept a token, so an `abort` request cancels an in-flight model
stream or a running `bash` command immediately, and a login can be cancelled
while it is waiting on the browser.

## Backend layers (`backend/lib`)

### Foundations

- `Import` — `Json = Jsonaf`, Eio aliases, `Env.t`.
- `Cancellation`, `Process` (streamed subprocess with timeout/kill; the
  child leads its own process group and the whole group is killed, so a
  cancelled `bash` cannot leave grandchildren holding the output pipes),
  `Http_client` (cohttp-eio + ocaml-tls; `post` and streaming
  `post_stream`), `Sse` (incremental `text/event-stream` parser),
  `Sse_request` (streaming POST whose non-2xx bodies become
  `"HTTP <status>: <message>"`, the shape `Agent_loop` retries on).

### Model layer

- `Content` — assistant content blocks: `Text`, `Thinking {text;
  signature}`, `Tool_call {id; name; arguments}`. The thinking signature is
  provider-opaque (Anthropic block signatures, OpenAI encrypted reasoning
  items) and is echoed back only to the provider that produced it.
- `Message` — `User | Assistant | Tool_result`; assistant messages carry
  usage, stop reason and the `provider/id` key of the model that wrote them.
- `Assistant_event` / `Assistant_builder` — the streaming delta vocabulary
  (`Text_delta`, `Thinking_delta`, `Thinking_signature`, `Tool_call_start`,
  `Tool_call_delta`) and the accumulator that turns deltas into a message.
  Every provider emits these, so the loop, the session and the UI never see
  provider wire formats.
- `Model` — the static model table (id, provider, context window, max output,
  thinking support, prices). Ids may repeat across providers, so `Model.key`
  is `provider/id` and `Model.find` accepts either form.
- `Provider` — the provider interface: `stream : Request.t -> cancel ->
  on_event -> Message.Assistant.t`. Providers never raise for network or API
  failures; those come back as an `Error`/`Aborted` stop reason.

### Providers

Each provider module converts `Provider.Request.t` (model, system prompt,
messages, tool specs, thinking level) to its wire format, streams SSE, and
maps events back to `Assistant_event.t`:

- `Deepseek` — OpenAI-compatible chat completions (`reasoning_content`,
  index-based tool-call argument accumulation).
- `Anthropic` — Messages API. Consecutive same-role turns are merged, the
  last block and the system blocks carry cache breakpoints, thinking blocks
  are replayed only with a signature. With an OAuth token (Claude Pro/Max) the
  request is shaped like Claude Code's: Bearer auth, `claude-cli` user agent,
  `claude-code-20250219`/`oauth-2025-04-20` betas, the Claude Code identity
  as the first system block, and tool names in Claude Code's casing (mapped
  back to ours on `tool_use`).
- `Openai_responses` — Responses API for both `api.openai.com` (API key) and
  the ChatGPT Codex backend (`chatgpt.com/backend-api/codex/responses`, OAuth
  access token plus `chatgpt-account-id`). Requests use `store: false` and
  replay reasoning items through `encrypted_content`, stored verbatim as the
  thinking signature.
- `Provider_router` — the `Provider.t` the agent actually holds. On every
  request it looks at `request.model.provider`, resolves credentials through
  `Provider_auth` (refreshing OAuth tokens if needed) and delegates to the
  right backend, so logging in, out or switching models takes effect on the
  next request. Missing credentials become an `Error` stop reason naming the
  `/login` command.
- `Faux_provider` — a scripted provider for tests and `--faux` runs.

### Authentication

Modelled on pi's auth design; the credential file is the same shape so the
two can share one.

- `Credential` — `Api_key of string | Oauth {access; refresh; expires_ms;
  account_id}` with JSON in pi's format.
- `Auth_store` — `~/.config/prigh/auth.json`, one entry per provider,
  unknown providers preserved, atomic 0600 writes. Reads always go to disk;
  `modify` is the only write path and runs under an in-process mutex plus a
  cross-process `lockf`, so two prigh (or pi) processes cannot both rotate the
  same refresh token.
- `Provider_auth` — which methods each provider supports (`anthropic`: oauth,
  api_key; `openai`: api_key; `openai-codex`: oauth; `deepseek`: api_key),
  the environment variables consulted when nothing is stored, `login`,
  `logout`, `status`, and `resolve`. A stored credential owns the provider;
  environment fallback only happens when nothing is stored, and a failed
  refresh is an error rather than a silent fallback. OAuth refresh is
  double-checked: the cheap read decides whether to lock, the expiry is
  re-checked under the lock, and the rotated credential is persisted before
  release.
- `Auth_interaction` — how a flow talks to the user: `prompt` (secret,
  manual code, select) and `notify` (auth URL, progress), plus a cancel token.
  Implemented by `Auth_terminal` for the CLI and by `Login_manager` for the
  RPC client.
- `Pkce`, `Oauth_callback_server`, `Oauth_common` — S256 PKCE, a one-shot
  loopback HTTP server that receives the redirect and validates `state`, and
  the shared "race the callback against a pasted code" logic (`Fiber.first`;
  whichever finishes first cancels the other).
- `Oauth_anthropic` — claude.ai authorization with the Claude Code client id
  and scopes, `state = verifier`, callback on port 53692, JSON token exchange
  and refresh at platform.claude.com with a five-minute expiry margin.
- `Oauth_openai_codex` — auth.openai.com authorization with a random state,
  callback on port 1455, form-encoded exchange/refresh, ChatGPT account id
  extracted from the access token's JWT claims.
- `Login_manager` — runs at most one flow at a time in a background fiber and
  turns its prompts and notices into `auth` events; the client answers
  prompts by id (`auth_respond`) or cancels (`auth_cancel`). A prompt that the
  flow no longer needs (the browser callback won) is withdrawn with
  `prompt_cancelled`.

### Tools

- `Tool` — `{spec; run : Context.t -> Json.t -> Result.t}`; `Tool.execute`
  turns invalid arguments and exceptions into error results. The context
  carries an `execute : executor` hook (`Tool.execute_via`) through which
  the loop runs every call; the default runs in-process and `Agent`
  installs one that forwards `on_host` tools to the session's active host.
  `Tool_args` gives typed accessors and builds the JSON schema; `Truncate`
  bounds output by lines and bytes. `Tool_spec` carries the flags the
  harness reasons about: `parallel_safe` (safe to run concurrently with
  other tools), `destructive` (held for confirmation when `confirm_tools`
  is on) and `on_host` (runs on the active tool host rather than always in
  the backend).
- `Host_ops` — what a tool host does for a session: the `on_host` tools by
  name plus `$resolve_dir`/`$read_file`/`$list_paths`/`$instructions`. `Tool_host` is the `prigh tool-host`
  worker loop around it (`exec`/`cancel` in, `output`/`result` out, one fiber
  per exec).
- `Tool_bash` (streamed output, timeout, cancellation), `Tool_read`,
  `Tool_write` (`wrote N lines`), `Tool_edit` (multi-edit, unique
  non-overlapping matches, atomic; returns a unified diff from `Udiff`),
  `Tool_ls`, `Tool_grep`/`Tool_find` (via `rg`).
- `Tool_subagent` — one tool, no roles: the model passes `task` (required)
  plus optional `tools` (a subset of the parent's, default all), `model`
  (`Model.resolve`; default the parent's), `cwd`, `max_turns` (default 50)
  and `context` (extra system text). It runs a nested `Agent_loop`
  in-process sharing the parent's provider and cancellation, and returns the
  child's final text plus a `[subagent: N turns, in/out tokens, $cost]`
  trailer. Progress comes back as nested `Subagent*` events (below), not
  text chunks.
- `Tools.all` is the fixed built-in set; `Tools.for_context` builds the
  per-agent tool list (`parent`, `depth`, optional `only`), so the
  subagent's tool set can be restricted and the `subagent` tool is dropped at
  depth 2; unknown names are an argument error.

### Harness

- `System_prompt` — built-in guidance plus environment facts plus
  `AGENTS.md`/`CLAUDE.md` files from `/` down to the cwd and
  `~/.prigh/AGENTS.md`; `read_instructions` scans the local filesystem and
  `build ?instructions` accepts files fetched elsewhere (the tool host).
  `Agent` builds it once, at the session's first run, and records it as a
  `System_prompt` session entry so every later request (and every reload)
  sends the same prefix, which is what provider prompt caches key on. When
  the cwd or the active host changes afterwards, `host_changed_note` /
  `cwd_changed_note` are queued and prepended to the next user message: they
  say what changed and leave re-reading the instructions to the model's
  discretion, since they are often unchanged.
- `Agent_loop` — the core loop. Per turn: build the request from the context,
  stream the assistant message (emitting `Message_start/update/end`), retry
  with exponential backoff on retryable errors, execute the tool calls
  (`Tool_start/output/end`), append results, then poll `steer` for user
  messages to inject before the next turn. A turn whose calls are all
  `parallel_safe` runs them through `Eio.Fiber.List.map`; a mixed turn stays
  sequential, and results are appended in call order regardless of completion
  order so the context stays deterministic. Destructive tools block on a
  `Tool_confirm` event until `respond_confirm` when `confirm_tools` is set; a
  deny becomes an error result. Stops on `End_turn` without tool calls,
  `Length`, `Error`, `Aborted`, `max_turns` or cancellation. Every tool call
  always gets a result (a `[cancelled]` one if needed) so the context stays
  valid.
- `Agent_event` — the loop's event vocabulary (`Agent_start/end`, `Turn_*`,
  `Message_*`, `Tool_start/output/end`, `Tool_confirm`, and the recursive
  `Subagent` / `Subagent_start` / `Subagent_end`, whose inner events carry
  `call_id`/`agent_id`), so the UI can build a transcript per agent.
- `Agent` — one conversation: owns the session, model, thinking level and
  `Config`, the tool hosts (`add_host`/`remove_host`/`set_active_host`,
  `host_exec` and the pending remote executions), the run lifecycle (`prompt`, `steer` = after the current turn,
  `follow_up` = after the loop ends, `abort` = cancels and returns the queued
  texts to restore, `dequeue` = pops the last queued message, `shell` = runs
  a `!cmd` through the bash machinery), automatic compaction at 80% of the
  context window, and a subscriber list receiving `Agent.Event.t` (`Loop of
  Agent_event.t | State_changed | Compacted | Notice | Config_changed |
  Queue_update`). `prompt`/`steer`/`follow_up` accept optional `attachments`
  (paths whose contents are appended to the user message as `<file>` blocks).
  Subagent usage is rolled up into `State.usage`/`cost_usd`
  (never `context_tokens`), and `State` also carries the session name, cwd
  and `git_branch`. `respond_confirm` answers a pending `Tool_confirm`.
- `Config` — `~/.prigh/config.json` (`scoped_models : string list`,
  `confirm_tools : bool`), loaded at agent creation, read/written through
  `get_config`/`set_config`; unknown fields are ignored.
- `Session` — an append-only JSONL log forming a tree: every entry has a
  `parent`, the active conversation is the path from the root to `head`.
  Rewinding moves `head`; forking copies the active path to a new file.
  Entries are messages, model/thinking changes, compaction summaries, names,
  cwds (so a reload restores both) and the system prompt; `Session.messages` is the message
  list for the next request with the compaction summary replacing everything
  before `kept_from`. `list` returns name, cwd, timestamps, message count,
  first prompt and parent; `export` writes markdown or copies the JSONL,
  `import` copies a file in, and `session_stats` counts turns, tool calls by
  name, tokens, cost, model changes and compactions. Under the RPC,
  `get_entries` returns `{head, entries}` (`all: true` includes abandoned
  branches for the tree view).
- `Compaction` — summarises older messages via the model and keeps a tail;
  manual (`/compact`) or automatic.

### RPC

- `Rpc_json` — the wire encoding (plain tagged objects, not the derived
  `["Ctor", ...]` form) for messages, deltas, state, models, sessions,
  auth status and events.
- `Rpc_server` — the connection and session manager: a table of live
  agents by session id, a table of clients, and per-connection
  `serve_lines` (`serve_connection` over newline-delimited flows, or one
  WebSocket text message per line: reader loop, one outbox fiber, and one fiber per
  request so a blocking method such as `shell` or a remote tool round trip
  never stalls the reader that must deliver the client's own
  `tool_exec_result`). Agent events are routed to the clients attached to
  that agent, except `tool_exec`/`tool_exec_cancel`, which go to the named
  host only; login events go to everyone. The session methods
  (`new_session`, `switch_session` by id or path, `fork`, `clone`, `import`)
  create or load an agent and move only the calling client; `list_sessions`
  marks live sessions with `live`, `running` and `clients`; `delete_session`
  refuses live ones. `set_model` goes through `Model.resolve` (key, id,
  display name or unique case-insensitive prefix; otherwise "did you mean"
  by edit distance). Methods: `hello`, `ping`, `prompt`, `steer`,
  `follow_up`, `abort`, `dequeue`, `shell`, `get_state`, `get_messages`,
  `get_entries`, `set_model`, `set_thinking`, `list_models`, `compact`,
  `new_session`, `switch_session`, `list_sessions`, `set_session_name`,
  `delete_session`, `export`, `import`, `fork`, `clone`, `rewind`,
  `session_stats`, `set_cwd`, `list_paths`, `get_config`, `set_config`,
  `tool_confirm_respond`, `set_active_host`, `tool_exec_output`,
  `tool_exec_result`, `auth_status`, `login`, `auth_respond`, `auth_cancel`,
  `logout`. `State` carries `active_host` and `hosts` (the backend first).
- `Websocket` — a minimal RFC 6455 server side (handshake key, frame
  encode/decode with client masking, fragment reassembly, ping/pong and
  close) and `Web_server` — the `-web` listener: a connection whose first
  byte is `{` is a plain JSON-lines client (the TUI's `-connect`) and goes
  straight to `Rpc_server.serve_lines`, so one port serves terminals and
  browsers alike; otherwise one HTTP/1.1 request per connection, `GET /ws`
  upgraded and handed to `Rpc_server.serve_lines`,
  anything else served from the web root (the built `tui/web-bin/site`,
  found via `-web-root`, `$PRIGH_WEB_ROOT` or next to the executable;
  no `..`, no dot files). The token check is the same `hello` check as
  for TCP; the static files are public.

### CLI (`backend/bin/main.ml`)

`serve` (RPC on stdio; `-listen HOST:PORT` accepts TCP clients instead,
`-web HOST:PORT` serves the browser frontend, WebSocket clients and TCP
`-connect` clients on one port (`-open` launches a browser, `-web-root DIR`
overrides the assets), `-stdio` as well,
`-token SECRET`/`$PRIGH_TOKEN` gates them; with stdio the backend exits when
the spawning frontend closes it, with `-listen`/`-web` only it runs until
killed), `tool-host` (the local tool worker), `run <prompt>`
(headless, streams to stdout), `sessions`, `login <provider> [-method]`,
`logout <provider>`, `auth`. All commands share `-auth-file`; `run`/`serve`
share `-model`, `-thinking`, `-session`, `-cwd`, `-no-tools`, `-faux` (and
`-faux-script FILE`, a JSON array of scripted replies that implies `-faux`);
for `serve` these describe the default session, the one a client lands on
when its `hello` names none. With no explicit model or session, the default
model is the first logged-in provider's in the order anthropic, openai-codex,
openai, deepseek.

## Frontend (`tui/`)

Built in the `prigh-ox` opam switch (OxCaml 5.2 with the Jane Street
`v0.18~preview` packages) or under `nix develop`; the backend stays on the
vanilla `prigh` switch because some of its dependencies do not compile with
OxCaml modes. The two only meet over the wire, so the frontend owns its own
copy of the protocol types and the e2e test guards the contract.

- `protocol/` (`prigh_protocol`) — `Jsonaf` decoders for everything
  `Rpc_json` emits (`Message`, `Delta`, `State`, `Model`, `Session_summary`,
  `Auth_status`, `Auth_event`, `Event`, `Server_message`) and the `Request`
  encoder.
- `client/` (`prigh_client`, `Async_kernel` only so it links under
  js_of_ocaml) — `Transport.t` (a line channel; an in-memory pair for
  tests) and `Client` (created with a `connect` thunk and
  reconnectable: correlates responses by id, fans out events, stderr and
  `Closed` on one `Incoming.t` pipe that outlives the transport, fails calls
  with "not connected" in between). `client_unix/` (`prigh_client_unix`)
  has the transports that need a process or a socket — `Stdio_transport`
  spawns the backend, `Tcp_transport` connects to `-listen` — and
  `Tool_host` (spawns `prigh tool-host` lazily and proxies
  `Tool_exec`/`Tool_exec_cancel` events to it and its `output`/`result`
  lines back as `tool_exec_*` requests).
- `ui/` (`prigh_ui`) — **platform-agnostic**, depends only on `core` and
  `bonsai`; it is what both the terminal and a future web frontend mount.
  - `App` is an Elm-style pure state machine: `update : Model.t -> Action.t
    -> Model.t * Command.t list`. Actions are keys/intents, backend events,
    RPC replies (tagged with `Reply_tag.t` so they stay sexpable), clock
    ticks and resizes. Commands (`Rpc`, `List_paths`, `Load_history`,
    `Append_history`, `Copy_to_clipboard`, `Suspend`, `Edit_externally`,
    `Open_browser`, `Quit`) are executed by the platform and answered through
    `Action.Reply`.
  - `Component.create ~platform` wraps `App` in `Bonsai.state_machine`,
    turns commands into effects and feeds replies back; it also runs the
    spinner clock while a turn is active. `@` path completion is an RPC
    (`list_paths`), answered by the active tool host, so both platforms
    complete the same paths.
  - Reconnection is App state (`Connection.t`), so the policy is an expect
    test: `Backend_closed` emits `Command.Reconnect {generation; delay_ms;
    session}` (immediately, then 250ms doubling to a 10s cap), the platform
    sleeps, connects and re-sends `hello` for the current session, and the
    reply comes back tagged with its generation so late attempts are ignored;
    `/retry-backend-connection` issues a new generation with no delay.
    Success clears the transcript and re-runs the startup requests.
  - Headless widgets: `Editor` (multi-line, kill ring, undo, chips for long
    pastes, persistent history), `Picker` (fuzzy list with `Fuzzy` ranking),
    `Transcript` (items plus streaming tails; `Transcript.apply` is the one
    event→transcript function, used for the main transcript and each
    subagent), `Viewport` (`Follow | Anchored`, so new output never pushes an
    anchored view), `Verbosity` (quiet/normal/verbose), `Autocomplete`
    (inline command/argument/path completion), `Agent_view` (per-subagent
    transcript and status), `Commands` (slash table, parse, complete,
    closest), `Model_match` (display-name/prefix/did-you-mean), `Markdown`.
  - `Key.t` → `Intent.t` through `Keymap` (the one binding table; `/help`
    prints it). `Mode.t` (`Editing | Picker | Login_prompt | Text_prompt |
    Confirm | Search`) says who owns the keyboard; dialogs never stack, Esc
    always closes.
  - `Render.screen : Model.t -> Screen.t` lays out a frame as `Content.t`
    (styled spans with `Text_width`-aware wrapping) plus the cursor cell. The
    status line keeps the cwd and model, then fills remaining width by
    priority (context, cost/queued/agents, thinking, verbosity, new-line
    count, mode hint) and left-truncates, so the model key stays visible at
    narrow widths.
- `term/` (`prigh_ui_term`) — `Key_of_event` (Bonsai_term events → `Key.t`,
  bracketed paste → one `Insert`), `View_of_content` (spans → notty attrs,
  including OSC-8 links),
  `Tty`/`tty_stubs.c` (clears `IEXTEN`), and `Term_app` (spawns the backend,
  runs `Bonsai_term.start_with_driver`, pushes client `Incoming.t` into the
  component, executes the platform commands, sets the cursor). Three
  terminal-only details live here. Notty's raw mode leaves `IEXTEN` set, so
  the line discipline eats `^O` before the program sees it; the stub clears
  the flag around the driver (OCaml's `terminal_io` cannot express it, so
  notty cannot restore it either). Bonsai_term's `Driver.finished` ivar is
  not filled when `exit` is scheduled from `apply_action`, so `Term_app`
  tracks the quit itself. Suspend (`Ctrl+Z`) and the external editor leave
  and re-enter the alt screen; notty still believes its last frame is on
  screen, so `install_repaint` re-emits it. Bracketed paste is buffered in a
  plain `ref`, not Bonsai state, because the handler receives a whole batch
  of events at once and state would only update after the frame, so every key
  in the batch would still see `Idle`.
  `Term_app.run` sends `hello` before mounting the app and feeds the client
  id back as `Set_client_id`, so `/host` can mark this frontend as "(here)"
  and the status line can show `tools:<host>` when tools run elsewhere or
  `tools:offline` when the active host is gone. Mouse reporting is on so the
  wheel scrolls the transcript (`Scroll_up`/`Scroll_down`, three lines);
  without it the terminal turns the wheel into arrow keys, which walk the
  prompt history. Text selection therefore needs Shift.
- `web/` (`prigh_ui_web`, pure: `core` + `virtual_dom`) — `Dom_of_screen`
  renders a `Screen.t` as a monospace cell grid (`pre.screen` > `div.line` >
  spans with style classes, links as anchors, the cursor cell in
  `span.cursor`; `style.css` colours it) and `Key_of_dom` maps a browser
  `keydown` (`key`, `code`, modifiers) to `Key.t`, using the physical `code`
  for Alt/Ctrl combinations (Option on a Mac produces symbols) and leaving
  paste and Meta shortcuts to the browser. A test checks every keymap
  binding is producible from a browser event.
- `web-app/` (`prigh_ui_web_app`) — the page: `Ws_transport` (a browser
  WebSocket as a `Transport.t`), `Browser` (localStorage, query string,
  clipboard, the cell measurement that turns the window into columns and
  rows) and `Web_app`, the counterpart of `Term_app`: reads
  `?backend=`/`?token=`/`?session=`/`?name=` (using the page's origin `/ws`
  when no backend is explicit), sends `hello` with
  `tools: false`, mounts the shared component with
  `Bonsai_web.Start.start_and_get_handle` (incoming actions through the
  handle), installs document-level `keydown`/`paste`/`wheel`/`resize`
  listeners, and implements the platform: history in `localStorage`,
  `navigator.clipboard`, `window.open`; suspend and the external editor
  report themselves unavailable. A failed first `hello` shows a connect
  form instead (the token is saved and the selected backend is put in the
  reloaded page's query string). `web-bin/` is
  the js_of_ocaml executable plus `index.html`/`style.css`, assembled
  under `web-bin/site/` and installed to `share/prigh_tui/web`.
- `bin/` — `prigh-tui` (`-faux`, `-session`, `-model`, `-cwd`, `-auth-file`,
  `-backend`; `PRIGH_BACKEND` overrides the backend path). `-connect
  HOST:PORT` (`$PRIGH_CONNECT`) joins a running backend instead of spawning
  one, with `-token` (`$PRIGH_TOKEN`) and `-name`; `-tools local|remote`
  says where this session's tools run (local = this machine through
  `tool-host`, the default with `-connect`; remote = the backend, the default
  when spawning, where the two coincide).

## Data on disk

- `~/.config/prigh/auth.json` — credentials (pi-compatible).
- `~/.prigh/sessions/<stamp>_<id>.jsonl` — session logs.
- `~/.prigh/sessions/exports/` — default `/export` output.
- `~/.prigh/history` — prompt history (one JSON string per line, last 500).
- `~/.prigh/config.json` — `scoped_models`, `confirm_tools`.
- `~/.prigh/AGENTS.md` — global instructions.

## Testing

Four layers, cheapest first.

1. **Pure expect tests.** Backend (`cd backend && dune build @runtest`)
   drives the loop/agent/RPC with `Faux_provider` and
   providers/HTTP/OAuth with `Fake_http_server` (an in-process Eio HTTP
   server); nothing touches the network. Frontend (`cd tui && dune build
   @runtest`) covers protocol decoding, the client over an in-memory
   transport, the widgets and the keymap, and `App.update` scenarios that
   print the screen (`Screen.to_plain`) — pickers, login prompts, Esc/Ctrl+C,
   streaming, resize, scrolling, subagents. `test_component` runs the Bonsai
   component under `Bonsai_test.Handle` with a scripted platform.
2. **Rendered-frame snapshots.** `tui/test/test_term_frames.ml` (same
   `@runtest`) drives the real Bonsai_term driver over an in-memory tty,
   decodes Notty's output with the VT emulator in `tui/test/vt.ml`, and
   compares the grid and cursor with `Screen.to_plain`, so
   `View_of_content`/`Key_of_event` cannot diverge from the pure renderer.
3. **Protocol e2e.** `tui/e2e` (in `@runtest`, also `@e2e`) runs the real
   `main.exe serve -faux` with an isolated `-auth-file` and `HOME` through
   the real client and diffs a normalised transcript.
4. **Real terminal.** The cram test `tui/tmux-test/run.t` (in `@runtest`;
   skipped when `tmux` is absent) runs each scenario of `run.t/harness.sh`:
   the built TUI inside a detached tmux session, keys sent with `tmux
   send-keys`, panes captured and normalised (spinners, paths) into the
   cram output, so `dune promote` re-records. Scenarios: startup, prompt,
   ctrl_o, resize, quit, tools, suspend, editor, confirm, paste, reconnect
   (the backend is killed; the TUI respawns it and rejoins the session).

The last two layers are the only ones that exercise the real terminal, and
they paid for themselves immediately: the tmux layer caught `Ctrl+O` being
eaten by the tty's line discipline (fixed by clearing `IEXTEN`) and the quit
hang (`Driver.finished` never resolving), and the paste scenario caught the
batched-event buffering bug.

The web layer adds two: `backend/test/test_web.ml` (frames, fragmentation,
the RFC handshake vector, static serving and traversal, browser URL/token
encoding, and a masked WebSocket RPC conversation over a real loopback socket)
and `tui/test-web/` (connection selection, `Key_of_dom` and `Dom_of_screen`, run under `node` with
js_of_ocaml because `virtual_dom`'s initialisers need a JavaScript runtime;
skipped without `node`). On Linux, the flake's `web-browser` check starts the
packaged wrapper and `serve -faux -web`, captures the URL sent to the browser,
checks that its token is encoded but not logged, fetches the installed bundle,
and loads it in headless Chromium through the real WebSocket until the session
screen replaces `connecting…`. Additional interactions (prompt, `/help`,
pickers, `@` completion, `!` shell, paste, resize, the connect form,
`?backend=` to a second backend, quit and reconnect after a backend restart)
have been exercised manually.
