# prigh architecture

prigh is an agentic coding harness split into an OCaml backend (`backend/`,
all the logic) and an OCaml frontend (`tui/`, Bonsai on OxCaml: rendering,
input and UI state only). There is no plugin system: tools, subagents,
providers and slash commands are compiled in.

```
 terminal ── frontend (prigh-tui, Bonsai_term) ── JSON lines on stdio ── backend `prigh serve` (OCaml, Eio)
                                                               │
                                                               ├─ Agent ── Agent_loop ── Provider_router ── Anthropic / Openai_responses / Deepseek ── HTTPS (SSE)
                                                               │                │                 └─ Provider_auth ── Auth_store (~/.config/prigh/auth.json)
                                                               │                └─ Tools (bash, read, write, edit, ls, grep, find, subagent)
                                                               ├─ Session (JSONL tree under ~/.prigh/sessions/)
                                                               └─ Login_manager ── Oauth_anthropic / Oauth_openai_codex ── loopback callback server + browser
```

## Process model and concurrency

The frontend spawns `prigh serve` and exchanges one JSON object per line over
stdin/stdout. Requests carry `{id, method, params}`; the backend answers with
`{type:"response", id, ok, result|error}` and pushes `{type:"event", event,
...}` for everything that happens asynchronously (streaming deltas, tool
output, state changes, login prompts). The same binary also has headless
subcommands (`run`, `sessions`, `login`, `logout`, `auth`) that use the same
library modules without the RPC layer.

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
- `Cancellation`, `Process` (streamed subprocess with timeout/kill),
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
  turns invalid arguments and exceptions into error results. `Tool_args`
  gives typed accessors and builds the JSON schema; `Truncate` bounds output
  by lines and bytes. `Tool_spec` carries the flags the harness reasons
  about: `parallel_safe` (safe to run concurrently with other tools) and
  `destructive` (held for confirmation when `confirm_tools` is on).
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
  `~/.prigh/AGENTS.md`.
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
  `Config`, the run lifecycle (`prompt`, `steer` = after the current turn,
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
  Entries are messages, model/thinking changes, compaction summaries, names
  and cwds (so a reload restores both); `Session.messages` is the message
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
- `Rpc_server` — reads request lines, dispatches to `Agent` and
  `Login_manager`, writes responses and events through a single outbox
  fiber. `set_model` goes through `Model.resolve` (key, id, display name or
  unique case-insensitive prefix; otherwise "did you mean" by edit
  distance). Methods: `ping`, `prompt`, `steer`, `follow_up`, `abort`,
  `dequeue`, `shell`, `get_state`, `get_messages`, `get_entries`, `set_model`,
  `set_thinking`, `list_models`, `compact`, `new_session`,
  `switch_session`, `list_sessions`, `set_session_name`, `delete_session`,
  `export`, `import`, `fork`, `clone`, `rewind`, `session_stats`, `set_cwd`,
  `get_config`, `set_config`, `tool_confirm_respond`, `auth_status`, `login`,
  `auth_respond`, `auth_cancel`, `logout`.

### CLI (`backend/bin/main.ml`)

`serve` (RPC), `run <prompt>` (headless, streams to stdout), `sessions`,
`login <provider> [-method]`, `logout <provider>`, `auth`. All commands share
`-auth-file`; `run`/`serve` share `-model`, `-thinking`, `-session`, `-cwd`,
`-no-tools`, `-faux` (and `-faux-script FILE`, a JSON array of scripted
replies that implies `-faux`). With no explicit model or session, the default
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
- `client/` (`prigh_client`) — `Transport.t` (line channel: `Stdio_transport`
  spawns the backend; an in-memory pair for tests; a websocket later) and
  `Client` (Async; correlates responses by id, fans out events, stderr and
  close on one `Incoming.t` pipe).
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
    spinner clock while a turn is active.
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
  including OSC-8 links), `Paths` (path completion via `fd`/`readdir`),
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
- `bin/` — `prigh-tui` (`-faux`, `-session`, `-model`, `-cwd`, `-auth-file`,
  `-backend`; `PRIGH_BACKEND` overrides the backend path).

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
3. **Protocol e2e.** `cd tui && dune build @e2e` runs the real
   `main.exe serve -faux` with an isolated `-auth-file` and `HOME` through
   the real client and diffs a normalised transcript.
4. **Real terminal.** `tui/tmux-test/run.sh` (alias `cd tui && dune build
   @tmux`; skipped when `tmux` is absent) starts the built TUI inside tmux,
   sends keys with `tmux send-keys`, and diffs captured panes against
   `expected/*.txt` (normalised for spinners and paths; `UPDATE=1`
   re-records). Scenarios: startup, prompt, ctrl_o, resize, quit, tools,
   suspend, editor, confirm, paste.

The last two layers are the only ones that exercise the real terminal, and
they paid for themselves immediately: the tmux layer caught `Ctrl+O` being
eaten by the tty's line discipline (fixed by clearing `IEXTEN`) and the quit
hang (`Driver.finished` never resolving), and the paste scenario caught the
batched-event buffering bug.
