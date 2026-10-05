# prigh architecture

prigh is an agentic coding harness split into an OCaml backend (`backend/`,
all the logic) and an OCaml frontend (`tui/`, Bonsai on OxCaml: rendering,
input and UI state only) that runs in a terminal or a browser. There is no
plugin system: tools, subagents, providers and slash commands are compiled in;
what comes from outside is skills (instructions in `SKILL.md` files) and the
tools of MCP servers.

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
optional session id or path, the `-token` if the backend requires one, and
with `-tokens` optionally the user, i.e. the namespace name).

### Tool hosts

Each session has an *active tool host*: where its `on_host` tools (bash,
read, write, edit, ls, grep, find) and `!cmd` shells run. It is either the
backend itself or a connected client that advertised `tools: true` in
`hello` (a TUI, or a standalone `prigh tool-host -connect`); the subagent
tool always runs in the backend but its tool calls follow the same active
host. With `-no-backend-host` the backend is not a host at all (not listed,
no in-process tools, instructions, path listings or git branch), and a
session without a host adopts the first one that connects. Tools default to the frontend: a tool-capable
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
plus five pseudo-tools: `$resolve_dir` for `/cd` and `/host`, `$read_file`
for prompt attachments, `$list_paths` for `@` completion (`Path_listing`:
`fd` or a bounded `readdir` under the session cwd, so completion always
reflects the machine the tools run on), `$list_dirs` for the directory
completion in `/cd` and the `/host` prompt (one level, in the notation
typed: absolute, `~/` or relative; `list_dirs` takes a `host` so the `/host`
prompt completes on the host being switched to), and `$instructions`, called when a session's system
prompt is first built and by every subagent, so that `AGENTS.md`/`CLAUDE.md`
come from the host's cwd ancestors and the host's own `~/.prigh/`; asked
`with_nix`, it also says whether `nix` is on the host's PATH, and
`instructions_of_result` still accepts the bare array of files that older
hosts reply with).

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
  User messages and tool results carry `images` (`Image.t`: MIME type and
  base64), as do `Tool_result.t`s; in sessions and on the wire the field is
  omitted when empty.
- `Image` — what models accept: PNG/JPEG/GIF/WebP sniffed from the bytes,
  dimensions from the header, and `load`, which downscales anything over
  2000 px or 4.5 MB of base64 by running ImageMagick or `sips` (PNG, then
  JPEG at falling quality and size) and adds a note with the scale factor
  for the model; without one, images within the hard limits go as they are.
- `Assistant_event` / `Assistant_builder` — the streaming delta vocabulary
  (`Text_delta`, `Thinking_delta`, `Thinking_signature`, `Tool_call_start`,
  `Tool_call_delta`) and the accumulator that turns deltas into a message.
  Every provider emits these, so the loop, the session and the UI never see
  provider wire formats.
- `Provider_id` — `Anthropic | Openai | Openai_codex | Deepseek | Custom of
  string`: a custom provider is named by the user, so `of_builtin_string`
  only knows the built-in names and custom ones are looked up in the
  `Model_registry`.
- `Model` — the static model table (id, provider, context window, max output,
  thinking support, prices). Ids may repeat across providers, so `Model.key`
  is `provider/id` and `Model.find` accepts either form.
- `Custom_provider` — a user-defined endpoint from `config.json`'s
  `providers` (`base_url`, `api`: `chat`/`responses`/`anthropic`, extra
  `headers`, per-model overrides of context, output, thinking, images and
  cost); loading reports each problem with what to fix, saving rewrites only
  its entry; name and base URL validation; `<NAME>_API_KEY`.
- `Model_registry` — the models a namespace can use: `Model.all` plus each
  custom provider's `GET {base_url}/models` (fetched in the background at
  start, when the config changes and after `/login`; cached in memory)
  merged with its overrides. `reload` rereads the config (`list_models`
  calls it, so hand edits show); `resolve` accepts an unlisted
  `<custom>/<id>` only while that provider's list is unknown. Problems (bad
  entries, failed fetches) go to subscribers, which the RPC server turns
  into one notice per client.
- `Provider` — the provider interface: `stream : Request.t -> cancel ->
  on_event -> Message.Assistant.t`. Providers never raise for network or API
  failures; those come back as an `Error`/`Aborted` stop reason.

### Providers

Each provider module converts `Provider.Request.t` (model, system prompt,
messages, tool specs, thinking level) to its wire format, streams SSE, and
maps events back to `Assistant_event.t`. Images follow the text: Anthropic
`image` blocks (base64 source; a tool result's `content` becomes a block
list), OpenAI `input_image` data URLs (a `function_call_output`'s `output`
becomes a list):

- `Openai_chat` — OpenAI-compatible chat completions, for DeepSeek and for
  custom providers with `api: chat`. Tool calls are accumulated by `index`,
  tolerating servers that repeat the id and name in every delta or omit the
  index or id; thinking arrives as `reasoning_content` or `reasoning`.
  Images are `image_url` data-URL parts; a tool message is text only, so a
  tool result's images follow the run of tool messages in a user message.
  Per-server differences are `Quirks`: how the thinking level is sent
  (DeepSeek's `thinking: {type}` plus `reasoning_effort`, or OpenAI's
  `reasoning_effort`) and whether earlier thinking is replayed as
  `reasoning_content` (DeepSeek needs it on tool-call turns).
- `Deepseek` — `Openai_chat` with DeepSeek's URL and quirks. Its models take
  no images: each becomes a line saying it was omitted.
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
  `/login` command. A custom provider is built per request from its
  `Model_registry` entry: `Openai_chat` (generic quirks), `Openai_responses`
  or `Anthropic` at its base URL with its headers and optional key (Bearer;
  `x-api-key` too for Anthropic); a `401`/`403` from it names `/login
  <name>` (and the environment variable when no key was sent).
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
  possibly allowed to be empty; text with a placeholder and a prefilled
  default, where an empty answer means the default for frontends that cannot
  prefill; manual code; select) and `notify` (auth URL, progress), plus a
  cancel token.
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
  `prompt_cancelled`. `start_custom` runs `Custom_login.login`; logging out
  of a configured custom provider is a flow too (one select), ending in
  `logged_out`, or in nothing when the user keeps everything.
- `Custom_login` — `/login custom` through `Auth_interaction` prompts, so
  every frontend runs it: name (text; an existing custom name edits it),
  base URL (text, prefilled when editing), API style (select), key (secret
  that may be empty; with a stored key, a keep/new/remove select). An
  invalid answer is asked again with the error first and the answer
  prefilled. It lists `GET /models` and reports the count, or offers save
  anyway / change the settings / cancel; only then does it write the
  definition (`config.json`) and the key (`auth.json`) and tell the
  registry. `logout` asks whether to remove the key only or the provider
  too.

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
  name plus `$resolve_dir`/`$read_file`/`$list_paths`/`$instructions` (whose
  reply also lists the host's skills), `$skill` (one skill's file, for
  `/skill:`) and `$mcp_servers`/`$mcp_call`/`$mcp_approve` (the host's
  `Mcp_hub`). Every `$` op honours a `home` argument only in the backend:
  `Tool_host`, the `prigh tool-host` worker loop around it (`exec`/`cancel`
  in, `output`/`result` out, one fiber per exec), strips it so a remote host
  uses its own home. A tool host predating an op answers "unknown host tool",
  which the backend treats as having nothing (no skills, no MCP servers).
- `Frontmatter`, `Skill` — a `SKILL.md`'s YAML frontmatter (the scalar forms
  skill files use), discovery (`.prigh/skills`, `.claude/skills`,
  `.agents/skills` in the cwd, its ancestors, then the home directory;
  closest wins; nested a few levels), the system prompt's section, and
  `/skill:NAME ARGS` expansion into a `<skill name location>` block followed
  by ARGS.
- MCP — `Mcp_config` reads `mcpServers` from the host's `~/.prigh/mcp.json`
  and `.mcp.json` in the cwd and its ancestors (closest wins; `${VAR}` and
  `${VAR:-default}`), and keeps approvals of project servers (by a digest of
  the definition as written) in `~/.prigh/mcp-approvals.json`. `Mcp_client`
  is one connection (stdio child in its own process group, or streamable
  HTTP; `initialize`, `tools/list`, `tools/call`, cancellation, ping).
  `Mcp_hub` holds a host's running servers, shared by all its sessions: one
  per tool-host process, and one per namespace in the backend (none with
  `-no-tools`, which turns MCP off for every host). A server starts the first
  time a session needs it, keyed by its definition (so an edit starts a new
  one); a failed start is remembered until `reconnect`. `Mcp_tools` is the
  `$mcp_servers` wire format and turns a listing into prigh tools named
  `mcp__<server>__<tool>` (`parallel_safe` when `readOnlyHint`, otherwise
  `destructive`), whose `run` sends `$mcp_call {source, server, tool}` to the
  session's active host. `Agent` asks the host for the listing at the start
  of every run (so tools follow host and cwd changes), adds the tools to the
  run's and its subagents' tool lists, and reports each problem once as a
  `Notice`.
- `Tool_bash` (streamed output, timeout, cancellation; with `background`
  at depth 0 it starts a job instead, see below), `Tool_read` (images
  through `Image.load`, as an image result),
  `Tool_write` (`wrote N lines`), `Tool_edit` (multi-edit, unique
  non-overlapping matches, atomic; returns a unified diff from `Udiff`),
  `Tool_ls`, `Tool_grep`/`Tool_find` (via `rg`).
- `Tool_subagent` — one tool, no roles: the model passes `task` (required)
  plus optional `tools` (a subset of the parent's, default all), `model`
  (`Model.resolve`; default the parent's), `cwd`, `max_turns` (default 50)
  and `context` (extra system text). It runs a nested `Agent_loop`
  in-process sharing the parent's provider, whose report is the child's final
  text plus a `[subagent: N turns, in/out tokens, $cost]` trailer. Progress
  comes back as nested `Subagent*` events (below), not text chunks. At depth 0
  the context carries the agent's `Background_tasks` and the call only spawns
  the loop there, with its own cancellation, and returns `started agent
  a<n> (...)`; deeper calls (no jobs) block and return the report.
  `subagent_wait`/`subagent_status`/`subagent_cancel` act on the jobs and are
  dropped below depth 0.
- `Background_tasks` — one agent's background work, of two kinds:
  subagents (ids `a<n>`, seeded past the subagent calls already in the
  session) and shell jobs (ids `j<n>`, seeded past the `started job j<n>`
  results and `[job j<n> ...]` reports already in it). Each task has its
  label (task or command), start time, its own cancellation, last activity
  (subagents, from their events), an `Output_tail` (jobs: the last 1 MB of
  output and the total byte count), an outcome (`status` such as `finished`,
  `exited 2`, `killed`, `failed: ...`, a body and `is_error`) and a
  `delivered` flag. Each finished task's report (`[<kind> <id> <status>]
  <label>` and the body) is delivered exactly once, by `take_undelivered`
  (the agent turns a batch, subagents and jobs together, into one user
  message) or by `wait`/`cancel_and_wait` (tool results). Events and changes
  go to the hooks `Agent` installs with `connect`.
- Background jobs — `bash` with `background: true` at depth 0 spawns a job
  whose fiber runs the same call in the foreground (`background` dropped,
  no timeout unless one was given) through the context's executor, so it
  runs on the session's active host exactly like a normal call: in-process,
  or as a `Tool_exec` round trip that the backend keeps pending in the job's
  fiber while the turn goes on, its streamed `tool_exec_output` chunks
  feeding the job's buffer (`Agent.executor` runs the spawning call itself in
  the backend, never on the host). Killing a job cancels that call, which
  kills the process group (locally, or on the host via `tool_exec_cancel`); a
  host disconnecting fails it. The outcome's status is read from the
  foreground result's trailer (`[exit code N]`, `[cancelled]`, `[killed by
  ...]`, `[timed out ...]`), its body is the last 40 lines of output.
  `Tool_jobs` has `job_status`, `job_output` (`lines`, `offset` from the
  end), `job_wait` and `job_kill`; like the subagent controls they and
  `background` itself are dropped below depth 0.
- `Tools.all` is the fixed built-in set; `Tools.for_context` builds the
  per-agent tool list (`parent`, `depth`, optional `only`), so the
  subagent's tool set can be restricted and the `subagent` tool is dropped at
  depth 2; unknown names are an argument error.

### Harness

- `System_prompt` — built-in guidance plus environment facts plus
  `AGENTS.md`/`CLAUDE.md` files from `/` down to the cwd and
  `~/.prigh/AGENTS.md`; `read_instructions` scans the local filesystem and
  `build ?instructions ?nix ?skills` accepts files fetched elsewhere (the
  tool host), whether that host has Nix, which adds how to get missing tools
  from nixpkgs, and the host's skills, listed when the model has `read`.
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
  `call_id`/`agent_id`), so the UI can build a transcript per agent. A
  background subagent's events keep arriving after its `subagent` call's
  `Tool_end`, possibly during later turns or while the agent is idle.
- `Agent` — one conversation: owns the session, model, thinking level and
  `Config`, the tool hosts (`add_host`/`remove_host`/`set_active_host`,
  `host_exec` and the pending remote executions), the run lifecycle (`prompt`, `steer` = after the current turn,
  `follow_up` = after the loop ends; `/skill:NAME ARGS` texts are expanded
  through the host's `$skill` when queued, so an unknown skill fails the
  request, and the queue keeps the typed text, `abort` = cancels and returns the queued
  texts to restore, `dequeue` = pops the last queued message, `shell` = runs
  a `!cmd` through the bash machinery), automatic compaction at 80% of the
  context window, and a subscriber list receiving `Agent.Event.t` (`Loop of
  Agent_event.t | State_changed | Compacted | Notice | Config_changed |
  Queue_update`). Background subagents and jobs (`Background_tasks`) run in
  the agent's switch, not the run's: `abort` leaves them running,
  `cancel_subagent`/`kill_job` stop one, `start_job` starts a job for a
  user's `!&cmd`. When one finishes, an idle agent starts a run whose prompts
  begin with the delivery message; a running loop gets it from `steer` at
  the next turn boundary (after the turn's tool results, so tool calls and
  results stay paired), or the run's tail starts a new run unless it was
  aborted (then the report waits for the next prompt, which it precedes).
  `wait_idle` also waits for running subagents and jobs and their
  deliveries (so headless `run` does too); `State.subagents` and
  `State.jobs` list the running and undelivered ones; `Subagent_log`
  records every subagent's status, activity and transcript (nested ones as
  `<parent>/<call id>`) from the agent's own events, for `subagents`/
  `subagent` (`list_subagents`, `get_subagent {id}` by agent or tool call
  id); `queued_texts` and `pending_confirms` back `get_pending`; the in-place
  `new_session`/`switch_session` cancel them and drop their reports.
  `prompt`/`steer`/`follow_up` accept optional `attachments`
  (paths whose contents are appended to the user message as `<file>` blocks;
  image files are read like `read` does and attached as images) and
  `images`.
  Subagent and `btw` usage is rolled up into `State.usage`/`cost_usd`
  (never `context_tokens`), and `State` also carries the session name, cwd
  and `git_branch`. `respond_confirm` answers a pending `Tool_confirm`.
- `Config` — `~/.prigh/config.json` (`scoped_models : string list`,
  `confirm_tools : bool`, `default_model`/`default_thinking`), loaded at
  agent creation, read/written through `get_config`/`set_config`; unknown
  fields are ignored. `Agent.save_as_default` (`change_default`,
  `/change_default`) rereads the file and records the agent's current model
  and thinking level there; agents created without an explicit `-model`/
  `-thinking` start from them (a loaded session's own settings still win).
- `Session` — an append-only JSONL log forming a tree: every entry has a
  `parent` and `at`, when it was appended (seconds; optional, so files
  written before it load, without times; a fork keeps the copied entries'
  times), and the active conversation is the path from the root to `head`.
  Rewinding moves `head`; forking copies the active path to a new file.
  Entries are messages, model/thinking changes, compaction summaries, names,
  descriptions, cwds (so a reload restores both) and the system prompt;
  `Session.messages` is the message list for the next request with the
  compaction summary replacing everything before `kept_from`
  (`timed_messages`: each with its entry's time, as a `Timed_message.t`;
  the summary has none). Nothing is
  written to disk until the first message (or name): an abandoned empty
  session leaves no file. `list` returns name, description, cwd,
  timestamps, message count, first prompt and parent, most recently
  modified first; `export` writes markdown or copies the JSONL,
  `import` copies a file in (the markdown has each message's time, in UTC,
  in its heading), and `session_stats` counts turns, tool calls by
  name, tokens, cost, model changes and compactions. Under the RPC,
  `get_entries` returns `{head, entries}` (`all: true` includes abandoned
  branches for the tree view; entries carry `at` in milliseconds when
  known).
- `Compaction` — summarises older messages via the model and keeps a tail;
  manual (`/compact [instructions]`: the RPC's `instructions`, pi's
  `customInstructions`, are appended to the summariser's) or automatic.
- `Session_description` — after a turn, once the conversation has a second
  user message (or a long first one), asks the model for a one-line
  description and records it (`Agent ~auto_describe`, off under `-faux` and
  in tests); it runs after the turn has finished so it never delays the
  user, and shows up in the session list and in `State`.

### RPC

- `Rpc_json` — the wire encoding (plain tagged objects, not the derived
  `["Ctor", ...]` form) for messages, deltas, state, models, sessions,
  auth status and events. Messages carry `at`, milliseconds since the
  epoch, when known: in `get_messages` (the session entries' times, none for
  older entries or the compaction summary) and `get_subagent` (when the
  subagent's `message_end` arrived), and in `message_start`/`message_end`
  events (also nested in `subagent` ones), stamped by `Rpc_server` with the
  time it sends them: a `message_end` is sent as its message is appended,
  so it agrees with `get_messages` to within milliseconds. A streaming
  assistant message's `message_start` has its start, its `message_end` its
  end (the time frontends keep).
- `Rpc_server` — the connection and session manager: a table of live
  agents by session id, a table of clients, and per-connection
  `serve_lines` (`serve_connection` over newline-delimited flows, or one
  WebSocket text message per line: reader loop, one outbox fiber, and one fiber per
  request so a blocking method such as `shell` or a remote tool round trip
  never stalls the reader that must deliver the client's own
  `tool_exec_result`). Agent events are routed to the clients attached to
  that agent, except `tool_exec`/`tool_exec_cancel`, which go to the named
  host only. A login's URL, prompts and progress go only to the client that
  started it (another frontend, perhaps on another machine, must not open a
  browser or a dialog for it); its outcome (`done`, `failed`, `logged_out`)
  goes to everyone. `login {provider: "custom"}` starts `/login custom`
  (`failed` then names `custom` until the flow has a name); a custom
  provider's `logout` is a flow owned by its caller like a login;
  `list_models` rereads the registry; `auth_status` entries of custom
  providers carry `custom: {base_url, api, api_label}`. The session methods
  (`new_session`, `switch_session` by id or path, `fork`, `clone`, `import`)
  create or load an agent and move only the calling client; `list_sessions`
  marks live sessions with `live`, `running` and `clients`; `delete_session`
  refuses live ones. A session with running background subagents or jobs
  is not evicted (it delivers their reports into its own session even with
  no client attached); `shutdown` cancels them (killing the jobs).
  `kill_job {job_id}`, `job_output {job_id, lines}` (`{text}`: a header line
  and the last lines) and `list_jobs` (every job of the session: id,
  command, running, exit, delivered, elapsed, bytes, last_line) serve the
  `/jobs` picker, and `shell {command, background: true}` (`!&cmd`) returns
  `{job_id}`. `set_model` goes through `Model.resolve` (key, id,
  display name or unique case-insensitive prefix; otherwise "did you mean"
  by edit distance). Methods: `hello`, `ping`, `prompt`, `steer`,
  `follow_up`, `abort`, `dequeue`, `cancel_subagent`, `list_subagents`,
  `get_subagent`, `get_pending`, `kill_job`, `job_output`, `list_jobs`,
  `shell`, `get_state`, `get_messages`,
  `get_entries`, `set_model`, `set_thinking`, `list_models`, `compact`,
  `new_session`, `switch_session`, `list_sessions`, `set_session_name`,
  `delete_session`, `export`, `import`, `fork`, `clone`, `rewind`,
  `session_stats`, `set_cwd`, `list_paths`, `list_dirs`, `get_config`, `set_config`,
  `change_default`, `btw`, `btw_cancel`,
  `tool_confirm_respond`, `set_active_host`, `tool_exec_output`,
  `list_skills` (`{skills: [{name, description, path, model_invocable}]}`),
  `list_mcp {reconnect?}` and `mcp_approve {source, server}` (both
  `{servers: [{name, source, project, status: ready|failed|needs_approval,
  error?, tools: [{name, description}]}], problems}`, tools under their
  prigh names),
  `tool_exec_result`, `auth_status`, `login`, `auth_respond`, `auth_cancel`,
  `logout`. `State` carries `active_host` and `hosts` (the backend first).
  `btw {question, btw_id?}` (`/btw`) answers a side question with one
  tool-less model call (`Btw`, `Agent.btw`) over the session's recorded
  system prompt and a snapshot of its messages, sanitised because it may be
  taken mid-turn (a tool call without its result yet gets a "still running"
  placeholder), plus the question. It runs in the request's fiber, takes
  no run lock and never writes to the session; the answer streams to the
  calling client only as `btw_delta {btw_id, delta}` events and the
  response is `{btw_id, text, usage, cost_usd}`. `btw_cancel {btw_id}`
  (or the client disconnecting) cancels it. Its usage is added to the
  in-memory `State.usage`/`cost_usd` like subagents'.
- `Websocket` — a minimal RFC 6455 server side (handshake key, frame
  encode/decode with client masking, fragment reassembly, ping/pong and
  close) and `Web_server` — the `-web` listener: a connection whose first
  byte is `{` is a plain JSON-lines client (the TUI's `-connect`) and goes
  straight to `Rpc_server.serve_lines`, so one port serves terminals and
  browsers alike; otherwise one HTTP/1.1 request per connection, `GET /ws`
  upgraded and handed to `Rpc_server.serve_lines`, `GET /terminal` upgraded
  and handed to `Terminals` (below),
  anything else served from the web root (the built `tui/web-bin/site`,
  found via `-web-root`, `$PRIGH_WEB_ROOT` or next to the executable;
  no `..`, no dot files). The token check is the same `hello` check as
  for TCP; the static files are public.
- `Rpc_router` / `Namespace` — `-tokens name=token,...`: one `Rpc_server`
  per namespace, each with its own home (sessions, config, `auth.json`,
  global `AGENTS.md`) under `~/.prigh/namespaces/<name>` and no provider keys
  from the environment. Every listener goes through the router, which reads
  a connection's first line (a `hello` with a known token) and hands the
  connection to that namespace's server; `/terminal` and pi-web look the
  server up by the query `token` (and `user`). Without `-tokens` it wraps
  one server. Logins are user name + password: `hello`'s optional `user`
  must then be the token's namespace name (`Rpc_server.credentials_ok`,
  shared by the router and `hello`; without it the token alone still
  selects the namespace), every failure is `unauthorised: bad user name or
  password`, and the `hello` result carries `namespace` (null outside this
  mode). Single-token servers ignore `user`.
- `User_access` — who may act as which namespace: login tokens,
  `-host-tokens` (sign in as their namespace, never as a superuser) and
  `-superusers`. The router authenticates the first `hello`, including its
  optional `as_user` (superusers only), and connects the client to that
  namespace's server pre-authenticated (`Rpc_server.connect ~signed_in`),
  so its `hello` result also carries the signed-in `user` and `superuser`.
  `list_users` and `set_user` answer from the client's credentials; an
  allowed `set_user` ends `Rpc_server.serve_lines` and the router serves
  the rest of the connection from the new namespace's server, answering
  `set_user` with a `hello` of the first one's client details. pi-web
  instead reconnects with `as_user` (Pi_rpc's `/setusr` sends
  `prigh_set_user`), as do `/terminal` URLs. Without the backend host,
  `Rpc_server.session_file` keeps session paths inside the sessions
  directory and `Agent.export` writes through the tool host.
- `Terminal_channel` / `Terminal_relay` — terminals run on the session's
  active host (`Rpc_server.terminal_target`). `Terminals.serve` speaks to an
  abstract frame channel (a WebSocket, or frames fed by a relay). For a
  client host the backend relays the browser socket as `terminal_open` /
  `terminal_frame` / `terminal_close` events to that host and its
  `terminal_frame` / `terminal_closed` requests back; the host runs the same
  `Terminals` code (`Tool_host`, also behind the TUI's stdio worker, which
  forwards the same messages as lines).
- `Terminal` / `Terminals` / `Tmux_control` — the browser's shell panel.
  A `Terminal` is a tmux session on the server `-L prigh` (session names
  carry the backend's pid; `$PRIGH_TMUX` picks the binary) driven by one
  control-mode client (`tmux -C`) over pipes, so no pty is needed:
  `Tmux_control` parses its output into pane bytes (`%output`, unescaped),
  replies to our commands (`%begin`…`%end`/`%error`, matched in order) and
  `%exit`. Input is `send-keys -H`, resizing `refresh-client -C`. A new
  viewer's replay (`display-message` for the cursor and modes, then
  `capture-pane` of the screen and scrollback, and of the normal screen when
  an application has the alternate one) goes through the same client, so it
  lines up exactly with the live output after it. `Terminals` keys
  terminals by session id (cwd: that session's directory on the backend)
  and serves the `/terminal` WebSocket: binary frames carry bytes both
  ways, text frames carry `resize`, `ping`/`pong`, `exit` and `error`.
  Leaks are prevented at three levels: a socket silent for 30 s is closed
  (the page pings every 10 s), a terminal with no sockets for 10 minutes is
  killed, and every session has `destroy-unattached` with our control
  client as its only client, so when the backend dies (even by SIGKILL) the
  client sees EOF, exits, and tmux destroys the shell.

- `Pi_protocol` / `Pi_rpc` — pi's RPC protocol (what pi's web UI in
  `pi-web/` speaks) on top of `Rpc_server`: one WebSocket connection is one
  prigh client (`connect`/`handle`/`disconnect`); pi commands become prigh
  requests (`prompt` with `streamingBehavior` → `steer`/`follow_up`, its
  `images` passed on, which come back as pi `image` blocks in user and tool
  result content,
  `set_model {provider, modelId}` → `provider/id`, pi's seven thinking
  levels ↔ prigh's five, `fork {entryId}` → fork at the entry's parent,
  `bash` → `shell`) and prigh events become pi's (`message_update` carrying
  the accumulated message with synthetic index timestamps, which pi keys
  messages by; `tool_execution_*` with accumulated output; `tool_confirm`
  and login prompts as `extension_ui_request` dialogs (text prompts as
  `input` with the default as `prefill`) answered through
  `tool_confirm_respond`/`auth_respond`; notices as toasts; subagents as the
  agents-rail widget snapshot, re-read from `list_subagents` on every
  subagent lifecycle event; `session_reloaded`/`session_info_changed`/
  `thinking_level_changed`/`agent_settled` derived by diffing `state`
  events, suppressed while running a command the frontend re-syncs after).
  The prigh-only slash commands (`/login`, `/logout`, `/auth`, `/sessions`,
  `/switch`, `/host`, `/change_default`, `/help`) arrive as prompts and run in the adapter;
  `list_sessions`/`switch_session` are pi-protocol additions for the
  sidebar. `get_state` also re-sends the session's non-empty live state
  (status entries, queue, pending confirmations, agents rail) since the
  frontend clears it when it re-syncs after connecting or switching; a
  session change cancels the old session's confirmation dialogs.
  `watch_subagent {agentId | toolCallId}` answers with a subagent's
  transcript (`get_subagent`) and forwards its own message and tool events
  as `prigh_subagent_event`s until unwatched or the session changes (the
  pi-web subagent view). `serve -pi-web HOST:PORT` is a second `Web_server` listener whose
  `/ws?token=&user=&session=&name=` goes to `Pi_rpc.serve_websocket` (the query
  string becomes the `hello`; a refused hello is reported as
  `prigh_hello_failed` and the socket closed) and whose `/terminal` is the
  same `Terminals` as the `-web` listener's. `serve -prigh-web HOST:PORT`
  is a third, serving prigh-web's site with the same `/ws` (prigh's RPC) and
  `/terminal` as `-web`.

### CLI (`backend/bin/main.ml`)

`serve` (RPC on stdio; `-listen HOST:PORT` accepts TCP clients instead,
`-web HOST:PORT` serves the browser frontend, WebSocket clients and TCP
`-connect` clients on one port (`-open` launches a browser, `-web-root DIR`
overrides the assets), `-prigh-web HOST:PORT` and `-pi-web HOST:PORT` the
other two web frontends (`-prigh-web-root`, `-pi-web-root`), `-stdio` as
well,
`-token SECRET`/`$PRIGH_TOKEN` gates them; with stdio the backend exits when
the spawning frontend closes it, with `-listen`/`-web` only it runs until
killed), `tool-host` (the local tool worker), `run <prompt>`
(headless, streams to stdout), `sessions`, `login <provider> [-method]`,
`logout <provider>`, `auth`. All commands share `-auth-file`; `run`/`serve`
share `-model`, `-thinking`, `-session`, `-cwd`, `-no-tools`, `-faux` (and
`-faux-script FILE`, a JSON array of scripted replies that implies `-faux`);
for `serve`, `-session` is the default session, the one a client lands on
when its `hello` names none; without it every new client starts in a fresh
session of its own (sharing one is explicit: `hello` with the session id or
path, which is also how a frontend reattaches after a reconnect). With no explicit model, a new
session uses the config's `default_model`, else the first logged-in
provider's in the order anthropic, openai-codex, openai, deepseek.

## Frontend (`tui/`)

Built with OxCaml 5.2 and the Jane Street `v0.18~preview` packages
(`nix develop`); the backend stays on vanilla OCaml 5.3 (`nix develop
.#backend`) because some of its dependencies do not compile with OxCaml
modes. The two only meet over the wire, so the frontend owns its own
copy of the protocol types and the e2e test guards the contract.

- `protocol/` (`prigh_protocol`) — `Jsonaf` decoders for everything
  `Rpc_json` emits (`Message`, `Delta`, `State`, `Model`, `Session_summary`,
  `Auth_status`, `Auth_event`, `Event`, `Server_message`, `Skill`,
  `Mcp_server`, `Mcp_list`) and the `Request` encoder (`Request.Method` has
  typed constructors for `list_skills`, `list_mcp` and `mcp_approve`).
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
  lines back as `tool_exec_*` requests, a result's `images` verbatim).
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
    transcript and status; the app keeps an agent while it runs or while
    `State.subagents` lists it as undelivered, then until the next prompt;
    a delivered report, a subagent's or a job's, renders as a compact
    `Transcript` `Delivery` item; `/jobs` lists `list_jobs` in a picker,
    Enter adds `job_output` to the transcript as a block, Ctrl+D kills),
    `Commands` (slash table, parse, complete,
    closest), `Model_match` (display-name/prefix/did-you-mean), `Markdown`,
    `Btw_box` (the `/btw` panel above the editor: a newer question cancels
    and replaces it; Esc dismisses it before Esc's other meanings, so it
    never aborts the run), `Boxed` (the framed dialogs).
  - Skills and MCP: `/skills` and `/mcp` are pickers over `list_skills` and
    `list_mcp` (Enter on a server needing approval sends `mcp_approve`,
    whose reply is the new list). `/skill:NAME ARGS` is sent as a prompt
    tagged `Skill_prompt`, so a failure (an unknown name) puts the text back
    in the editor and takes it off the queue. `Autocomplete`'s `Skill`
    source completes names from `Skill_cache`, which is keyed by session,
    tool host and directory (a stale key or reply fetches again or is
    dropped) and emptied on reloads, `/setusr` and reconnects.
    `Skill_message` parses the backend's expansion (`<skill name=…
    location=…>`) back into name, file, body and arguments: `Transcript`
    renders it as a `Skill` item (`skill NAME` and the arguments; the body
    only in verbose), and `/fork` lists and restores it as
    `/skill:NAME ARGS`.
  - `Key.t` → `Intent.t` through `Keymap` (the one binding table; `/help`
    prints it). `Mode.t` (`Editing | Picker | Login_prompt | Text_prompt |
    Confirm | Search`) says who owns the keyboard; dialogs never stack, Esc
    always closes.
  - `Render.screen : Model.t -> Screen.t` lays out a frame as `Content.t`
    (styled spans with `Text_width`-aware wrapping) plus the cursor cell. The
    status line keeps the cwd and model, then fills remaining width by
    priority (context, cost/queued/agents/jobs, thinking, verbosity, new-line
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
  `?backend=`/`?session=`/`?name=` plus the user name and password (the
  token) saved by the connect form (`Login`: `prigh.user`, `prigh.token`)
  (using the page's origin `/ws` when no backend is explicit), sends `hello` with
  `tools: false`, mounts the shared component with
  `Bonsai_web.Start.start_and_get_handle` (incoming actions through the
  handle), installs document-level `keydown`/`paste`/`wheel`/`resize`
  listeners, and implements the platform: history in `localStorage`,
  `navigator.clipboard`, `window.open`; suspend and the external editor
  report themselves unavailable. A failed first `hello` shows a connect
  form instead (user name, password with a Caps Lock warning; both are
  saved and the selected backend is put in the reloaded page's query
  string). `/signout` (the `Sign_out` command; the terminal's platform
  answers that it is browser-only) and the `sign out` button next to `>_`
  forget the login, note it, and reload without `?session=`, so every socket
  is dropped and the next load shows the form instead of connecting. The page's `?session=` follows the current
  session (`history.replaceState`), so a reload rejoins it. The app sits in
  `#screen-area` (whose height `Browser.grid_size` measures) next to an
  optional `Terminal_panel`: a `>_` button opens it, and it is a
  `Vdom.Node.widget` around `web-bin/terminal.js` (xterm.js and its fit
  addon, vendored in `web-bin/vendor/`): connect with the size, reset on
  open (the first message is the replay), reconnect with backoff, ping, and
  a key after the shell exits starts a new one. `mount`'s options let a
  page show the connection's state itself (`onStatus`, as prigh-web
  does), not focus at once, and set the theme. Keys, pastes, wheel and
  touches inside the panel are left to xterm.js. `web-bin/` is
  the js_of_ocaml executable plus `index.html`/`style.css`/`terminal.js`
  and the vendored xterm.js, assembled under `web-bin/site/` and installed
  to `share/prigh_tui/web`.
- `prigh-web/` — the DOM-native browser frontend. Unlike `web-app/` it does
  not mount `ui/`'s cell-grid `App`: it has its own Elm-style state
  machine over the same `prigh_protocol` types and `prigh_client`, and
  renders HTML.
  - `lib/` (`prigh_web`, pure: `core` + `virtual_dom`):
    - `App` — `update : Model.t -> Action.t -> Model.t * Command.t list`;
      commands are `Rpc` tagged with a `Reply_tag.t`, `Reconnect`,
      `Set_url_session`, history saves, focus and toast expiry. Startup asks
      for `get_state` and `list_models`; a state with a new session id
      resets everything that belongs to the session and fetches its
      messages and the session list, and `new_session`/`switch_session`/
      `clone` are answered with `Reload_state`, so every way of changing
      session goes through that one path. Reconnection mirrors `ui/`
      (`Backend_closed` → `Reconnect` with backoff behind a banner; success
      starts over). `!cmd`/`!!cmd`/`!&cmd` go to `shell` like the TUI's.
      `run_command` has every command of `ui/`'s `Commands` (plus `/copy`;
      `/agents` and `/jobs` open, select and stop items in the agents
      panel); `Model` also holds the transcript `verbosity` (a class on
      `#chat`: the CSS hides tool output in quiet and opens every
      `<details>` in verbose), the `Config` (`get_config` at startup: the scope Ctrl+P
      cycles, `/confirm`), the side question (`Btw`, streamed by
      `btw_delta`), the saved accounts and whether we may act as others
      (`list_users` at startup succeeds only for superusers). `set_user`'s
      reply resets everything that belonged to the old user like a new
      session does, then starts over.
    - The transcript: `Chat` (one agent: `get_messages` then events; each
      tool call's streamed output and result; subagents' nested chats,
      fetched with `get_subagent` after a reload since `get_messages` only
      has their reports; `!cmd` runs as `Shell` entries), `Chat_view` (each
      entry, and the whole transcript, a `Node.lazy_` cached by `View_cache`
      on the value: virtual_dom skips what did not change, so a streamed
      delta or a key press costs the same in a long session as in a short
      one),
      `Tool_view` (a card per tool: bash, read with images, write, edit
      with `Line_diff`/`Diff_view`, ls/grep/find, subagents (marked with
      `data-call` and `data-agent` for the agents panel), jobs),
      `Delivery_view` (reports of finished background work, parsed by
      `ui/`'s `Delivery`), `Markdown` (a total parser that also renders
      streaming prefixes) and `Markdown_view`, `Image_view`,
      `Output_view`. Messages' times (`at`; the page stamps live events
      without one, from an older backend, with when it received them,
      `Chat.received`) show under user messages and in a reply's footer, as
      `Message_time` formats them: in the browser's time zone
      (`Model.utc_offset`, which the page sends at startup and when it
      changes) relative to today (`Model.now`): `14:32`, `Yesterday
      14:32`, `3 Oct 14:32`, `3 Oct 2025 14:32`, the full date in the
      `title`; `Chat_view` puts a day separator where consecutive messages'
      days differ, and nested and agents-panel transcripts get the same.
    - The agents panel: `Agents` (per session: `list_subagents` and
      `list_jobs`, kept live by `subagent_start`/`subagent`/`subagent_end`
      at any depth and by the state's `subagents`/`jobs` changing; nested
      ids `<parent>/<call>` make a tree; finished items fold under
      "earlier" at the next prompt; the selection, cancels and kills in
      flight, a job's polled `job_output`) and `Agents_view` (the list, a
      subagent in full — its transcript is the one in `Chat`, found by
      agent id at any depth, fetched with `get_subagent` when nested ones
      are missing after a reload — or a job's output; the status line's
      summary and the top bar's toggle). Elapsed times tick with `Clock`,
      which the page sends every second only while the panel is open with
      something running (`Model.ticking`); running jobs are then polled
      every 2s. `Reveal` scrolls the chat to a subagent's card through the
      `subagent` call ids from the top-level one down.
    - The terminal panel: `Terminal` (open, the dragged height, a
      generation that "New shell"/"Retry" bump, and the widget's last
      `Status` with the key of the target it is about) and
      `Terminal_view` (a header with the active host's name and directory
      and the connection's state, a notice when the shell exited or could
      not start — `Terminal.advice` says what to do — and the top bar's
      toggle). The view does not know xterm.js: `View.view` takes the
      widget as a function of a `Terminal.Target.t` (session, active host
      and whether it is connected, the user acted as, generation), so
      switching session, host or user, the host coming back or a new shell
      is a new target and a new connection, and tests print the target.
      Keys inside the panel (`Keys.Target.Terminal`) are the shell's, Esc
      and Enter included, even with a dialog or a tool confirmation
      showing; only Ctrl+` (by `code`) is ours, toggling the panel.
    - Around it: `Sidebar_view`/`Session_list` (fuzzy search, ages from
      `Rel_time`), `Topbar_view`, `Composer_view` with `Completion`
      (slash commands from `Slash`, their arguments, `@` paths via
      `list_paths`) and `History`, `Dialog`/`Dialog_view`/`Modal`
      (pickers built on `Picker`, help, rename, delete, the login flow
      `Login_flow` — also a custom provider's logout question — tool
      confirmations), `Command_dialog_view` (`/hotkeys`, `/scoped-models`,
      `Prompt` — a path with the backend's completions whose failures stay
      in the dialog, for `/cd`, `/host`, `/export`, `/import` — `/rewind`'s
      confirmation, `/session`, text), `Session_tree` (`get_entries`
      as `/fork`/`/rewind`/`/tree` pickers), `Btw_view`, `Account_view` (the
      sidebar's account button and the account menu, a `Picker` of
      accounts and actions) over `Accounts` (the saved sign-ins in
      localStorage, behind a `Storage` record so that tests use a table),
      `Status_view`, and `Keys` (which of the dialog, the popup or the
      editor owns a key; `help`, and `browser`: the TUI's keys that the
      browser keeps).
    - Skills and MCP: `Skills` caches `list_skills` for `/skill:`'s
      completion, keyed by the session, its directory and tool host (a
      reply for another place is ignored; switching user empties it), and
      builds `/skills`' picker; `Completion` offers the names right after
      `/skill:`, and `send` sends `/skill:NAME args` as a prompt (or steer
      or follow-up) for the backend to expand. `Skill_message` parses the
      expanded user message (`<skill name location>…</skill>` then the
      arguments), which `Chat_view` renders as a folded card above the
      arguments and `Session_tree` labels as typed. `Mcp_servers` turns
      `list_mcp`/`mcp_approve` replies (`Mcp_list`) into `/mcp`'s picker
      (servers to act on first; the problems under it, in `Dialog_view`),
      `/mcp reconnect`'s report and what became of an approved or restarted
      server; a ready one's tools are the `Mcp_tools` dialog.
  - `app/` (`prigh_web_app`) — `Web_main.run`: connects a `Ws_transport`
    to `?backend=` or the page's `/ws`, sends `hello` with the active
    account (`prigh.user`/`prigh.token`, `web-app/`'s `Login`) and
    `?session=`; on failure it shows the sign-in form (with the saved
    accounts, one click each), otherwise it saves the login as an account
    (`Accounts.remember`) and runs `App.update` in a
    `Bonsai.state_machine`, executes the commands (RPCs through the
    `Client`, replies back as `Action.Reply`; `history.replaceState` for
    `?session=`, also kept as the account's last session unless acting as
    another user; the clipboard; switching accounts, which activates one
    and loads the page for its backend and session; adding one, which
    shows the sign-in form on the next load; scrolling), and installs
    document listeners: keys (through `Keys`),
    pasted and dropped image files become attachments (PNG, JPEG, GIF,
    WebP, sent with the prompt as base64; others get a toast), the narrow
    (phone) layout, a clock for ages and toasts, and the chat following new
    output unless scrolled up. `Chat_listeners` copies code blocks and
    closes an open image with Esc; `Agents_listeners` opens a subagent's
    card (`data-agent`) in the agents panel, resizes the panel from its
    left edge (`--agents-width`, remembered in local storage), keeps a
    shown transcript or job output at its end unless scrolled up, and
    performs `Reveal` (opening the folds the card is in).
    `Terminal_widget` is the terminal panel's xterm.js: a
    `Vdom.Node.widget_of_module` around `web-bin/terminal.js` (loaded with
    the vendored xterm.js, its fit addon and `xterm.css` the first time the
    panel opens), connected to `Prigh_ui_web_app.Terminal_panel.url` with
    the page's user and token and the target's `as_user`; a new target key
    disposes the old connection (the backend keeps the shell) and mounts a
    new one; `terminal.js` reports its state through `onStatus`, which
    becomes `Action.Terminal_status`. It also resizes the panel from its
    top edge (`--terminal-height`, remembered with whether the panel is
    open, so a reload reopens it), and focuses the shell for
    `Focus_terminal`. The document listeners leave keys (a capture
    listener takes only Ctrl+`, before xterm.js), pastes and drops inside
    the panel to it, and the chat's and agents panel's mutation observers
    ignore xterm.js redrawing.
  - `bin/` — `main.bc.js` plus `index.html`, `style.css`, `chat.css`,
    `agents.css` and `terminal.css`, and `web-bin`'s `terminal.js` and
    vendored xterm.js (copied by dune rules, not duplicated), assembled under
    `bin/site/` and installed to `share/prigh_tui/prigh-web` (the Nix
    wrapper exports it as `$PRIGH_PRIGH_WEB_ROOT`). The dev profile links
    separately compiled units with inline source maps (~49 MB, fast to
    relink); release builds (`dune build -p`, so Nix) are whole-program at
    `--opt 3`, ~1.4 MB (400 KB gzipped; `web-bin` is ~1.65 MB).
  - `test/` — inline expect tests under node: a `Harness` drives
    `App.update`, answers RPCs and prints the commands and the rendered
    page. `e2e/` — the Playwright e2e (see Testing).
- `bin/` — `prigh-tui` (`-faux`, `-session`, `-model`, `-cwd`, `-auth-file`,
  `-backend`; `PRIGH_BACKEND` overrides the backend path). `-connect
  HOST:PORT` (`$PRIGH_CONNECT`) joins a running backend instead of spawning
  one, with `-token` (`$PRIGH_TOKEN`), `-user` (`$PRIGH_USER`) and `-name`; `-tools local|remote`
  says where this session's tools run (local = this machine through
  `tool-host`, the default with `-connect`; remote = the backend, the default
  when spawning, where the two coincide).

## Data on disk

- `~/.config/prigh/auth.json` — credentials (pi-compatible).
- `~/.prigh/sessions/<stamp>_<id>.jsonl` — session logs.
- `~/.prigh/sessions/exports/` — default `/export` output.
- `~/.prigh/history` — prompt history (one JSON string per line, last 500).
- `~/.prigh/config.json` — `scoped_models`, `confirm_tools`, `providers`
  (custom endpoints; their keys are in `auth.json` under their names).
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

`backend/test/test_background_jobs.ml` covers background jobs: delivery
while idle and at a turn boundary (batched with a subagent's report), the
job tools, abort, kills that really end the process, id continuity across a
reload, and jobs on a real `prigh tool-host` connected over TCP (output
streamed into the job, `kill_job` cancelling the exec on the host, a dropped
connection failing the job).

`backend/test/test_pi_rpc.ml` drives `Pi_rpc` over in-memory lines
(pi commands in, pi events out) for prompts, steering, confirmations,
thinking levels, forks, sessions, login dialogs, subagents and compaction;
`pi-web/` has vitest unit tests for its pure modules and a Playwright e2e
(`pi-web/e2e`, the `e2e` check of `pi-web/flake.nix`) that drives Chromium
and Firefox through the connect form, a scripted run with tools and a
subagent, slash commands, the model picker, a reload, a session switch and
the terminal panel.

The web layer adds two: `backend/test/test_web.ml` (frames, fragmentation,
the RFC handshake vector, static serving and traversal, and a masked WebSocket
RPC conversation over a real loopback socket)
and `tui/test-web/` (connection selection, `Key_of_dom` and `Dom_of_screen`,
run under `node` with js_of_ocaml because `virtual_dom`'s initialisers need a
JavaScript runtime; skipped without `node`). On Linux, the flake's
`web-workflows` check starts the packaged wrapper and `serve -faux -web`, checks
that the opened URL does not carry the token and that the secret never reaches
the log, fetches the installed bundle, and then drives real browsers with
Playwright (Chromium and Firefox): it submits the connect form by typing the
token into the Password field, verifies the token is remembered and the page reloads to an
authenticated WebSocket session, then types into the hidden keyboard input,
edits with arrow/backspace, submits a prompt to the faux provider and reloads
to prove the session survives. Every step saves a normalised ASCII snapshot of
the screen and the expected output lives in `tui/e2e-web/web_workflows.expected`.
Additional interactions (`/help`, pickers, `@` completion, `!` shell, paste,
resize, `?backend=` to a second backend, quit and reconnect after a backend
restart) have been exercised manually.

prigh-web's e2e, `tui/prigh-web/e2e/prigh_web.sh` (the flake's Linux
`prigh-web-e2e` check, against the packaged backend and site), runs
`prigh_web.mjs` in Chromium and Firefox. The driver owns the backend
(`serve -prigh-web 127.0.0.1:0 -faux-script ...` with an isolated `HOME`
and cwd) so that it can restart it, and prints normalised text snapshots
(ids, cwd, cost, context and messages' times replaced) diffed against `prigh_web.expected`:
the sign-in form and signing in with the token, the top bar, a scripted
bash call and its output, the model `read`ing a real PNG (`png.mjs` writes
it; the tool result's `img` must be a loaded `data:image/png` of the right
natural size), an image pasted and one dropped into the composer (shown,
sent, cleared, rendered in the prompt, and present with the right sizes in
the backend's copy of the message via a second WebSocket's
`get_messages`), a reload rejoining the session from `?session=`, a new
session and switching back from the sidebar, the backend dying (the
banner) and restarting on the same port (the page reconnects to the same
session and runs another prompt), and the agents panel (a background
subagent running a synchronous one and a background job: the list, the
nested one in full via Alt+2 and its card revealed in the chat, the job's
output, a card in the chat opening its agent), and the terminal panel (a
real shell through tmux, on a private tmux server via `TMUX_TMPDIR` with
`SHELL=/bin/sh` and a fixed prompt: typed output, Esc reaching `cat -v`,
`stty size` matching xterm.js's rows before and after dragging the panel
taller, Ctrl+` closing it with the focus back in the editor, the replay
after reopening and after a reload, another session's shell and back, the
phone sheet), with no console or page errors. Then `accounts.mjs` runs the account switcher against `serve -tokens
alice=a,bob=b -superusers alice`: alice signs in, adds bob (each sees only
their own sessions), switches back in one click, acts as bob and comes
back, and signs bob out, leaving alice on the sign-in page.
`SHOTS=DIR` saves a screenshot per step, `UPDATE=1` re-records,
`ENGINES=chromium` runs one browser.
