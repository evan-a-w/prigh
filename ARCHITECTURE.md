# prigh architecture

prigh is an agentic coding harness split into an OCaml backend (`backend/`,
all the logic) and OCaml frontends (`frontend/`, Bonsai: rendering, input and
UI state only): the TUI in a terminal and prigh-web in a browser; all build
with OxCaml. There is no
plugin system: tools, subagents, providers and slash commands are compiled in;
what comes from outside is skills (instructions in `SKILL.md` files) and the
tools of MCP servers.

```
 terminal ── frontend (prigh-tui, Bonsai_term) ── JSON lines on stdio or TCP ── backend `prigh serve` (OCaml, Eio)
 browser ─── prigh-web (main.bc.js, Bonsai_web) ── JSON lines on a WebSocket ┘  (`-web`: Web_server serves the page and /ws)
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
a client sends `hello` first (name, cwd, whether it can run tools and its
host id, an optional session id or path, the `-token` if the backend requires one, and
with `-tokens` optionally the user, i.e. the namespace name).

### Tool hosts

Each session has an *active tool host*: where its `on_host` tools (bash,
read, write, edit, ls, grep, find) and `!cmd` shells run. It is either the
backend itself or a connected client that advertised `tools: true` in
`hello` (a TUI, or a standalone `prigh tool-host -connect`); the subagent
tool always runs in the backend but its tool calls follow the same active
host. A client host is known by its *host id*: the `host_id` it sends in
`hello`, or its client id without one, so a host that reconnects or
restarts is the same host. `prigh tool-host -connect` and the TUI send the
machine's: `Host_id` (`backend/lib`, and `client/unix/` in the TUI) reads
`~/.prigh/host-id` from the home the tools see (the TUI's, which its
`prigh tool-host` worker shares), creating it with a random `host-…` id the
first time (written to a temporary file and linked into place, so
processes starting together agree); `-host-id` overrides it (the Docker
image's per-user `container-<name>`), and when the file cannot be kept a
random per-process id is used, with a warning. A newer connection claiming
a held id takes it over: the older one is a connection the host gave up on
(the backend may notice a dropped connection late) or another frontend on
the same machine (two TUIs share the id), so it falls back to its client id
(and is told with a notice), and its in-flight calls and relayed terminals
fail. When the connection holding the id disconnects, the newest other
connection that asked for it takes it back (`hand_over_host_id`), so a
machine's sessions keep running while any of its frontends is open. The
TUI's `(here)` is by host id, i.e. this machine. `hello` answers with both
`client_id` and `host_id`. With `-no-backend-host` the
backend is not a host at all (not listed, no in-process tools,
instructions, path listings or git branch), and a session that never had a
host adopts the first one that connects. Tools default to the frontend: a
tool-capable client takes over a session that runs on the backend by
default or has no host when it attaches, never one the user pinned with
`set_active_host` (`/host` in the TUI) or whose host is only disconnected.
The session cwd is a property of the host, so switching hosts switches the
cwd (and `/cd` validates the directory on the host: `set_cwd` and
`set_active_host` take `if_missing` (`fail`, the default; `ask`, answered
`{missing: path}` when the directory could be created; `create`, which
creates it with its parents through the host op `$make_dir`) and answer
`{cwd, created}`; `Dir_change` in the backend and in `frontend/protocol`, so
the frontends confirm before creating anything); a session remembers
its cwd on each host, also while the host is away. The session file records
where it runs: every change of cwd, host or pinning appends a `cwd` entry
carrying the host (id, name, whether pinned), so a session loaded again
(evicted from memory when it had no clients, or after a backend restart) is
on the host it had, in the cwd it had there, exactly like a live one: on it
if connected, otherwise waiting for it (never adopting another; first-time
adoption is only for sessions that never had a host, or whose recorded host
is the disabled backend). A single-agent `new_session` or `fork` carries on
on the current host; files without recorded hosts load as before.

A brand-new session made by or for a client (`new_session`, `fork`,
`clone`, `import`, the session a client starts in, and, without the backend
host, a saved session that never had a host when switched to) starts on a
host chosen from that client's context (`Rpc_server.start_location`), never
on whichever host connected first: on the client itself when it is a tool
host, in the cwd the client's current session had there (`new_session`,
`fork`, `clone`) or the client's own cwd (or the cwd an imported or loaded
session recorded there); otherwise where the client's current session is,
in its cwd, also while that host is away (the new session waits for it like
the one it came from); only when that session has no host either, where
any new session would be (the backend, or first-time adoption). The choice
is recorded in the session before its agent exists, so first-time adoption
cannot win a race with it; the session a tool host is given on connecting,
before `hello` says it is one, is replaced by one on it (with the backend
host it is taken over instead). Such a session is unpinned (its host was a
default), except that a fork or clone of a session pinned with `/host`
stays on that host, pinned: it is the same work. A non-tool client's
`import` follows the rules for loaded sessions (a recorded host is kept).

When the active host is a client, `Agent.host_exec` emits a
`tool_exec` event to that client only and waits on a promise; the client
answers with `tool_exec_output` (streamed chunks, fanned out to everyone as
`tool_output`) and `tool_exec_result`; an abort sends `tool_exec_cancel`.
A background job's exec carries `stdin: true` (the host keeps the process's
stdin open) and `job_input` reaches it as `tool_exec_input {exec_id, data,
close}` events, to the host only. Hosts announce what they support beyond
that in `hello`'s `features` (`exec_input`, sent by `prigh tool-host` and
the TUI with local tools; `Agent.Host.exec_input`): a job on a host without
it gets a `Stdin_feed` that refuses writes with "runs an older prigh ...
update it". A tool an older host does not know (`unknown host tool`) gets
the same advice appended.
A host whose connection drops keeps its in-flight calls for `host_grace`
(60s, `Rpc_server.create`): its next `hello` lists the executions it still
runs (`running_execs`), which carry on over the new connection; the others
on that host id fail (`[tool host reconnected]`), ids the backend no longer
waits for get a `tool_exec_cancel`, and when no hello comes in time they
fail with `[tool host disconnected]` (`prigh tool-host -connect` and the
TUI's `Tool_host` keep their worker across connections and hold results
and up to 1 MiB of output while offline). The session stays on that host: later calls fail with
"waiting for tool host NAME to reconnect; /host picks another" (the run
continues and the model sees the error) and the TUI shows `tools:offline`.
When the same host id connects again the session resumes on it in the cwd
it had there (a `Notice` "tools run on NAME again in CWD", a state change,
and no environment note for the model). Only `set_active_host` moves a
session off a disconnected host. The frontend does not implement any tools: it proxies
`tool_exec` to a local `prigh tool-host` process (`Tool_host` in the backend
runs `Host_ops.execute`, the same code path the backend uses for itself,
plus six pseudo-tools: `$resolve_dir` for `/cd` and `/host`, `$make_dir`
to create a missing one once confirmed (or `/mkdir`), `$read_file`
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

`backend/lib` is one library, `prigh`, with its modules grouped in
subdirectories (`(include_subdirs unqualified)`, so module names do not
carry the directory): `agent/` (the agent, its loop and what runs inside
it: compaction, approvals, plans, goals, reminders, the system prompt,
skills), `session/` (the session log and the message types it records),
`providers/` (the model providers and their routing, models, SSE, HTTP),
`auth/` (credentials, OAuth flows, logins), `tools/` (the built-in tools
and their helpers), `host/` (tool hosts and their operations), `mcp/`,
`rpc/` (the RPC server and router, namespaces, the web server and
WebSocket, file transfers), `terminal/` (the browser's terminals) and
`util/` (`Import`, `Config`, cancellation and other small shared pieces).
The layers below cut across those groups.

### Foundations

- `Import` — `Json = Jsonaf`, Eio aliases, `Env.t`.
- `Cancellation`, `Process` (streamed subprocess with timeout/kill; the
  child leads its own process group and the whole group is killed, so a
  cancelled `bash` cannot leave grandchildren holding the output pipes),
  `Http_client` (cohttp-eio + ocaml-tls; `post` and streaming
  `post_stream`, whose `idle_timeout` gives up a response or body that
  sends nothing for that long: `Stalled`; a connection broken mid-body is
  `Connection_lost`), `Sse` (incremental `text/event-stream` parser),
  `Sse_request` (streaming POST whose non-2xx bodies become
  `"HTTP <status>: <message>"`, the shape `Agent_loop` retries on; a model's
  stream that is silent for 5 minutes, not even a ping, is given up as
  stalled and retried).

### Model layer

- `Content` — assistant content blocks: `Text`, `Thinking {text;
  signature}`, `Tool_call {id; name; arguments}`, `Server_tool {id; name;
  input; output; is_error; raw}` (a tool the provider ran itself: web
  search; `output` is a readable summary, `raw` the provider's wire data).
  The thinking signature and a server tool's `raw` are provider-opaque
  (Anthropic block signatures, OpenAI encrypted reasoning items, search
  results with encrypted content) and are echoed back only to the provider
  that produced them; other providers get a server tool as text
  (`Server_tool.as_text`).
- `Message` — `User | Assistant | Tool_result`; assistant messages carry
  usage, stop reason and the `provider/id` key of the model that wrote them.
  User messages and tool results carry `images` (`Image.t`: MIME type and
  base64), as do `Tool_result.t`s; in sessions and on the wire the field is
  omitted when empty. User messages may carry an `origin`
  (`Injected.t`): what the harness put into the text — environment notes
  in front (`Note`: host, cwd, tools added/removed by a model change
  (`Tools`; the older editor note is recognised as one too), plan mode; a
  plan mode note
  sent between turns is a message of notes only, with an empty body), `@`
  files behind
  (`Attachment`: path and length), or a whole message written in the
  user's place (`Source`: delivery, hand-over, goal start/continuation,
  reminder, resume, `!cmd` output, compaction summary, `/review`, `/init`; a goal's
  or a reminder's message may carry a plan mode note too). The
  `text` stays exactly what the model sees, so providers and prompt caches
  are unaffected (the notes are derived by `Told_environment`).
  `Injected.infer` recognises the same things by their wording in sessions
  written before `origin` existed, and `Rpc_json`
  sends `origin` (recorded, else inferred) as `{body, notes: [{kind,
  title, text}], attachments: [{path}]}` or `{source: {kind, title}}`.
- `Image` — what models accept: PNG/JPEG/GIF/WebP sniffed from the bytes,
  dimensions from the header, and `load`, which downscales anything over
  2000 px or 4.5 MB of base64 by running ImageMagick or `sips` (PNG, then
  JPEG at falling quality and size) and adds a note with the scale factor
  for the model; without one, images within the hard limits go as they are.
- `Assistant_event` / `Assistant_builder` — the streaming delta vocabulary
  (`Text_delta`, `Thinking_delta`, `Thinking_signature`, `Tool_call_start`,
  `Tool_call_delta`, `Server_tool_use {id; name; input}` (adds the block or
  updates its input), `Server_tool_result {id; output; is_error; raw}`) and
  the accumulator that turns deltas into a message.
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
  `Request.prepare` (every provider's first step) applies
  `Tool_pairing.repair`, so every tool call is answered right after its
  message (a call left without a result, e.g. by a backend that stopped
  while it ran, gets an `[interrupted: ...]` error result; stray results
  are dropped), then omits images the model cannot see. `Agent.start_run`
  also records those interrupted results in the session
  (`Tool_pairing.unanswered`) before anything else is appended, so the
  transcript shows them.

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
- `Anthropic` — Messages API. The `web_search` tool is sent as the
  server tool `web_search_20250305` (`max_uses` 5); `server_tool_use` blocks
  (input from `input_json_delta`s, emitted at `content_block_stop`) and
  `web_search_tool_result` blocks (results or `error_code`, kept as `raw`)
  become `Server_tool_*` events. A `pause_turn` (a long server-side search)
  is continued (up to 10 times): what the turn has so far is sent back as
  the last, assistant message, its unresolved `server_tool_use` included,
  and the reply goes on in the same message; elsewhere an unresolved
  `server_tool_use` is dropped. Consecutive same-role turns are merged.
  Cache breakpoints (three of the four allowed): the end of the system
  prompt (tools come before it), the last cacheable block (not thinking or
  `server_tool_use`), and the last one before the newest assistant reply,
  where the request before ended: the provider only looks about 20 blocks
  back from a breakpoint for a cached prefix, so a turn of many parallel
  calls would otherwise read the whole context again
  (`test_agent.ml` checks that each request extends the one before).
  Adaptive thinking asks for `display: "summarized"`: newer models omit
  the text by default, leaving minutes of thought with nothing to show. A
  thinking block's start is passed on at once (an empty `Thinking_delta`),
  and a mid-stream `error` event keeps its type (`overloaded_error: ...`)
  so a transient one is retried. Thinking blocks
  are replayed only with a signature. With an OAuth token (Claude Pro/Max) the
  request is shaped like Claude Code's: Bearer auth, `claude-cli` user agent,
  `claude-code-20250219`/`oauth-2025-04-20` betas, the Claude Code identity
  as the first system block, and tool names in Claude Code's casing (mapped
  back to ours on `tool_use`).
- `Openai_responses` — Responses API for both `api.openai.com` (API key) and
  the ChatGPT Codex backend (`chatgpt.com/backend-api/codex/responses`, OAuth
  access token plus `chatgpt-account-id`). Requests use `store: false` and
  replay reasoning items through `encrypted_content`, stored verbatim as the
  thinking signature. `web_search` is sent as `{"type":"web_search"}` with
  `web_search_call.action.sources` included; `web_search_call` items become
  `Server_tool_*` events (the action without its sources as input, the
  sources or what was opened as output, the item as `raw`, replayed as an
  input item). Both get `prompt_cache_key` from
  `Request.cache_key` (the session id; `<session>/<agent id>` for a
  subagent; `btw` reuses the session's); custom servers are not sent it.
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
- `Faux_provider` — a scripted provider for tests and `--faux` runs
  (`Reply.context_overflow`, `Reply.usage_limit ?resets_at` and their
  script forms play those errors).

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
  The context also carries `approve` (checks a model's call before it
  runs: `Some result` refuses it with that result) and `ask_user` (how the
  `ask_user` tool reaches the user); `Agent_loop` puts the ones it is given
  into every call's context and `Tool_subagent` hands `approve` down to its
  nested loop, so subagents are checked too.
  `Tool_args` gives typed accessors and builds the JSON schema; `Truncate`
  bounds output by lines and bytes. `Tool_spec` carries the flags the
  harness reasons about: `parallel_safe` (safe to run concurrently with
  other tools), `destructive` (what the approval modes hold for
  confirmation, see `Approval`), `on_host` (runs on the active tool host rather than always in
  the backend) and `freeform` (a Lark grammar: the Responses API sends the
  tool as an OpenAI custom tool, whose free-text input streams and is
  replayed as the `input` string argument; other providers send the JSON
  schema).
- `Host_ops` — what a tool host does for a session: the `on_host` tools by
  name plus `$resolve_dir`/`$read_file`/`$list_paths`/`$instructions` (whose
  reply also lists the host's skills), `$skill` (one skill's file, for
  `/skill:`) and `$mcp_servers`/`$mcp_call`/`$mcp_approve` (the host's
  `Mcp_hub`). Every `$` op honours a `home` argument only in the backend:
  `Tool_host`, the `prigh tool-host` worker loop around it (`exec`/`cancel`
  in, `output`/`result` out, one fiber per exec), strips it so a remote host
  uses its own home. A tool host predating an op answers "unknown host tool",
  which the backend treats as having nothing (no skills, no MCP servers).
  A binary file asked for with `$read_file` (an upload attached to a
  prompt) is named with its size instead of inlined.
- The file browser, in layers. `Host_files` is the host's side: the
  pseudo-tools `$fs_list` (one directory's entries with kind, size, mtime,
  a symlink's target kind; at most 5000, dot files on request; a file lists
  its directory with it selected), `$fs_stat`, `$fs_read` (a base64 slice,
  at most 1 MB), `$fs_mkdir` (optionally with parents and a `.gitignore` of
  `*`), `$fs_rename`, `$fs_delete` (recursive, never following symlinks;
  not `/`, the home or a directory holding it, however written),
  `$fs_download` and `$fs_upload`. Paths resolve against the exec's cwd
  (the session's), are normalised lexically (no `.`, `..`, `//` or
  trailing `/`, so `link/` is the link) and may be anywhere the host's
  user can reach, like `bash`; reads and downloads take only regular
  files (a FIFO would block, a device never end). The transfers stream through the exec's
  output and its `Stdin_feed` (so they reach a client host through
  `tool_exec_output`/`tool_exec_input`, which need the `exec_input`
  feature): a download sends base64 chunks of 192 KiB raw (a file, or `tar
  -czf - -C parent name` of a directory), at most `window` ahead of the
  credits the backend grants with one `+` per chunk it has passed on;
  an upload takes base64 chunks from stdin into a temporary file next to
  the target, acknowledges each with an output `+`, checks the announced
  size and renames it into place at the end of input (`conflict`: `fail`,
  `overwrite` or `rename` to a free `name-N.ext`); cancelled, the
  temporary file goes. `File_ops` drives them on a session's active host
  through `Agent.host_call`: `preview` (the first 256 KiB: text as valid
  UTF-8, an image by `Image.sniff` with a link, else binary), `download`
  (writes each chunk to the caller's sink before granting the next
  credit, so memory stays at a window's worth whatever the size) and
  `upload` (at most 8 chunks ahead of the host's acknowledgements; at most
  2 GiB), and turns "unknown host tool" into "update prigh on tool host
  NAME". `File_tickets` issues short-lived capabilities (128 random bits,
  5 minutes, single use except image previews) bound to the agent, i.e.
  the session and so its namespace and host, and `Files_rpc` is the RPC
  face: `fs_list {path?, hidden?}`, `fs_preview {path}`, `fs_mkdir`,
  `fs_rename {from, to}`, `fs_delete`, `fs_download {path, archive?}`
  (`{url, name}`) and `fs_upload {name, size, dir?, overwrite?}` (`{url,
  path}`; without `dir` into the session cwd's `.prigh/uploads`, renamed
  when the name is taken), all on the calling client's session. `File_http`
  serves the tickets on the web server: `GET /files/download?ticket=` (a
  file with its length, a directory as `NAME.tar.gz`, an image preview
  inline; `Content-Security-Policy: sandbox`, `nosniff`; archives and
  files of unknown size chunked, so that one cut short by an error lacks
  the last chunk) and `PUT
  /files/upload?ticket=` (a `Content-Length` body read in chunks; 409 when
  the file appeared meanwhile, 410 for a used or expired ticket), with CORS
  open since the ticket, not a cookie, is the authority. So a client never
  reaches files except through its own session's active host: in a
  multi-user server without the backend host (Docker) that is the user's
  own tool host, running as that user.
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
- `Seen_files` — the guard against clobbering: `write`, and
  `apply_patch`'s `*** Add File:`/`*** Move to:` over an existing file, are
  refused unless the file's current contents are what the agent last read
  or wrote (an MD5 per path; the error says to read it first, or that it
  changed N seconds ago and must be read again). `edit` and patch updates
  are not blocked but note a change. The record lives where the files are:
  a process-wide table on the tool host (the backend's for its own host),
  keyed by the session id that `Agent.executor` adds to `read`/`write`/
  `edit`/`apply_patch` calls (and `$read_file` attachments) as the hidden
  argument `$seen_scope`, so subagents share it; it is bounded (2000 files
  per scope, 200 scopes, least recently used dropped). A restarted host, a
  host switch or an evicted entry leave the file unseen: the agent is asked
  to read it again. Calls without a scope (older backends) are unchecked.
- `Tool_web_fetch` / `Html_text` — `web_fetch` runs on the active host
  (`on_host`, so `localhost` is the host's; a host predating it answers
  `unknown host tool`), through `Http_client.get_stream`: up to 10
  redirects, a 5 MB body limit, a 30 s default timeout (at most 120). HTML
  goes through `Html_text` (a hand-written tokenizer and renderer: headings,
  lists, links resolved against the final URL, `pre` fences, tables,
  quotes; scripts, styles, `nav` and form controls dropped; only `main`,
  else `article`, when present; entities decoded), text types pass through
  (Latin-1 converted), images are attached through `Image.load`, other types
  are refused with a `curl` hint. The result starts `URL (HTTP 200, type,
  size)` and a `Title:` line; long text is cut like `bash` output with the
  full text saved (`Tool_bash.save_output`). HTTP errors keep the start of
  the body. Not destructive.
- `Tool_web_search` — `web_search` is a provider-side tool: `Tools.for_model`
  keeps it only for Anthropic, OpenAI and OpenAI Codex models, whose
  providers send their native tool for it; its `run` only answers a model
  that calls it as a function anyway.
- `Tool_bash` (streamed output, timeout, cancellation; long output keeps
  its beginning and end (`Truncate.head_and_tail`) and the full output is
  saved under `$TMPDIR/prigh-output-<uid>/` (private to the user) on the host, kept for 7 days, its
  path in the result; with `background` at depth 0 it starts a job instead,
  see below), `Tool_read` (images
  through `Image.load`, as an image result),
  `Tool_write` (`wrote N lines`), `Tool_edit` (multi-edit, unique
  non-overlapping matches, atomic; returns a unified diff from `Udiff`),
  `Tool_apply_patch` (Codex's patch format, parsed and matched by
  `Apply_patch` with Codex's whitespace and punctuation leniency; adds,
  deletes, moves and updates several files, writing nothing unless all of
  it applies (a write that fails anyway is reported with what was and was
  not changed; `a.txt` and `./a.txt` are one file); returns Codex's `Success. Updated the following files:` list
  plus the updated files' diffs), `Tool_ls`, `Tool_grep`/`Tool_find` (via
  `rg`).
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
  `subagent_status`/`subagent_cancel` act on the jobs and are dropped below
  depth 0. Nothing blocks on background work: its reports wake the agent.
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
  message; `take_next` first blocks until a running task finishes) or by
  `cancel_and_wait` (`job_kill`/`subagent_cancel` results). Events and changes
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
  A foreground `bash` call at depth 0 also runs through the backend
  (`Tool_bash.runs_in_backend`) when `Background_tasks.auto_background`
  (the config's `auto_background_seconds`, read through the closure
  `Agent` passes to `connect`) is set: `Background_tasks.run_foreground`
  runs the same foreground call in a fiber of the agent's switch with its
  own cancellation (cancelled by the run's until then) and waits up to
  that long (from when the command starts, so after any approval); its
  output streams as the call's `Tool_output` and into an `Output_tail`.
  Still running then, it is moved without being restarted: a `Job` task is
  made around the running fiber (next `j<n>`, that output, that
  cancellation, a refusing `Stdin_feed`), the fiber's result becomes its
  outcome (`job_outcome`) and it is delivered like any job, while the call
  returns `still running after Ns, so it was moved to the background as
  job j<n> ...` with the last 10 lines (`Tool_bash.moved_job_id` reads the
  id back, for the session's job numbering; `Moved_job` in `frontend/protocol`
  for the frontends). On a client host the pending `Tool_exec` simply
  keeps going in that fiber. Without an explicit `timeout` (and a limit
  under the default 600 s) the call is sent with the jobs' "no timeout",
  since the limit always comes first. A call finishing in time leaves no
  task and uses no id.
  `Tool_jobs` has `job_status`, `job_output` (`lines`, `offset` from the
  end), `job_input` and `job_kill`. Below depth 0 the subagent controls are
  dropped and `bash` is `Tool_bash.subagent_tool` (whose `background`
  description says the subagent finishes only once its jobs have exited):
  `Tool_subagent.execute` gives each subagent run a `Background_tasks` of
  its own (`with_jobs`, no auto-background, so its foreground calls block
  as long as they take), delivers finished jobs' reports through the
  loop's `steer` between turns and its `wake` when the model ends a turn
  (`Background_tasks.take_next`: blocks until a job finishes, so the run
  goes on with the report and ends only at a turn ended with no job
  running), and when the run ends otherwise (cancelled, out of turns)
  kills (and discards) jobs still running, naming them in the report. Every job (the
  model's and `!&cmd`) has a `Stdin_feed` (`Background_tasks.Task.input`):
  `Tool_bash.start_job` puts it in the foreground call's context
  (`Tool.Context.stdin`), so locally `Process.run ~input` keeps the
  process's stdin open and copies the feed's items into it (a `Close` closes
  it), and on a client host `Agent` forwards them as `tool_exec_input`
  events, the host's worker feeding its own `Stdin_feed`. Unless the job
  was started with `stdin: true` (`?keep_stdin`), the feed is closed at
  once and refuses `job_input` with a message saying so (so a job that
  reads stdin sees end of input instead of waiting forever). Other
  processes still get an empty stdin. `job_input` writes (and optionally closes) and
  returns the output that follows (`Output_tail.since`): until it is quiet
  for 300 ms after something arrived, the job exits or `wait_ms` pass.
- Approvals — `Approval_mode` (`Ask | Auto_edit | Auto`), `Approval_rule`
  (Claude Code's `TOOL(PATTERN)` rules: parsing, `Call.create` (a call's
  command or the paths it touches, `..` resolved lexically), `covers` (all
  of a call's parts, a command's segments or paths, matched by some rule;
  never commands with substitutions or output redirections), `touches`
  (any part), `suggest` (what "don't ask again" adds: `bash(PREFIX:*)` per
  segment, `edit`, the tool's name) and the command splitter) and
  `Approval` (`Rules`, config's `permissions` with its problems, and
  `decide`: deny rule → plan mode refuses edits → ask rule (no
  suggestions) → allow rules, the config's and the session's → plan mode
  asks for other destructive tools → the mode).
- `Plan` — `update_plan` (Codex's): steps with statuses, validated (several
  may be in progress, for parallel work), and `latest`, the plan as of the last successful call
  in a list of messages, which is how `Agent` keeps it as session state
  (persisted for free, following rewinds and forks). `User_question`
  (`ask_user`'s questions, answers checked against them, the result text)
  and `Tool_ask_user`; `Tool_exit_plan_mode` (its call is handled entirely
  by the agent's approver). `Tools.interaction` holds the three; they are
  not tool-host tools, and `Tools.for_context` drops them for subagents,
  which cannot ask the user (nor `goal_complete`, `job_input` and the other
  background controls).
- `Tools.all` is the fixed built-in set; `Tools.builtin` adds the variants
  (`apply_patch`) that a tool host can also run. `Tools.for_model` gives a
  model the file editor it is trained on: `apply_patch` for OpenAI's GPT-5
  and later (Codex's tool, sent freeform), `edit` for the rest. `Agent`
  applies it to every run and to the system prompt; when a model change
  changes the tool list (another editor, or `web_search` that only some
  providers run), the next user message carries a note naming the tools
  now available and no longer available (the prompt stays fixed). Several
  switches before that message give one note with the net change, none if
  the tools end up as they were. `Tools.for_context` builds the per-agent tool list
  (`parent`, `depth`, optional `only`), so the subagent's tool set can be
  restricted and the `subagent` tool is dropped at depth 2; unknown names
  are an argument error, and either editor's name means the child model's
  editor (`Tool_subagent` applies `for_model` for the child's model).

### Harness

- `System_prompt` — built-in guidance plus environment facts plus
  `AGENTS.md`/`CLAUDE.md` files from `/` down to the cwd and
  `~/.prigh/AGENTS.md`; `read_instructions` scans the local filesystem and
  `build ?instructions ?nix ?skills ?auto_background ?role` accepts files
  fetched
  elsewhere (the tool host), whether that host has Nix, which adds how to
  get missing tools from nixpkgs, the host's skills, listed when the model
  has `read`, and the auto-background limit, which the guidance on
  background work (given to agents with `job_output`/`subagent_status`:
  keep working or end the turn, never sleep or poll, `job_output` to look
  without blocking) names. `role`
  (`Main` by default; `Tool_subagent` passes `Subagent`) picks whose prompt
  it is: the main agent's, given the `subagent` tool, tells the model it is
  the orchestrator, delegating substantial work to subagents and keeping
  its own context for planning, coordinating and reviewing; a subagent's
  (nested ones keep `subagent`) leaves that out and says instead to use
  foreground commands rather than sleep, that job reports wake it after its
  turn, and that it finishes only once its jobs have exited (so kill
  servers first).
  `Agent` builds it once, at the session's first run, and records it as a
  `System_prompt` session entry so every later request (and every reload)
  sends the same prefix, which is what provider prompt caches key on. When
  the cwd or the active host changes afterwards, `host_changed_note` /
  `cwd_changed_note` are prepended to the next user message: they say what
  changed and leave re-reading the instructions to the model's discretion,
  since they are often unchanged.
- `Told_environment` — what the model was last told about its environment
  (host id, cwd, the tool names `Tools.for_model` gives it, plan mode):
  set from the current environment when the system prompt is built (plan
  mode excepted: the prompt does not describe it) and whenever notes are
  delivered, and recorded as a `Told_environment` session entry on the
  active path. The environment notes (host, cwd, tools, plan mode) are not
  queued but derived when a user message is built, by `notes ~told now`, so
  a change made before the session is evicted, switched away from (in
  place, as the TUI does, or by another frontend), reloaded after a backend
  restart or reconnected to is still told, exactly once, and changes that
  cancel out tell nothing. Rewinding past the message that delivered a note
  tells it again on the new branch; a fork carries the pending changes.
  Sessions from before the entry start from their environment as loaded
  (what was pending then is lost) and record it at their first change.
- `Agent_loop` — the core loop. Per turn: build the request from the context,
  stream the assistant message (emitting `Message_start/update/end`), retry
  transient errors (HTTP 408/429/5xx/529, Anthropic's overloaded, API and
  rate limit errors, failed, lost or stalled connections) up to 5 times
  with backoff (1, 2, 4, 8, 16 s; cancellable), emitting `Retry {attempt;
  max_attempts; error; delay}` before each wait so the frontends say so,
  execute the tool calls
  (`Tool_start/output/end`), append results, then poll `steer` for user
  messages to inject before the next turn. A turn whose calls are all
  `parallel_safe` runs them through `Eio.Fiber.List.map`; a mixed turn stays
  sequential, and results are appended in call order regardless of completion
  order so the context stays deterministic. Every call goes through the
  context's `approve` first (the agent's: the approval mode, plan mode and
  rules; a call waiting for the user blocks on a `Tool_confirm` event); a
  refusal becomes the call's result. Stops on `End_turn` without tool calls
  (unless `wake`, which may block, returns messages to go on with: a
  subagent's job reports), `Length`, `Error`, `Aborted`, `max_turns`, cancellation or `halt` (polled
  before each turn, and handed to tools so subagents poll it too: the
  session budget). Every tool call
  always gets a result (a `[cancelled]` one if needed) so the context stays
  valid. Before each turn after the first, when the latest response's
  input and output tokens plus an estimate of what was appended since cross
  `Compaction.should_compact`, the context is compacted through the
  `compact` hook (default: `Compaction.compact_messages`, in memory; the
  agent's compacts its session) and the run goes on with the result; a
  failed compaction is not retried within the run. A request rejected as
  too long (`Context_overflow.matches`: pi's and opencode's wordings,
  excluding rate limits and usage limits) is compacted and sent again, the
  failed reply dropped, with less kept the second time; after two, or when
  compaction fails, the turn ends with the error plus what to do. Each
  compaction emits `Compacted {summary; usage; cost_usd}`. The request
  carries `Config.cache_key`.
- `Agent_event` — the loop's event vocabulary (`Agent_start/end`, `Turn_*`,
  `Message_*`, `Tool_start/output/end`, `Tool_confirm`, `Compacted`,
  `Tool_confirm_done`, `Ask_user`, `Ask_user_done`, and the recursive
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
  texts to restore, `dequeue` = pops the last queued message,
  `remove_queued` = takes one message out of a queue by index, if its text
  still matches (the frontend's view may be stale), `shell` = runs
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
  id). No subagent stays "running" once nothing runs it: `Tool_subagent`
  emits a `Subagent_end` even when its loop raises (`subagent failed: …`,
  or `[cancelled]`), and when the main run ends and whenever a background
  task finishes `Agent` sweeps the log (`Subagent_log.orphan_ends`): a
  subagent still running whose top-level ancestor is gone (a finished
  background task with its id, or the main run for its synchronous ones)
  gets a synthetic `Subagent_end` (`[interrupted] <why>`, zero usage),
  broadcast inside its ancestors' `Subagent` envelopes like a real one.
  The log follows the session (`Agent.create` and the in-place
  `new_session`/`switch_session`/`fork` `Subagent_log.load` it): it is
  appended to the session's `subagents.jsonl` (one JSON line per subagent
  start, `message_end` and end, with their times and turn/tool counts, and
  per top-level run once the file exists, for `stale`; nothing per
  delta, nothing before the session file exists) and replayed from it when
  the session is loaded again (after eviction, a restart, `-session`),
  skipping lines that do not parse. Memory stays the source of truth for
  reads. Subagents saved as running load as failed (`[interrupted] the
  backend stopped before it finished`, ended at their last saved
  activity), and the background task ids skip the reloaded ones
  (`Background_tasks.avoid_subagent_ids`), so a rewound conversation does
  not reuse `a<n>`.
  `queued_texts` and `pending_confirms` back `get_pending`; the in-place
  `new_session`/`switch_session` cancel them, drop their reports and
  forget the finished tasks (`list_jobs` starts empty).
  `prompt`/`steer`/`follow_up` accept optional `attachments`
  (paths whose contents are appended to the user message as `<file>` blocks;
  image files are read like `read` does and attached as images) and
  `images`.
  `State.usage`/`cost_usd` count every assistant message in the session file
  (all branches, also those compacted away), each at its own model's price,
  plus the `Spend` entries recorded as it happens for what the messages do
  not hold: subagents' responses (from their nested `Message_end`s, so a
  running subagent's spend shows), compactions and `btw` (never
  `context_tokens`); being in the file, they survive a restart. `State` also
  carries the session name, cwd and `git_branch`.
  Approvals: the agent's approver (`approve`, given to
  every run and through it to subagents) runs `Approval.decide` with the
  session's mode, plan mode, the config's `permissions` and the session's
  rules; a call that needs the user emits `Tool_confirm` (call id, agent id
  for a subagent's, summary, the rules "always" would add) and waits;
  `respond_confirm` answers it (`Allow`, `Always` (adds the rules, recorded
  in the session, and lets through any other pending call they now cover),
  `Deny reason`), a mode change lets through those it no longer stops, and
  `Tool_confirm_done` follows whatever ends it, so every frontend closes its
  dialog. `exit_plan_mode` is answered by the approver itself: a
  confirmation whose summary is the plan; approving leaves plan mode.
  `set_mode` switches the approval mode and/or plan mode (recorded in the
  session; a changed config `approval_mode` switches the session too). The
  model is told about plan mode by `System_prompt.plan_mode_note`, on the
  next user message (or goal message) or, while a loop runs, as a message
  of its own at the next turn boundary, recorded as an `Injected` note of
  kind `Plan_mode`. `ask_user` emits `Ask_user` and waits for
  `answer_question` (the first answer wins; abort withdraws it;
  `Ask_user_done` follows). Its timeout (`User_question.timeout`: the
  config's `ask_user_timeout_seconds`, or the call's `timeout_seconds`,
  clamped to 30-300, when shorter: the model can only shorten it) gives the question a
  `deadline` (in `Ask_user`, `Pending_question` and so `deadline_ms` over
  RPC); a fiber racing the answer sleeps until it through the agent's
  `sleep_until` (injectable, as for the automatic resume), then resolves the
  call with `Timed_out`, which the tool turns into a reply telling the model
  to carry on; `Ask_user_done` then says `timed_out` and a `Notice`
  follows. Subagents cannot ask, so only the main agent's questions time
  out. `pending_confirms`/`pending_questions` back
  `get_pending`, so a reconnecting frontend shows them again. `State`
  carries `approval_mode`, `plan_mode`, `plan` (`Plan.latest` of the active
  path, updated on each successful `update_plan`) and `session_rules`.
- Budget, goal and resume (`Agent`) — the budget is the session's last
  `Budget` entry (`set_budget`; `None` turns it off), else the agent's
  `max_budget_usd` (the CLI's `-max-budget-usd`), else the config's;
  `budget_exhausted` is the run's `halt`, refuses `prompt`/`steer`/
  `follow_up`/`set_goal`, and is announced in a `Notice` naming `/budget`.
  The goal (`Goal.t`: text, `Active | Done | Stopped`, continuations) is the
  session's last `Goal` entry; while it is active the main agent's tools
  include `Tool_goal` (`goal_complete`, dropped by `Tools.for_context` for
  subagents), and a run's tail, after hand-over, follow-ups and undelivered
  reports, starts a run with `Goal.continuation_message` when the run ended
  normally; an abort, an error, the budget or `goal_max_continuations` stop
  it (a hand-over or a scheduled resume keep it active). In plan mode no
  continuation starts (the goal stays active): `set_mode` leaving plan mode
  while idle starts it, and goal messages carry the plan mode note like
  user messages do. `set_goal` and
  `resume_goal` start a run with `Goal.start_message` when idle. When a run
  ends on an unavailable model with no fallback left, `Usage_limit.resets_at`
  reads the reset time `Sse_request` noted in the error
  (`Usage_limit.reset_time`: the body's `resets_at`/`resets_in_seconds`,
  `x-codex-*-reset-at`, `anthropic-ratelimit-unified-*reset`,
  `retry-after`; one already past backs off from a minute, doubling to an
  hour, `past_resets` counting them); with `resume_on_limit_reset`, a fiber
  sleeps until then
  (`?sleep_until`, injectable for tests) and starts a run with a message to
  carry on. `State.resume_at` shows it; `cancel_resume`, `resume_now`,
  `abort`, any new run and a session switch end it. It is recorded in the
  session (`Resume_at`; `None` when it runs or ends): an agent created for
  (or switched to) the session arms it again while its time is still to
  come, and drops it otherwise; `Rpc_server.restore_resumes` (called by
  `main` for each server at startup) loads every session whose recorded
  resume is to come. When the timer fires on a session whose file was
  deleted meanwhile (`Session.gone`), a notice says so and nothing runs.
  `Rpc_server` keeps the agent loaded while one is scheduled.
- Reminders (`Reminder`, `Agent`) — the session's (`/remind`), each named
  (`r1`, `r2`, ...: `Reminder.create` numbers after every name the file
  ever held, `Session.reminder_names`, so none is reused) with `On_stop
  {max_repeats}` or `Every_turns n`, for the main agent only. The whole list
  is a `Reminders` session entry (the last on the active path counts, like
  the goal; `fork` copies it, a new session has none), cached in the agent
  and changed by `add_reminder`, `remove_reminder` and `clear_reminders`
  (RPC methods of the same names), each followed by a state event. A
  `Reminder.Tracker` in the agent counts the main loop's `Turn_end`s
  (subagents' events never reach it), the turn each periodic reminder last
  fired (by name; one not seen yet counts from then) and the eligible stops
  since the user spoke (`user_message` clears it, `replace_session` resets
  everything). Due periodic reminders follow
  the queued messages in the loop's `steer` and the prompts of a run's
  start; on-stop ones are the last choice of a run's tail, after hand-over,
  follow-ups, undelivered reports and the goal's continuation, under the
  goal's conditions plus no running background task; `Tracker.stopped`
  numbers the repeats and reports, once, the ones that used theirs up (a
  `Notice`). Both are user messages with `Source.Reminder`, carrying the
  plan mode note like goal messages. The frontends parse `/remind` with
  the shared `Remind_command` (`frontend/ui`).
- `Config` — `~/.prigh/config.json` (`auto_background_seconds` (default
  30, 0 = off: see background jobs), `scoped_models : string list`,
  `approval_mode` (new sessions' mode; read from `confirm_tools` when
  absent, which saving replaces), `permissions` (allow/ask/deny rules;
  saving keeps the file's as written), `default_model`/`default_thinking`,
  `fallback_models` (model keys), `default_cwd`, `max_budget_usd`,
  `goal_max_continuations`, `resume_on_limit_reset`, `ask_user_timeout_seconds` (default
  300, 0 = never) and `snippets`, the frontends' `/snippet` texts,
  validated by `Snippet`), loaded at agent
  creation, read/written through `get_config`/`set_config`; unknown fields
  are ignored by `load` but reported by `problems` (with wrong-typed ones;
  a leftover `reminders` says reminders are per session now),
  which `Model_registry` adds to its problems along with an unknown
  `default_model`. Each namespace has its own file, so each has its own
  defaults; `archive_after_days` (default 7, 0 = never) is when an
  inactive session counts as archived (`Session.Summary.is_archived`).
  `set_config` merges the given fields over the current ones and
  resolves `fallback_models` like `set_model` (stored as keys). A new
  session starts in `default_cwd` (on the backend host) and on
  `default_model`, else the first of `fallback_models`.
- `User_prefs` — a user's preferences and data that frontends keep on the
  backend, `~/.prigh/prefs.json` under the user's home (per namespace with
  `-tokens`): a JSON object of keys the backend gives no meaning
  (prigh-web's `web.*`, the shared prompt `history`). `set` sets or removes
  keys and returns those that changed; `push` puts a value at the front of
  a list unless it is already first, keeping the newest `max` (and the
  newest within 256 KiB), so that frontends adding to the history at once
  lose nothing. Every change re-reads the file and replaces it (a
  temporary file renamed over it) under an `Eio.Mutex` and a `Lock_dir`,
  so backends sharing a home (a TUI's own and `prigh serve`) keep each
  other's changes. Keys are at most 200 bytes, values 256 KiB, the file
  4 MiB; a broken file is reported with what to do, never overwritten.
  `Rpc_server` serves it as `get_prefs {keys?}` (`{prefs}`), `set_prefs
  {prefs: {key: value | null}}` (`{prefs}`: what changed) and `push_pref
  {key, value, max?}` (`{value}`, `max` 500 by default, at most 1000), and
  sends what changed to the server's other clients (the user's other
  browsers and terminals) as `{"event": "prefs_changed", "prefs": {key:
  value | null}}`; a server without a home (tests) keeps none.
- Model hand-over — `Usage_limit.unavailable` recognises errors meaning a
  model cannot be used for now: an exhausted allowance or balance (`HTTP
  402`, quota codes such as `usage_limit_reached`/`insufficient_quota`,
  which `Sse_request` marks with `(usage limit reached)`, as it does an
  Anthropic subscription's `anthropic-ratelimit-unified-status: rejected`)
  or missing credentials. `Agent_loop` does not retry them. When a run ends
  on one, `Agent` switches the session to the next model in
  `fallback_models` after the current one (the first when the current one
  is not listed) that this hand-over sequence has not tried, says so in a
  `Notice`, and starts a new run whose first message tells the new model it
  is taking over; the queue and follow-ups wait behind it. A user's prompt
  starts a fresh sequence; an abort, a session switch or the end of the
  chain stops it. `Agent.save_as_default` (`change_default`,
  `/change_default`) rereads the file and records the agent's current model
  and thinking level there; agents created without an explicit `-model`/
  `-thinking` start from them (a loaded session's own settings still win).
- `Session` — an append-only JSONL log forming a tree: every entry has a
  `parent` and `at`, when it was appended (seconds; optional, so files
  written before it load, without times; a fork keeps the copied entries'
  times), and the active conversation is the path from the root to `head`.
  Rewinding moves `head`; forking copies the active path to a new file.
  Entries are messages, model/thinking changes, compaction summaries, names,
  descriptions, cwds with the tool host they are on (so a reload restores
  where the session runs), the system prompt, the goal, the reminders
  (`Reminders`, the whole list), the approval mode with plan
  mode (`approval`), rules the user allowed for the session (`allow_rule`),
  `Archived {archived}` (archived or unarchived by hand), `Spend {usage,
  cost_usd}` (what the messages do not record: subagents' responses,
  compactions, `btw`; `spend` totals them on all branches) and `Resume_at
  {at}` (a scheduled resume after a usage limit, `None` once it ran or was
  cancelled; `resume_at`, also in `Summary`) — these three are ignored by
  `messages` and not copied by `fork` — and `Told_environment` (what the
  model was last told about its environment, see the harness);
  `Session.messages` is the message list for the next request with the
  compaction summary replacing everything before `kept_from`, preceded by
  the compaction's `pinned` message (the user's latest, kept verbatim when
  the cut falls in the middle of the work on it; `context_ids` gives each
  message's entry id)
  (`timed_messages`: each with its entry's time, as a `Timed_message.t`;
  the summary has none). Nothing is
  written to disk until the first message (or name): an abandoned empty
  session leaves no file. After that, entries are only appended to the
  existing file (no `O_CREAT`), so a session deleted while something still
  holds it (its description being written) is not recreated. `list` returns name, description, cwd,
  timestamps, message count, first prompt, parent, the `Archived` value
  after the last message (if any), the last activity (the last message's
  time, else the mtime, or a later unarchiving) and `bytes` (disk usage of
  `files`: the JSONL and the session's directory `<stamp>_<id>/` next to
  it, `data_dir`, for per-session data; `delete_files` removes both),
  most recently modified first; it caches each file's summary by (path,
  size, mtime), so only changed files are parsed again (300 sessions of
  50 messages: `list_sessions` 170 ms → 3.5 ms). `list_with` (what
  `Rpc_server` uses) sums up the live sessions (whose files grow with every
  message) from memory instead of parsing them again (an 80 MB session
  being worked on: `list_sessions` 950 ms → 3 ms), and parses the others
  with the given `load`. `load ~yield` calls `yield` every 256 KB parsed
  (the server passes `Fiber.yield`). `Summary.is_archived`:
  never while a run or background work is going on, else archived by hand,
  else when the last activity is older than `archive_after_days`.
  `export` writes markdown or copies the JSONL,
  `import` copies a file in (the markdown has each message's time, in UTC,
  in its heading), and `session_stats` counts turns, tool calls by
  name, tokens, cost, model changes and compactions. Under the RPC,
  `get_entries` returns `{head, entries}` (`all: true` includes abandoned
  branches for the tree view; entries carry `at` in milliseconds when
  known; an `Archived` entry is `{kind: "archived", archived}`, a `Spend`
  `{kind: "spend", usage, cost_usd}`, a `Resume_at` `{kind: "resume_at",
  at}` (ms or null), a `Reminders` `{kind: "reminders"}`; the frontends
  ignore the last three).
- `Compaction` — summarises older messages via the model and keeps a tail;
  manual (`/compact [instructions]`: the RPC's `instructions`, pi's
  `customInstructions`, are appended to the summariser's) or automatic.
  `plan` picks the cut: the earliest user or assistant message (never a
  tool result, so calls keep their results) such that the rest fits in
  `keep_recent_tokens`, else the last turn, and pins the user's latest
  message when it falls before the cut; the transcript sent to the
  summariser loses its middle if it would not fit the model.
  `compact_messages` works on a list (subagents), `compact` on a session
  (appending a `Compaction` entry); both return the summary and its usage.
- `Git_diff` — the pseudo-tool `$git_diff` on the active tool host:
  `git diff` against `HEAD` (or a base's merge base) plus untracked files
  as new ones, optionally one path, cut at 1 MB; errors say what to do
  (not a repository, no git, unknown base). `Review` writes `/review`'s
  prompt (the diff inlined when small) and `Init_prompt` `/init`'s; both
  go through `Agent.command_prompt`, which queues them like a prompt, with
  the long text as `Queued.expanded` and the `Injected.Source` as origin.
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
  end (the time frontends keep). A server tool block is `{type:
  "server_tool", id, name, input (JSON text), output?, is_error}` (no
  `raw`), streamed as the deltas `server_tool_use {id, name, input}` and
  `server_tool_result {id, output, is_error}`.
- `Rpc_server` — the connection and session manager: a table of live
  agents by session id, a table of clients, and per-connection
  `serve_lines` (`serve_connection` over newline-delimited flows, or one
  WebSocket text message per line: reader loop, one outbox fiber, and one fiber per
  request so a blocking method such as `shell` or a remote tool round trip
  never stalls the reader that must deliver the client's own
  `tool_exec_result`). Nothing waits for a peer: what a connection sends
  goes into its `Outbox`, a queue that never blocks the sender (an agent's
  events, the broadcasts to every client such as `sessions_changed`, other
  clients' requests), through `Update_coalescer`: a `message_update`
  carries the whole message so far, so those of one stream (the agent's or
  a subagent's) still waiting merge (latest `partial`, deltas joined) and a
  peer reading slower than the model writes gets fewer, current updates
  instead of falling behind by ever larger copies (they are rendered when
  written, `Outbox.send_late`). Before, a page that stopped reading (a suspended
  laptop, a phone gone behind a proxy that keeps its socket) filled a
  bounded queue, and then the agent emitting events to it, and every
  request broadcasting to all clients, waited forever: every session of the
  backend stopped until a restart. A peer more than 10000 messages (or
  256 MiB) behind, a failed write (`write_line` raising; a WebSocket that
  closed) or `Client.close` fail the outbox, which ends the connection: the
  reader stops, the write stuck on the peer is given up, and the requests
  still running are cancelled. Requests are also cancelled when the peer
  closes, unless `finish_requests` (stdio, which a script may close before
  reading the answers); one that raises is logged and answered with
  nothing, without ending the connection. `hello`'s optional `tab` names
  the page instance (prigh-web sends a random id per page load): a newer
  connection with the same `tab` ends the older one (`a newer connection
  from the same page took over` on stderr), whose network may have gone
  without a word. Every connection the backend ends says why on stderr
  (`prigh: closing the connection of NAME: …`). Session files are read by
  `load_session`: one parse per file at a time (others wanting it wait for
  it, and it finishes for the cache when its requester goes), yielding to
  the other fibers as it goes (an 80 MB file: other clients' pings waited
  920 ms, now 240 ms at most; parsing on another domain was tried, and
  requests the server's domain parsed meanwhile went missing: the JSON
  parsing is not safe across domains); a session opened by two clients at once is checked again after
  the parse, so it gets one agent. Agent events are routed to the clients attached to
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
  create or load an agent and move only the calling client (`new_session`,
  `switch_session`, `fork` and `clone` answer `{state}`, the new session's
  state, so a frontend needs no `get_state` round trip); `list_sessions`
  items carry `bytes`, `live` (with `clients` when live), `running` (a
  turn or background work), `background` (running background subagents +
  jobs, 0 when not loaded) and `archived` (computed with the namespace's
  current `config.json`, reread per call). `set_archived {path, archived}`
  (`path`: a session file or id) appends an `Archived` entry, through the
  live agent's session if loaded; archiving an active session is refused.
  `delete_sessions {paths}` and `delete_archived {}` answer `{deleted:
  [{path, id, bytes}], refused: [{path, error}]}`; `delete_session {path}`
  is the single form (its error the refusal's). A session is refused when
  it is the caller's own, the default (`-session`) one, running, open in
  other frontends, or unknown; otherwise its files go, it leaves the agents
  table, and its terminal is killed (`on_session_deleted`, wired to
  `Terminals.kill` by `main`, keyed by `terminal_key`; relayed ones are
  closed with `Terminal_relay.close_key`, and every connected host that ran
  a terminal for the key and announced the `terminal_kill` feature gets
  `{"event":"terminal_kill","key"}` and kills its shell; older hosts end it
  by their idle timeout). Every client of the server (not
  only the session's) gets `{"type":"event","event":"sessions_changed"}`
  when something `list_sessions` shows changes: a session's `running`,
  `background`, persisted, name or description (compared per live session
  on its state changes and message ends, so never per delta), and after
  archiving, deleting, importing, forking and cloning; frontends refetch
  the list. A session with running background subagents or jobs
  is not evicted (it delivers their reports into its own session even with
  no client attached); `shutdown` cancels them (killing the jobs).
  `kill_job {job_id}`, `job_output {job_id, lines}` (`{text}`: a header line
  and the last lines) and `list_jobs` (every job of the session: id,
  command, running, exit, delivered, elapsed, bytes, last_line) serve the
  `/jobs` picker, and `shell {command, background: true}` (`!&cmd`) returns
  `{job_id}`. `set_model` goes through `Model.resolve` (key, id,
  display name or unique case-insensitive prefix; otherwise "did you mean"
  by edit distance). Methods: `hello`, `ping`, `prompt`, `steer`,
  `follow_up`, `abort`, `dequeue`, `remove_queued` (`{queue: "steer" |
  "follow_up", index, text?}`, returning `{text, attachments}` like
  `dequeue`), `cancel_subagent`, `list_subagents`,
  `get_subagent`, `get_pending`, `kill_job`, `job_output`, `list_jobs`,
  `shell`, `get_state`, `get_messages`, `get_messages_page`,
  `get_outline`, `get_tool_output`, `get_entries`, `set_model`, `set_thinking`, `list_models`, `compact`,
  `new_session`, `switch_session`, `list_sessions`, `set_session_name`,
  `delete_session`, `delete_sessions`, `delete_archived`, `set_archived`,
  `export`, `import`, `fork`, `clone`, `rewind`,
  `session_stats`, `set_cwd`, `list_paths`, `list_dirs`, `get_config`, `set_config`,
  `change_default`, `btw`, `btw_cancel`,
  `tool_confirm_respond` (`{call_id, agent_id?, allow, always?, reason?}`),
  `set_mode` (`{approval_mode?, plan_mode?}`, answered with the state),
  `ask_user_respond` (`{call_id, answers: [[string]]}` or `{call_id,
  decline: true}`; the `ask_user` event and `get_pending`'s questions carry
  `deadline_ms`, null when it never times out, and `ask_user_done` says
  `timed_out`), `set_active_host`, `tool_exec_output`,
  `list_skills` (`{skills: [{name, description, path, model_invocable}]}`),
  `list_mcp {reconnect?}` and `mcp_approve {source, server}` (both
  `{servers: [{name, source, project, status: ready|failed|needs_approval,
  error?, tools: [{name, description}]}], problems}`, tools under their
  prigh names),
  `tool_exec_result`, `auth_status`, `login`, `auth_respond`, `auth_cancel`,
  `logout`, `set_budget {usd}` (a number, or null for none), `set_goal
  {text}`, `clear_goal`, `resume_goal`, `add_reminder {text, on: "stop",
  max_repeats?}` or `{text, every_turns}` (answers the reminder),
  `remove_reminder {name}`, `clear_reminders`, `cancel_resume`,
  `resume_now`, `git_diff {path?}`
  (`{root, against, diff, truncated}`), `review {args?}` (`[base]
  [instructions]`) and `init`, and the file browser's `fs_list`,
  `fs_preview`, `fs_mkdir`, `fs_rename`, `fs_delete`, `fs_download` and
  `fs_upload` (`Files_rpc`, above).
  `get_messages_page {limit, before?, from?, session?}` (`Message_page`,
  for frontends that open a session at its end) answers `{messages,
  older, before}`: the newest `limit` messages (a page never starts with
  a tool result or a compaction summary, so it may be longer), or those
  before the entry id `before`, or (with `from`, an entry id: what a
  frontend has loaded, after reconnecting) everything from that one to
  the newest; `older` counts the messages before them and `before` is the
  cursor of the page before. Tool outputs longer than 8 KiB come cut to
  their first and last 20 lines with `elided: {at, lines, bytes}`;
  `get_tool_output {call_id, agent_id?, session?}` has one whole (from
  anywhere in the session file, also before a compaction), and `get_subagent
  {elide: true}` cuts its transcript's the same way. `session` (a path or
  id) reads a session without opening it (the sidebar fetching ahead);
  sessions read from disk that way or evicted, and subagent logs, are
  kept (`File_cache`) while their files are unchanged.
  `get_outline {session?}` (`Outline`) answers `{points: [{index, kind,
  text, at?}]}`, the transcript's landmarks for a frontend that has only
  some pages loaded: the user's messages (what they wrote, or the title of
  their `/goal`, `/review`, `/init`; not notes alone or other messages of
  the harness's), the compaction summary and replies that ended with an
  error, `index` counting messages as a page's `older` does (so `limit:
  older - index` before the first page loaded reaches one) and `text` cut
  to 240 bytes.
  `State` carries `active_host` and `hosts` (the backend first),
  `budget_usd`, `goal` (`{text, status: active|done|stopped,
  continuations, max_continuations, summary?, reason?}` or null),
  `reminders` (`[{name, text, on: "stop", max_repeats, repeats}` or
  `{name, text, every_turns}]`, `repeats` counting the on-stop fires in a
  row since the user spoke) and `resume_at_ms`. A subagent's compaction arrives as a `compacted` event
  nested in a `subagent` one.
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
- `Guarded` — fibers on a long-lived switch that cannot fail it: every
  agent and listener of `prigh serve` shares one switch, so an exception
  escaping a run, a resume, a background task's bookkeeping or an accept
  loop would end the backend (every connection dropped, every session's
  background subagents and jobs lost). `Guarded.fork`/`run` log it (runs
  end with a `run failed` notice) and `accept_loop` retries a failed
  `accept`; `prigh tool-host` guards each request likewise.
- `Net_error` — a failed bind or connect as a short phrase ("address
  already in use", "connection refused") instead of Eio's exception dump,
  for `prigh serve`'s listeners and `prigh tool-host -connect`.
- `Websocket` — a minimal RFC 6455 server side (handshake key, frame
  encode/decode with client masking, fragment reassembly, ping/pong and
  close; `keepalive` pings every 15s while nothing is being written and
  shuts the socket down when the peer sent nothing for 45s since the last
  long write, or one write has been stuck for 5 minutes: a dead peer behind
  a proxy that keeps the socket open is noticed, and the read and write
  stuck on it end) and `Web_server` — the `-web` listener (every upgraded
  socket runs with `keepalive`, which cancels its handler): a connection whose first
  byte is `{` is a plain JSON-lines client (the TUI's `-connect`) and goes
  straight to `Rpc_server.serve_lines`, so one port serves terminals and
  browsers alike; otherwise one HTTP/1.1 request per connection, `GET /ws`
  upgraded and handed to `Rpc_server.serve_lines`, `GET /terminal` upgraded
  and handed to `Terminals` (below), `/files/download` and `/files/upload`
  to `File_http` (authorised by their tickets),
  anything else served from the web root (the built `frontend/web/bin/site`,
  found via `-web-root`, `$PRIGH_WEB_ROOT` or next to the executable)
  by `Static_site`: no `..`, no dot files; `Cache-Control: no-cache` with
  an `ETag` (content hash, cached per file version) and `Last-Modified` (not for Nix's mtime 1), so reloads revalidate to
  `304`; a precompressed `file.br`/`file.gz` no older than the file is
  sent with `Content-Encoding` when `Accept-Encoding` allows it (`Vary:
  Accept-Encoding`). The Nix build adds those siblings to the installed
  sites (zopfli, brotli); local dune builds stay uncompressed. Upgraded sockets set `TCP_NODELAY`, so that a
  large reply's last segment does not wait for a delayed ACK. The token check is the same `hello` check as
  for TCP; the static files are public.
- `Rpc_router` / `Namespace` — `-tokens name=token,...`: one `Rpc_server`
  per namespace, each with its own home (sessions, config, `auth.json`,
  global `AGENTS.md`) under `~/.prigh/namespaces/<name>` and no provider keys
  from the environment. Every listener goes through the router, which reads
  a connection's first line (a `hello` with a known token) and hands the
  connection to that namespace's server; `/terminal` looks the
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
  `set_user` with a `hello` of the first one's client details.
  `/terminal` URLs carry `as_user` in their query instead. Without the backend host,
  `Rpc_server.session_file` keeps session paths inside the sessions
  directory and `Agent.export` writes through the tool host.
- `Terminal_channel` / `Terminal_relay` — terminals run on the session's
  active host (`Rpc_server.terminal_target`). `Terminals.serve` speaks to an
  abstract frame channel (a WebSocket, or frames fed by a relay). For a
  client host the backend relays the browser socket as `terminal_open` /
  `terminal_frame` / `terminal_close` (and, once the session is deleted,
  `terminal_kill`) events to that host and its
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

### CLI (`backend/bin/main.ml`)

`serve` (RPC on stdio; `-listen HOST:PORT` accepts TCP clients instead,
`-web HOST:PORT` serves prigh-web, WebSocket clients and TCP
`-connect` clients on one port (`-open` launches a browser, `-web-root DIR`
overrides the site), `-stdio` as well,
`-token SECRET`/`$PRIGH_TOKEN` gates them; with stdio the backend exits when
the spawning frontend closes it, with `-listen`/`-web` only it runs until
killed), `tool-host` (the local tool worker), `run <prompt>`
(headless, streams to stdout; it denies confirmations and answers
`ask_user` as unavailable, since nobody is there), `sessions`, `login <provider> [-method]`,
`logout <provider>`, `auth`. All commands share `-auth-file`; `run`/`serve`
share `-model`, `-thinking`, `-session`, `-cwd`, `-no-tools`, `-faux` (and
`-faux-script FILE`, a JSON array of scripted replies that implies `-faux`);
`-session` takes a saved session's id, a unique prefix of it, or its file;
for `serve`, it is the default session, the one a client lands on
when its `hello` names none; without it every new client starts in a fresh
session of its own (sharing one is explicit: `hello` with the session id or
path, which is also how a frontend reattaches after a reconnect). With no explicit model, a new
session uses the config's `default_model`, else the first logged-in
provider's in the order anthropic, openai-codex, openai, deepseek.

## Frontends (`frontend/`)

Built with OxCaml 5.2 and the Jane Street `v0.18~preview` packages, like
the backend (one toolchain, `nix develop`; the few backend dependencies
that need adapting to it are patched in `nix/opam-repository`). The two
are separate dune projects and only meet over the wire, so the frontends
own their copy of the protocol types and the e2e tests guard the
contract. `protocol/`, `client/` and `ui/` are shared; `tui/` is the
terminal frontend and `web/` prigh-web, the browser one.

- `protocol/` (`prigh_protocol`) — `Jsonaf` decoders for everything
  `Rpc_json` emits (`Message`, `Delta`, `State`, `Model`, `Session_summary`,
  `Auth_status`, `Auth_event`, `Event`, `Server_message`, `Skill`,
  `Mcp_server`, `Mcp_list`, `Delete_result`, the file browser's
  `File_listing` and `File_preview`) and the `Request` encoder
  (`Request.Method` has typed constructors for `list_skills`, `list_mcp`,
  `mcp_approve`, `set_archived`, `delete_sessions` and `delete_archived`).
- `client/` (`prigh_client`, `Async_kernel` only so it links under
  js_of_ocaml) — `Transport.t` (a line channel; an in-memory pair for
  tests) and `Client` (created with a `connect` thunk and
  reconnectable: correlates responses by id, fans out events, stderr and
  `Closed` on one `Incoming.t` pipe that outlives the transport, fails calls
  with "not connected" in between; `received` and `waiting` count the lines
  received and the calls waiting, for prigh-web's probe). `client/unix/` (`prigh_client_unix`)
  has the transports that need a process or a socket — `Stdio_transport`
  spawns the backend, `Tcp_transport` connects to `-listen` — and
  `Tool_host` (spawns `prigh tool-host` lazily and proxies
  `Tool_exec`/`Tool_exec_cancel` events to it and its `output`/`result`
  lines back as `tool_exec_*` requests, a result's `images` verbatim) and
  `Host_id` (the machine's tool host id in `~/.prigh/host-id`, sent in
  every `hello` with local tools).
- `ui/` (`prigh_ui`) — **platform-agnostic**, depends only on `core` and
  `bonsai`; the terminal mounts its `App`, and prigh-web shares its
  screen-independent parts (commands, fuzzy matching, tool summaries...).
  - `App` is an Elm-style pure state machine: `update : Model.t -> Action.t
    -> Model.t * Command.t list`. Actions are keys/intents, backend events,
    RPC replies (tagged with `Reply_tag.t` so they stay sexpable), clock
    ticks and resizes. The prompt history is the backend's (`get_prefs`
    of `history` at startup, `push_pref` per prompt, `prefs_changed`
    while not browsing it), shared with prigh-web; `~/.prigh/history`
    is read first, kept as well, and uploaded when the backend has none.
    Commands (`Rpc`, `List_paths`, `Load_history`,
    `Append_history`, `Copy_to_clipboard`, `Suspend`, `Edit_externally`,
    `Open_browser`, `Quit`) are executed by the platform and answered through
    `Action.Reply`.
  - `Component.create ~platform` wraps `App` in `Bonsai.state_machine`,
    turns commands into effects and feeds replies back; it also runs the
    spinner clock while a turn is active. `@` path completion is an RPC
    (`list_paths`), answered by the active tool host, so the paths are the
    tool host's.
  - Reconnection is App state (`Connection.t`), so the policy is an expect
    test: `Backend_closed` emits `Command.Reconnect {generation; delay_ms;
    session}` (immediately, then 250ms doubling to a 10s cap), the platform
    sleeps, connects and re-sends `hello` for the current session, and the
    reply comes back tagged with its generation so late attempts are ignored;
    `/retry-backend-connection` issues a new generation with no delay.
    Success clears the transcript and re-runs the startup requests.
  - Switching servers is App state too, where the platform reports a
    `Server_link` (the terminal does). `/connect` and the `/signin` dialog (`Sign_in_form`, a
    `Mode.Sign_in`) emit `Command.Relink {generation; change}` and enter
    `Connection.Switching {generation; previous}`; the platform answers
    `Relinked` with a `Server_link.Outcome`: `Unreachable` (nothing changed:
    back to `previous`), `Rejected` (the old connection is gone: on a bad
    user or token, `Disconnected` with the dialog open, showing the error)
    or `Connected` (everything of the old connection is dropped,
    `reset_connection`, and the startup requests run again). A `Rejected`
    action (the first `hello`), a reconnect refused for its credentials and
    `/signout` (after `Command.Sign_out`) also end in `Disconnected`, which
    never retries; `Backend_closed` is expected while switching or
    disconnected and ignored.
  - Headless widgets: `Editor` (multi-line, kill ring, undo, chips for long
    pastes, persistent history), `Picker` (fuzzy list with `Fuzzy` ranking),
    `Transcript` (items plus streaming tails; `Transcript.apply` is the one
    event→transcript function, used for the main transcript and each
    subagent; it renders items as `Log_line`s, lines with a two-column
    gutter that holds a turn's bar or a step's mark and survives wrapping,
    and each tool call through `Tool_render`, which reads arguments with
    `Tool_args` and `apply_patch`'s per-file changes with `Patch` (both
    shared with prigh-web's `Tool_view`)), `Viewport` (`Follow | Anchored`, so new output never pushes an
    anchored view), `Verbosity` (quiet/normal/verbose), `Autocomplete`
    (inline command/argument/path completion), `Agent_view` (per-subagent
    transcript and status; the app keeps an agent while it runs or while
    `State.subagents` lists it as undelivered, then until the next prompt;
    a delivered report, a subagent's or a job's, renders as a compact
    `Transcript` `Delivery` item; `/jobs` lists `list_jobs` in a picker,
    Enter adds `job_output` to the transcript as a block, Ctrl+D kills),
    `Commands` (slash table, parse, complete,
    closest), `Model_match` (display-name/prefix/did-you-mean), `Markdown`,
    `Btw_box` (the `/btw` panel above the composer: a newer question cancels
    and replaces it; Esc dismisses it before Esc's other meanings, so it
    never aborts the run), `Boxed` (the framed dialogs, with an optional
    footer in the bottom border), `Output_box` (long command
    output: `Mode.Output`, a scrollable box that is not a dialog, so a
    confirmation or a picker replaces it; typing closes it into the editor),
    `Notice` (the acknowledgement slot: `App.notice` sets it, the next key
    press, Esc or its ticks retire it; `App.log` is for what belongs in the
    transcript).
  - Skills and MCP: `/skills` and `/mcp` are pickers over `list_skills` and
    `list_mcp` (Enter on a server needing approval sends `mcp_approve`,
    whose reply is the new list). `/skill:NAME ARGS` is sent as a prompt
    tagged `Skill_prompt`, so a failure (an unknown name) puts the text back
    in the editor and takes it off the queue. `Autocomplete`'s `Skill`
    source completes names from `Skill_cache`, which is keyed by session,
    tool host and directory (a stale key or reply fetches again or is
    dropped) and emptied on reloads, `/setusr` and reconnects.
    `Skill_message` parses the backend's expansion (`<skill name=…
    location=…>`) back into name, file, body and arguments (prigh-web uses
    it too): `Transcript`
    renders it as a `Skill` item (`skill NAME` and the arguments; the body
    only in verbose), and `/fork` lists and restores it as
    `/skill:NAME ARGS`.
  - Sessions: `/sessions` is a picker over `list_sessions` laid out by
    `Session_selection` (active sessions first, then the archived ones,
    dimmed: `Picker.Item.section` keeps sections apart, also when filtering,
    and `Render` draws a divider naming each). Tab marks rows
    (`Picker.toggle_check`); Ctrl+D asks to delete the marked (or the
    highlighted) sessions and Alt+D every archived one but the current, with
    the count and `bytes` (`Confirm_action.Delete_sessions` carries the paths
    confirmed and the picker as `Open_picker`, so Esc and Enter go back to
    it); the session you are in is refused up front
    (`Session_selection.deletable`), and the `delete_sessions` reply
    (`Delete_result`) is the notice, refusals included. Alt+A flips `archived` in the picker at once
    and sends `set_archived` (a refusal refetches the list). `/archive`,
    `/unarchive` and `/delete` look their arguments up in a fresh list
    (`Session_selection.resolve`: path, id, unique id prefix or name).
    `sessions_changed` refetches `list_sessions` (`Sessions_refresh`) while
    the picker is open (or behind its confirmation) or the completion's
    list is cached; `Picker.set_items` keeps the query, marks and highlight
    (Ctrl+N keeps marks on the sessions it hides and moves a hidden
    highlight to its neighbour). Their completions (`Autocomplete`) show
    names, active sessions first, archived ones marked, match on name,
    first prompt, id and directory, and insert the id; `/archive` offers
    the active ones, `/unarchive` the archived ones, `/delete` all but the
    current one.
    `/tree` hangs each message under the nearest message above it, skipping
    settings entries (model, name, cwd, archived).
  - Snippets: `Snippet_command` (shared with prigh-web) turns `/snippet`'s
    arguments into an action: the picker (also Alt+N, `Snippet_picker`),
    inserting at the cursor, or the whole new `Config.snippets` list to save
    with `set_config` (only that field; `Snippets_saved` acknowledges with
    the config the backend replies). `Autocomplete`'s `Snippet` argument
    completes names, `add` and `rm`; its `Snippet` source completes `#name`
    at a word start (a letter or digit first, only when a snippet matches),
    and `Autocomplete.insert_snippet` swaps `#name` for the text with
    `Editor.replace` (one undo step, the cursor after it).
  - Model hand-over and default directory: `/fallback` and `/default-dir`
    show `Config`'s `fallback_models`/`default_cwd`, or send `set_config`
    with only that field. The backend keeps the fields a `set_config`
    leaves out, so every one the app sends (`/confirm`, `/scoped-models`
    too) carries only its own field and a stale or missing `Config` cannot
    undo another change. A refusal (an unknown model, with the backend's
    suggestions) puts the command back in the editor. `Autocomplete`
    completes `/fallback` word by word (the models not listed yet, `off`
    first) and `/default-dir` with `list_dirs` on the backend's host when
    it runs tools.
  - Harness notes: `Message.User`'s `origin` (`P.Origin`) splits a user
    message into notes, the user's `body` and attachments, or marks it as
    the harness's (`source`). `Transcript` shows notes and sources as quiet
    `Note` lines (`Injected_note`'s glyph and title, e.g. `↪ Handed over
    from A to B`), the raw text only in verbose, and the user's turn with
    only what they wrote; `/fork` and `/tree` label messages the same way.
  - `/diff` sends `git_diff` and `Diff_files` (shared with prigh-web)
    parses the result into files with their counts, which the transcript
    shows coloured; `/review` and `/init` send `review`/`init` tagged so
    a refusal goes back to the editor.
  - Notifications: `Notify` decides what is worth telling (a main-agent
    turn's end, a tool confirmation, delivered background work) for both
    frontends. The TUI emits `Command.Notify` only while the terminal has
    reported losing the focus (`tui/lib/`'s `Term_focus` enables focus
    reporting and filters `CSI I`/`CSI O` out of the input), and
    `Term_notify` writes the bell or OSC 9/777 (wrapped for tmux) per the
    config's `notify` (`/notify off|bell|desktop`).
    `Loop_status` (prigh-web uses it too) words the budget, goal and
    scheduled resume for the status line (`$1.20/$5.00`, `goal:3/20`,
    `resumes 14:05` in the frontend's zone, from `Set_utc_offset`) and for
    `/budget`, `/goal` and `/resume` without arguments, and parses
    `/budget`'s amount; their argument completion only offers a keyword
    (`off`, `resume`, `now`, `cancel`) the typed text starts, so Enter sends
    a goal as typed.
  - Provider-side tools, `web_fetch` and `job_input`: `prigh_protocol`
    decodes the `server_tool` content block (`Content.Server_tool`) and the
    `server_tool_use`/`server_tool_result` deltas. `Transcript.apply` turns
    them into a `Server_tool` item placed among the reply's text (a use
    delta adds it or updates it by id, a result fills it in, `message_end`
    has the last word), rendered by `Tool_render.server_tool` as a step
    (`web_search "query"`, running until it has output, `N results` with
    the first five linked in normal and all in verbose, red when
    `is_error`). `Tool_summary` (shared with prigh-web) reads search
    results and `web_fetch`'s `URL (details)` header and words `job_input`
    calls (`job_input j1 "print(1)\n"`, `close stdin`); `web_fetch` shows its
    URL as a link, the details as a chip and the page cut like other
    output. `Event` has `Tool_exec.stdin` and `Tool_exec_input`; the
    terminal's `Tool_host` forwards them to its `prigh tool-host` worker
    (`"stdin": true` on `exec`, `{"type":"input",…}`), and
    `Backend_link.connection_hello` with local tools announces `"features":
    ["exec_input", "terminal_kill"]` (`Terminal_kill` goes to the worker,
    which kills that session's shell).
  - `Key.t` → `Intent.t` through `Keymap` (the one binding table; `/help`
    prints it). `Mode.t` (`Editing | Picker | Login_prompt | Text_prompt |
    Confirm | Question | Sign_in | Search`) says who owns the keyboard;
    dialogs never stack, Esc always closes.
  - Approvals, shared with prigh-web: `Session_mode` (the approval mode, or
    plan mode, as the status line shows it and as prigh-web's chip names it,
    `title`; Alt+M's cycle; `set_mode`'s parameters), `Confirm_prompt` (a confirmation's title and answers with
    their keys, y/a/n/r, and `tool_confirm_respond`'s parameters),
    `Question_form` (answering an `ask_user` call with keys), `Plan_view`
    (`plan 2/5: step`, the checklist); the backend's plan mode notes are
    harness notes (`◇ Plan mode on: ...`). Confirmations and questions queue in the
    model (`pending_confirms`, `pending_questions`) and open one at a time
    when the editor has the keyboard (a question's box says when its
    deadline is, `Question_form.deadline_note`); `tool_confirm_done`/`ask_user_done`
    close one answered elsewhere (or timed out: quietly, the backend's
    notice explains), `get_pending` at startup brings back what
    is still waiting. Replacing the session shown (a reload, `/setusr`, a
    reconnect) goes through `reset_session`: it closes the dialogs that
    answer the old session's agent (the reason prompt gives the draft back),
    drops the waiting confirmations, questions, queue, agents, focus and
    transcript, and fetches `get_state`, `get_messages` and `get_pending`,
    whose reply also sets the queue. Deny with a reason is a
    `Text_prompt` (`Deny_reason`, keeping the editor's draft; Esc goes back
    to the confirmation). `Tool_render` draws `update_plan`, `ask_user` and
    `exit_plan_mode` calls.
  - `Layout` computes the frame's fixed zones from the size, the sidebar
    preference (off by default, Alt+L) and the editor's rows: main area
    (sidebar of `Layout.sidebar_width` columns at `Layout.sidebar_threshold`
    and more, the transcript beside it), the composer's rule, the editor
    (capped by `Layout.editor_cap`, then it scrolls) and the status line. Only the
    editor's height moves the others; `App` uses the same layout for
    scrolling and picker pages.
  - `Render.screen : Model.t -> Screen.t` lays out a frame as `Content.t`
    (styled spans with `Text_width`-aware wrapping) plus the cursor cell.
    Overlays (the mode's rows: picker, completion, confirmation, question,
    sign-in, login, text prompt, search, output; in `Editing` also `/btw`,
    the queue and a notice too long for the rule) are stacked bottom-up
    in the transcript's area, the optional ones giving way first when the
    area is short; the transcript's tail ends above them (scrolled back,
    they cover its bottom rows). The cursor stays in the editor unless typing goes
    into an overlay (picker filter, search, question, sign-in, the deny
    reason). The rule is plain; a short notice sits on its right. The
    status line keeps the cwd and model, then fills remaining width by
    priority (mode hint, scroll position, context, queued/agents/jobs and an
    approval mode that asks, then thinking, `mode:auto` and cost, verbosity
    last; a mode hint too wide to fit whole drops its last keys, the least
    important, after the context has its place; dialogs list the rest of
    their keys in the box: the sessions picker on its title row, a question
    under its answer, the reason prompt is typed in its box under the call)
    in a fixed order, dropping whole parts: the verbosity gives way to the
    cwd, the cwd to everything else, and the model is never cut. It is
    gray; yellow marks modes, running work and warm context, red a full
    context or a lost backend.
  - `Queued_message` is a queued steer or follow-up (kind, index in its
    queue, text) as `queue_update` lists them (whichever frontend queued
    them), shown above the rule; the TUI adds what it sends at once, the
    next `queue_update` replaces the list. `/queue` is a picker over them:
    Enter (`remove_queued`, then into the editor), Ctrl+D (`remove_queued`).
    prigh-web shares the module.
  - Replies tagged `Retry_in_editor` put the command back in the editor
    with a hint when the backend refuses it (`/switch`, `/cd`).
- `tui/` — the terminal frontend.
  - `lib/` (`prigh_tui`) — `Key_of_event` (Bonsai_term events → `Key.t`,
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
    `Backend_link` is the platform's side of the connection: the target (a
    spawned `serve` or `HOST:PORT`), the credentials and the tool host. The
    `Client`'s transport factory reads the current target, so reconnects
    follow switches; `relink` opens the new transport before dropping the old
    connection (an unreachable server changes nothing), closes the old tool
    host and makes one for the new target (`-tools` rules as at startup:
    local for a remote backend unless `-tools` was given), sends `hello` and
    saves the connection through `Saved_connection` once it is answered
    (`resolve`'s rules for `/connect`, the new credentials for `/signin`);
    `sign_out` forgets the token there and in the saved file. A `Closed` seen
    while a newer transport is attached is dropped (`handle_incoming`), and a
    reconnect overtaken by a switch gives way.
    `Term_app.run` sends `hello` (`Backend_link.connection_hello`: with local
    tools, a `host_id` for the life of the process, also sent on reconnects)
    before mounting the app; a remote backend refusing the credentials does
    not stop it (the app starts with `Rejected` and the sign-in dialog).
    It sends `Server_link`, then the reply as `Hello` and `Start`; `client_id`
    is our host id (`Hello_reply` prefers the reply's `host_id`), so `/host`
    can mark this frontend as "(here)"
    and the status line can show `tools:<host>` when tools run elsewhere or
    `tools:offline` when the active host is gone. Mouse reporting is on so the
    wheel scrolls the transcript (`Scroll_up`/`Scroll_down`, three lines);
    without it the terminal turns the wheel into arrow keys, which walk the
    prompt history. Text selection therefore needs Shift.
  - `bin/` — `prigh-tui [DIR]` (`-faux`, `-session`, `-model`, `-cwd` (same
    as `DIR`), `-auth-file`, `-backend`; `PRIGH_BACKEND` overrides the backend
    path). `-connect HOST:PORT` (`$PRIGH_CONNECT`) joins a running backend
    instead of spawning one, with `-token` (`$PRIGH_TOKEN`), `-user`
    (`$PRIGH_USER`) and `-name`; the flags are remembered in
    `~/.prigh/tui-connection.sexp` (`client/unix/saved_connection.ml`, which
    resolves flag > environment > saved; `-local` ignores it, `-forget`
    deletes it); `/connect`, `/signin` and `/signout` change it from inside
    (`lib/backend_link.ml`). `-tools local|remote`
    says where this session's tools run (local = this machine through
    `tool-host`, the default with `-connect`; remote = the backend, the default
    when spawning, where the two coincide). `-detached` (`-tmux-session`)
    runs the same command in a new detached tmux session and exits: the
    argv (session name, `-e` environment, a `sh` wrapper that waits for
    Enter when the TUI fails) is built by `lib/tmux_detach.ml`; the parent
    validates the flags but neither saves nor spawns anything.
  - `test/` (the `App.update` scenarios, the widgets, the client),
    `e2e/` and `tmux-test/`: see Testing.
- `web/` — prigh-web, the browser frontend. Unlike `tui/` it does not mount
  `ui/`'s cell-grid `App`: it has its own Elm-style state machine over the
  same `prigh_protocol` types and `prigh_client`, renders HTML, and shares
  the parts of `ui/` that are not about the terminal's screen.
  - `lib/` (`prigh_web`, pure: `core` + `virtual_dom`):
    - `App` — `update : Model.t -> Action.t -> Model.t * Command.t list`;
      commands are `Rpc` tagged with a `Reply_tag.t`, `Reconnect`,
      `Set_url_session`, history saves, focus and toast expiry. Startup asks
      for `get_state` and `list_models`; a state with a new session id
      resets everything that belongs to the session and fetches its
      messages and the session list, and `new_session`/`switch_session`/
      `clone` are tagged `Reload_state`, whose reply carries the new
      state, so every way of changing session goes through that one path. Reconnection
      (`Rejoin`): `Backend_closed` → `Reconnect` with backoff behind a
      banner, keeping everything on show (transcript, draft, attachments,
      panels, dialogs and the confirmations and questions waiting). Back,
      the state, the messages (`get_messages_page` tagged `Resync`: `from`
      the first one on show when older ones are not loaded, else all of
      them, or the newest page while the session was still loading) and
      `get_pending` (`Resync_pending`) are fetched again: `Chat.reconcile`
      merges the messages into the transcript, keeping the same entry and
      call values where nothing changed (so `Chat_view`'s caches, and the
      DOM and its scroll position, stay, and so do the older pages loaded
      and outputs fetched whole), and only what is no longer waited for
      closes; when what was on show has gone from the session (rewound
      meanwhile) its newest page replaces it. `Network false` drops the socket (`Drop_connection`);
      `Network true` and the page shown again reconnect at once with the
      backoff reset, or, while connected, `Probe_connection` (a `get_state`
      that must answer within 8s, else the socket is dropped; a minute at
      most while other answers are still on their way or anything keeps
      arriving, since on a slow network ours queues behind them: dropping
      such a working connection, then fetching everything again over it,
      was a loop). `Drops` counts the connections lost before they brought
      the session back (state, and the messages fetched again): each such
      loss starts the next attempt further into the backoff (250 ms
      doubling, 30s at most; the page adds ±20% jitter), and after
      `Drops.give_up_after` (6) in a row the page stops (`Connection` stays
      `Reconnecting` with nothing scheduled; the network or the page shown
      again do not restart it) and the banner says "The backend keeps
      dropping the connection while loading this session: open another
      session, or reload" with Retry now (one more attempt), Another
      session and Reload (`Reload_page`, without or with `?session=`); the
      status line says "Stopped reconnecting". Before, a connection lost
      after its `hello` was answered was retried at once, without backoff,
      with a "Reconnected" toast each time ("Reconnected ×86" in
      production). A connection that brings the session back starts the
      count over, and only then says "Reconnected" (`settle`, after every
      update). Failing to connect at all (the backend restarting) never
      gives up. The socket must open within 10s and `hello` answer within
      30s more (the backend may be reading a large session for it), and
      `Reconnect_gate` lets the newest attempt say hello also on a socket
      an older attempt opened (whose late answer no longer counts: the page
      stayed "reconnecting" on an open socket). A refusal
      (`unauthorised…`) is `Auth_failed`, the only way back to the sign-in
      page; a session the backend no longer has (`Session_gone`, from the
      page's `hello` falling back to no session) stays on show read-only
      (`session_gone`) with a banner offering a new session or the list,
      unless it was empty. `!cmd`/`!!cmd`/`!&cmd` go to `shell` like the TUI's.
      `run_command` has every command of `ui/`'s `Commands` (plus `/copy`;
      `/agents` and `/jobs` open, select and stop items in the agents
      panel); `Model` also holds the transcript `verbosity` (a class on
      `#chat`: the CSS hides tool output in quiet and opens every
      `<details>` in verbose) and `folds`, the outputs toggled from what it
      shows (`Fold`), what we wait on the backend for (`In_flight`, below),
      the `Config` (`get_config` at startup: the scope Ctrl+P
      cycles, `/confirm`), the side question (`Btw`, streamed by
      `btw_delta`), the saved accounts and whether we may act as others
      (`list_users` at startup succeeds only for superusers). `set_user`'s
      reply resets everything that belonged to the old user like a new
      session does, then starts over.
    - Loading states: `In_flight` is what the page waits on the backend for,
      so that every action shows within a frame: the session being opened
      (`Switching` a path, `Starting` a new one, a clone or fork: until the
      `get_state` we asked for answers) and its `get_messages` (`View` shows
      a placeholder instead of the chat, also before the first state; the
      sidebar marks the session clicked), prompts sent until the
      transcript's next user message (echoed under it as "Sending…") and
      prompts, steers and follow-ups not yet answered (the send button and
      the status line), a model or thinking level chosen until a state
      has it or `set_model`/`set_thinking` (`Model_set`, `Thinking_set`)
      answers, and an approval mode until a state has it or `set_mode`
      fails (the composer's chips, disabled). A command that needs a list
      from the backend opens `Dialog.Loading` at once; `App.reply` gives the
      replies it waits for (`loads`) to the handler only while it still
      shows, so Esc meanwhile has no side effects.
    - Paging: opening a session asks for its newest page
      (`get_messages_page`, `Paging.size` messages, tagged `Page`) along
      with `switch_session`; `Paging` above the transcript says how many
      older messages there are, with a button, and `Paging_listeners` asks
      for the page before (`Load_older`) as the user scrolls near the top,
      then keeps what shows in place while it is added above
      (`Keep_scroll_position`; `Chat.prepend` numbers entries downwards,
      so the keyed entries already shown are not redrawn). `Chat_cache`
      keeps the transcripts of the last 8 sessions shown (by path): going
      back to one shows it at once, then its newest page refreshes it (kept
      as it is, views and all, when unchanged); the sidebar fetches a
      session's newest page ahead when the pointer rests on it, focuses or
      presses it (`Prefetch_session`, one at a time); a session gone from
      `list_sessions` leaves it (`Chat_cache.retain`). A `compacted` event
      leaves nothing older to load (`Paging.all`). Calls running while the
      page did not follow (a cached chat of a running session, a page of
      one, a lost connection) are `Tool.missed` (`Chat.missed_output`): the
      card says the output from meanwhile shows with the result. Outputs cut by the
      backend (`elided`) show their first and last lines, and unfolding one
      fetches it whole (`get_tool_output`, `Load_output`).
    - Jumping around: `Jump` (pure) gives the transcript's landmarks
      (`Point`s: the user's messages — what they wrote, a skill as
      `/skill:NAME`, their `/goal`/`/review`/`/init` — compactions and
      replies that ended with an error), oldest first: those before the
      first message loaded from the session's `get_outline` (`Model.jump`,
      asked for with each newest page while older messages are not loaded,
      dropped on switching session and on `compacted`; an older backend
      without it only loses those), then those of `Chat`. A `Target` is a
      loaded entry (`Entry n`, which `Chat_view` marks `data-entry=n`) or a
      message not loaded (`Message index`): `Jump_to` the latter asks for the
      page from it to the first loaded in one request (`limit: older -
      index`, tagged `Page` like `Load_older`; while another page is on its
      way, once it is in) and `Jump.entry_of_index` finds its entry in the
      page added, for `Command.Scroll_to_entry`. `Jump_view` (beside the
      chat, in `.chat-frame`, once the user has sent two messages) is the
      jump list's button and, but on a phone, the rail: a mark per landmark
      (at most ~120, picked evenly), evenly spaced rather than by height so
      that each is as easy to hit and the marks not loaded have a place,
      each a button with the message's first line and time as its label and
      tooltip (`tabindex=-1`: the list is the keyboard's way); made again
      only when the landmarks change, so not per streamed delta or key.
      Ctrl+J (`Open_jump_list`) is a `Picker` of the user's messages, newest
      first. Ctrl+↑/↓ stay the page's (`Jump_to_user_message`), which sends
      `Jump_older` when none is above the first loaded. `app/`'s
      `Jump_listeners` marks the rail's mark of the landmark at the upper
      third of the view (`aria-current`, as the chat scrolls) and the entry
      jumped to (`.jumped`, a fading sodium ring; still, under reduced
      motion, until taken off).
    - The transcript: `Chat` (one agent: a page of messages then events; each
      tool call's streamed output and result; subagents' nested chats,
      fetched with `get_subagent` after a reload since `get_messages` only
      has their reports; `!cmd` runs as `Shell` entries), `Chat_view` (each
      entry, and the whole transcript, a `Node.lazy_` cached by `View_cache`
      on the value: virtual_dom skips what did not change, so a streamed
      delta or a key press costs the same in a long session as in a short
      one; the cache's deps include the entry's folds; sidebar and file
      rows are cached the same way, also keyed on the `inject` their
      handlers capture),
      `Tool_view` (a card per tool: bash, read with images, write, edit
      with `Line_diff`/`Diff_view`, apply_patch file by file,
      ls/grep/find, subagents (marked with `data-call` and `data-agent`
      for the agents panel), jobs and `job_input`, `web_fetch` with its URL
      as a link and the page folded, and `server_tool`, the card a
      `web_search` block gets among the message's text, from the streamed
      partial message: the work lamp while it runs, then a folded list of
      result links),
      `Delivery_view` (reports of finished background work, parsed by
      `ui/`'s `Delivery`), `Markdown` (a total parser that also renders
      streaming prefixes) and `Markdown_view`, `Image_view`,
      `Output_view`. Outputs fold to their first 6 lines (an error's last
      6; a read file none, diffs 8 rows), faded by a CSS mask where they go
      on, behind `Fold.button`s: a `data-fold` key
      (the call id, scoped by the subagent's id in nested transcripts, or
      `job:<id>` in deliveries) that `Chat_listeners` turns into
      `Toggle_fold`; folded text is not rendered. Running calls show only
      their last 6 lines (`Output_view.live`, no toggle), and `Chat` keeps
      only the last 32-64 KB of streamed output with a count of its lines,
      so a long stream costs the same per chunk. `Markdown_view` cuts
      text at `Markdown.block_starts` (top-level blocks after a blank line
      that cannot continue a list or quote) into `.md-part`s, each parsed
      once and reused as a lazy node: while a reply streams only the last is
      parsed again per delta, and a reply that ends is drawn in the same
      parts, so its DOM is patched, not drawn again (the selection and
      everything else on it stay). Messages' times (`at`; the page stamps live events
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
      flight, a job's polled `job_output`) and `Agents_view` (the list as
      a tree of buttons, `aria-keyshortcuts` for Alt+N; outcomes from the
      report's prefix: `[cancelled]`/`[interrupted]`/a killed job are
      "stopped", not failed; a subagent in full — its transcript is the one in `Chat`, found by
      agent id at any depth, fetched with `get_subagent` when nested ones
      are missing after a reload — or a job's output, with what it is
      (crumbs, task, state, data, Cancel/Kill, "Show in chat") in a footer
      pinned under the scrolling body; the status line's
      summary and the toggle in the chat's corner or the bar). Elapsed times tick with `Clock`,
      which the page sends every second only while the panel is open with
      something running (`Model.ticking`); running jobs are then polled
      every 2s. `Reveal` scrolls the chat to a subagent's card through the
      `subagent` call ids from the top-level one down.
    - The terminal panel: `Terminal` (open, a
      generation that "New shell"/"Retry" bump, and the widget's last
      `Status` with the key of the target it is about) and
      `Terminal_view` (a header with the active host's name and directory
      and the connection's state, a notice when the shell exited or could
      not start — `Terminal.advice` says what to do — and the session's
      toggle). The view does not know xterm.js: `View.view` takes the
      widget as a function of a `Terminal.Target.t` (session, active host
      and whether it is connected, the user acted as, generation), so
      switching session, host or user, the host coming back or a new shell
      is a new target and a new connection, and tests print the target.
      Keys inside the panel (`Keys.Target.Terminal`) are the shell's, Esc
      and Enter included, even with a dialog or a tool confirmation
      showing; only Ctrl+` (by `code`) is ours, toggling the panel.
    - The file browser: `Files` (the panel's state: where it belongs —
      session, host, cwd, with `set_where` resetting it and dropping the
      attachments when the session or host changes —, the listing and the
      path being listed, so stale replies are ignored, sort, dot files, the
      selection, the preview, uploads into the directory with the
      conflicts waiting to be confirmed, its dialog, and the composer's
      attachments with their upload state). Its `update`/`reply` return
      `Out.t`s that `App` turns into `fs_*` requests (`Reply_tag.Files`),
      toasts, the clipboard, text for the composer and `Command.Files`
      page commands (`Pick`, `Upload {file, url}`, `Abort_upload`,
      `Forget`, `Download`, `Focus`); the page's files are ints, so the
      model stays pure. `send` adds the ready attachments' paths
      (relative to the cwd) as `attachments` and refuses while one
      uploads or after one failed; `Sent_with_files` puts them back if
      the prompt fails. `Files_view` draws the drawer (breadcrumbs or the
      path field, sortable columns, rows, the selected entry's actions,
      the preview, the transfers with the work lamp), its dialogs, the
      toggle in the chat's corner or the bar, the composer's chips and paperclip, and the hidden
      file pickers. Keys: Ctrl+Shift+F everywhere; in the list
      (`Keys.Target.Files`) ↑↓, Enter, Backspace, F2, Delete, Esc; with
      its dialog open, Enter and Esc.
    - Panel sizes: `Resize` (pure: per `Handle` — the sidebar, the file
      browser, its list/preview split in percent, the agents panel, the
      terminal — the user's preference (`web.<var>`, see below), and the window's
      `Viewport`; `bounds` gives a handle's size, minimum and maximum in
      that window given what is open (`App.Model.shown`): the side panels
      docked beside the chat (the file browser floats over it below
      1100px, the agents panel below 1180px, as in the CSS) share its
      width less `chat_min` (400px), shrinking in proportion to what they
      have above their minimums when they would not fit, while the
      preferences stay; `update` returns the `Save`s, preferences to keep)
      and `Resize_view` (the handles: `role=separator`, focusable, with
      `aria-valuenow/min/max` and `data-resize-*` for the page's drag
      code, a double click resetting; none on a phone; and the sizes as
      CSS variables on `.app`). `Keys.Target.Resize` gives a focused
      handle the arrows along its axis (Shift: ×4), Home, End and Enter.
    - Syntax highlighting: `Highlight` (pure, total: `Lang.of_name`
      for fences, `Lang.of_path` for files; `line` tokenizes one line from
      a `State` carrying block comments, multi-line strings, Markdown
      fences and HTML tags and embedded script/style across lines, with
      a generic scanner configured per language — comments, strings with
      prefixes and raw forms, keyword/type/constant sets, capitalised
      types, keys — plus hooks; loops are self tail calls, so a 256 KiB
      minified line is fine in js_of_ocaml) and `Highlight_view` (spans
      with `hl-*` classes; past 64 KiB the rest is plain text). Hand-written
      rather than a vendored JS highlighter: no bundling or JS interop,
      a few tens of KB in the bundle, testable in expect tests, ~20ms per
      64 KiB. Used by `Markdown_view`'s code blocks (a streaming reply's
      settled parts are cached, so only the open block is highlighted
      again per delta), `Files_view`'s text preview (cached per preview by
      `View_cache`) and `Diff_view` (`?lang`, from the edited path: the old
      and new sides each carry their own state, reset at each hunk).
    - Around it: `Sidebar_view`/`Session_list` (fuzzy search, ages from
      `Rel_time`; active sessions, then a divider with the archived ones'
      count that folds them (`Model.archived_open`, `Session_list.shown`,
      which keyboard navigation follows; a search shows them), muted, and
      "Delete archived"; a session is running while its
      turn or background work runs (`list_sessions`' `running`, and
      `Model.active` for the current one); the list is refetched on
      `sessions_changed`, never polled; each row's archive/unarchive and
      delete buttons; `Session_marks`, the selection mode: clicks and
      Space mark, shift-click and Shift+Space mark the range from the last
      one, and a bar at the list's foot deletes them; the `Delete` dialog (`Dialog.Delete_target`: paths or every
      archived one) lists them with their sizes and warns about those the
      backend will keep, running, open elsewhere or our own, and the
      `Delete_result` reply is a toast; the current session's row shows
      its live cwd and a rename button; at its foot `Session_view.card`,
      then the account and the settings gear), `Session_view` (the
      current session in one place at a time, by `Model.docked`: with the
      sidebar open beside the chat, its `card` in the sidebar —
      `Status_view.extras`, then `Status_view` on a grid with the tool host
      (opening `/host`) and the branch beside the activity, the context
      across, tokens and cost, each in its own cell so that nothing moves
      it — and its `tools`, the panel toggles floating in the chat's top
      right corner; otherwise its `bar`, a slim line over the chat with
      the sidebar's toggle, the title, the extras, the status, the panel
      toggles and the gear — its
      steps are container queries on the bar in `style.css`),
      `Composer_view` (one box: the editor, then the model, thinking and
      approval mode chips, plain text until pointed at, each opening the
      picker its command opens, and attach, stop and send along its
      bottom; the composer's container queries shorten the chips) with `Queue_view` (the queued steers and follow-ups from `Queued`:
      the backend's `queue_update` lists, taken out at once by Edit and
      remove via `remove_queued`, re-fetched with `get_pending` if that
      fails; what left the queue otherwise is acknowledged when its user
      message reaches the transcript, until the run ends), `Completion`
      (slash commands from `Slash`, their arguments, `@` paths via
      `list_paths`, `#` snippets) and `History`, `Dialog`/`Dialog_view`/`Modal`
      (pickers built on `Picker`, help, rename, delete, the login flow
      `Login_flow` — also a custom provider's logout question — tool
      confirmations; while one shows, `View` makes the page behind it
      `inert`), `Command_dialog_view` (`/hotkeys`, `/scoped-models`,
      `Prompt` — a path with the backend's completions whose failures stay
      in the dialog, for `/cd`, `/mkdir`, `/host`, `/export`, `/import` —
      the "Create folder?" confirmation of a missing directory, `/rewind`'s
      confirmation, `/session`, text), `Session_tree` (`get_entries`
      as `/fork`/`/rewind`/`/tree` pickers), `Btw_view`, `Account_view` (the
      sidebar's account button and the account menu, a `Picker` of
      accounts and actions, the settings and the commands and keys) over `Accounts` (the saved sign-ins in
      localStorage, behind a `Storage` record so that tests use a table),
      `Status_view` (the context linking to `/session`, tokens, cost and
      the activity, on a grid in the card and on one line in the bar, where
      the activity has a slot of fixed width; its `extras`, kept out of
      the status so that they never move it: the plan chip, queued
      messages, the transfers in progress from `Files.transfers`…; and
      `host`, where tools run, opening `/host`), `Settings_view` (`Dialog.Settings`, from the
      gear beside the account, the account menu, `/settings` or Ctrl+,:
      the theme and accent (`Theme_view`), notifications, verbosity, `/confirm`,
      providers, the account; its controls are `Action.Run` commands, which
      leave it open),
      `Toast` (at most two, the newest, over the bottom of the chat
      column; a repeat is counted rather than stacked, a notice's lifetime
      grows with its length, errors stay until dismissed, long paths are
      elided in the middle with the whole text as the title; with a dialog
      or a phone's sheet open, the newest one at the bottom of the page,
      under the dialog and outside its `inert` page), `Html.lamp` (the work-in-progress sign everywhere; `View` marks
      the page `offline` while reconnecting, which stills the others),
      `Html.mark` (how something ended or stands: an SVG icon from
      `Html.Icon`, never a text glyph a font may lack, in a box as tall as
      the line it marks, coloured by its class: tool cards, the agents
      panel, plan steps, pickers' current item), and `Keys` (which of the
      dialog, the popup or the editor owns a key; `help`, and `browser`:
      the TUI's keys that the browser keeps).
    - Approvals: the confirmation dialog (`Dialog_view`, answers from
      `Prigh_ui.Confirm_prompt` as buttons, a reason field, the plan of
      `exit_plan_mode` as markdown) and `Question_dialog`/`Question_view`
      (the model's `ask_user`, over `Prigh_ui.Question_form`: radio buttons
      or checkboxes and an own answer, keys and clicks, and the deadline's
      note); both queue
      (`Model.confirms`, `questions`), close on `tool_confirm_done` /
      `ask_user_done` and come back from `get_pending` (`Pending`).
      `/mode`, `/plan`, `/todos`, `/permissions` and Alt+M as in the TUI.
    - Skills and MCP: `Skills` caches `list_skills` for `/skill:`'s
      completion, keyed by the session, its directory and tool host (a
      reply for another place is ignored; switching user empties it), and
      builds `/skills`' picker; `Completion` offers the names right after
      `/skill:`, and `send` sends `/skill:NAME args` as a prompt (or steer
      or follow-up) for the backend to expand. `Prigh_ui.Skill_message` (shared with
      the TUI) parses the expanded user message (`<skill name
      location>…</skill>` then the arguments), which `Chat_view` renders as a folded card above the
      arguments and `Session_tree` labels as typed. `Mcp_servers` turns
      `list_mcp`/`mcp_approve` replies (`Mcp_list`) into `/mcp`'s picker
      (servers to act on first; the problems under it, in `Dialog_view`),
      `/mcp reconnect`'s report and what became of an approved or restarted
      server; a ready one's tools are the `Mcp_tools` dialog.
    - Snippets: `/snippet` runs `Prigh_ui.Snippet_command`; the picker
      (Alt+N, which passes the editor's caret) inserts at the caret, as do
      `/snippet NAME` and `#name` completion (`Completion.insert_snippet`).
      The textarea's value follows `Model.draft`, which puts the browser's
      caret at its end: inserts and accepted completions return
      `Command.Set_caret` (a byte offset, made UTF-16 by `Utf16.units`)
      so the caret lands after what went in.
      `Slash.parse` lets `/snippet add` span lines. Saving sends
      `set_config` with only `snippets`, shown at once; a refused
      `Config_saved` refetches the config so the page shows the backend's.
    - Model hand-over: `/fallback` and `/default-dir` send `set_config`
      with only the field they change (as `/confirm` and `/scoped-models`
      do), and show the config the backend replies with (`/fallback`'s
      names resolved to keys). `Completion`'s `Models` argument completes
      the word under the cursor, leaving out the models already listed;
      `Backend_directory` is `list_dirs` with `host: backend`.
    - Harness notes: `Chat_view` renders `origin`'s notes and sources as
      margin notes (a `details` with the glyph and title, opening to the
      text the model saw) and the user's bubble with only their `body`;
      `Session_tree` labels them like the TUI. `/diff` opens
      `Git_diff_view` (a dialog of `Diff_files`' files, each a fold with
      its counts).
    - The user's preferences: `Pref_sync` (the keys and when they go to the
      backend) and `Theme`. At startup the page restores the browser's copy
      (`Restore_prefs`, applied at once: the theme, sizes, the terminal) and
      `get_prefs` follows `hello` (and every reconnection and return from
      acting as another user); its reply wins, what it lacks being the
      default, and is cached again (`Command.Cache_prefs`). A browser whose
      copy predates the backend's (`legacy`, moved from the old
      localStorage keys) uploads it once, unless the backend has the
      user's already (`web.migrated`; the TUI's `history` stays). Changes
      go to the cache and `set_prefs` (the history is `push_pref`, cached
      only while the backend has not answered), not while acting as
      another user (theirs are not ours: nothing is fetched or sent, ours
      stay in this browser), nor to a backend that refused `get_prefs`
      (`Pref_sync.Remote.Local`). `prefs_changed` applies another
      frontend's at once, except opening or closing the terminal, and the
      history only while not browsing it. `Theme` (`original`, `dark`,
      `light`; the accent a `Preset` or a `Custom` `#rrggbb`, with the ink
      that reads on it by WCAG luminance) and `Model.look`, the theme on
      show: the `/theme` or `/accent` picker's highlighted item (or a
      colour typed into the latter), the settings' colour input being
      dragged (`Preview_accent`), else the user's. `update` emits
      `Command.Apply_theme` whenever it changes, so pickers preview and
      Esc puts the user's back. `Theme_view` draws each theme as a small
      picture of the page (`data-theme` on it takes that theme's
      variables) and the accents as swatches, for the settings' Appearance
      section and the pickers.
    - Notifications: `Page_notify` (pure) tracks `/notify on|off`, the
      browser's permission, whether the tab is hidden and how many things
      happened unseen; events raise `Command.Notify` only while hidden,
      enabled and granted, and `Set_title` keeps the count in the title.
      `/notify on` (or the account menu) emits
      `Request_notification_permission` from the user's action. `app/`'s
      `Page_notifications` does the browser side: the Notification API,
      `visibilitychange` and the favicon with a sodium dot while something
      is unseen; whether they are on is a preference (`web.notify`).
  - `app/` (`prigh_web_app`) — the page. `Ws_transport` (a browser
    WebSocket as a `Transport.t`), `Browser` (localStorage, the query
    string, the clipboard, the visual viewport, and `after_render`: work
    on the patched DOM, run by an `after_display` lifecycle in the frame
    that shows it, so that the editor's height, scrolling and focus never
    show a frame late), `Login` (the active sign-in: `prigh.user`, `prigh.token`) and
    `Terminal_panel` (the `/terminal` URL; whether an event is inside the
    panel). `Web_main.run`: connects a `Ws_transport`
    to `?backend=` or the page's `/ws`, sends `hello` with the active
    account (`Login`) and
    `?session=` (giving up after 10s); when the backend refuses, or the
    page has never been in a session on that backend, it shows the
    sign-in form (with the saved accounts, one click each); when it is
    only out of reach the app starts offline and reconnects (a phone
    bringing back a discarded tab); otherwise it saves the login as an account
    (`Accounts.remember`) and runs `App.update` in a
    `Bonsai.state_machine`, executes the commands (RPCs through the
    `Client`, replies back as `Action.Reply`; `history.replaceState` for
    `?session=`, also kept as the account's last session unless acting as
    another user; the clipboard; switching accounts, which activates one
    and loads the page for its backend and session; adding one, which
    shows the sign-in form on the next load; scrolling; `Auth_failed`
    notes its message in localStorage (`prigh-web.refused`) and reloads
    into the sign-in form, keeping `?session=`), keeps the chat's scroll
    position and the draft in sessionStorage (`Tab_memory`:
    `prigh-web.scroll`, `prigh-web.draft`, for the URL's session) and puts
    them back after a reload (the position as the distance from the end of
    the transcript, saved on scrolling and when the page is hidden or left:
    only the newest page loads at first, and older ones are loaded until
    it is reached, until the user scrolls), listens to `online`/`offline`, and installs
    document listeners: keys (through `Keys`; a Tab no binding takes goes
    round the open dialog), the folds' buttons,
    pasted and dropped image files become attachments (PNG, JPEG, GIF,
    WebP, sent with the prompt as base64; others get a toast), the narrow
    (phone) layout, a clock for ages and toasts, and the chat following new
    output unless scrolled up. `Chat_listeners` copies code blocks and
    closes an open image with Esc; `Agents_listeners` opens a subagent's
    card (`data-agent`) in the agents panel, keeps a
    shown transcript or job output at its end unless scrolled up, and
    performs `Reveal` (opening the folds the card is in).
    `Terminal_widget` is the terminal panel's xterm.js: a
    `Vdom.Node.widget_of_module` around `bin/terminal.js` (loaded with
    the xterm.js, its fit addon and `xterm.css` vendored in `bin/vendor/`
    the first time the panel opens), connected to `Terminal_panel.url` with
    the page's user and token and the target's `as_user`; a new target key
    disposes the old connection (the backend keeps the shell) and mounts a
    new one. `terminal.js` connects with the size, resets on open (the first
    message is the replay), reconnects with backoff, pings, and a key after
    the shell exits starts a new one; it reports its state through
    `onStatus`, which becomes `Action.Terminal_status`, focuses the shell for
    `Focus_terminal` and takes the theme's colours (`setTheme`, read from
    the computed CSS variables, with ANSI colours per theme). The document listeners leave keys (a capture
    listener takes only Ctrl+`, before xterm.js), pastes and drops inside
    the panel to it, and the chat's and agents panel's mutation observers
    ignore xterm.js redrawing. `Files_widget` keeps the browser's `File`s
    by id, `PUT`s one to its upload link with `XMLHttpRequest` (its upload
    progress becomes `Upload_progress`), saves a download through a link
    with `download`, opens the pickers, and sends files dropped on the
    drawer to its directory and other dropped or pasted files that are not
    PNG/JPEG/GIF/WebP images to the attachments (marking where a drag
    would land); transfer links go to the backend's web address, from the
    WebSocket's (`Files.http_base`). `Resize_widget` drags the resize
    handles: on `pointerdown` on a `[data-resize]` it captures the
    pointer, reads the size and bounds from the handle's `aria-value*`,
    and at each animation frame sets the CSS variable on the panel itself
    (`data-resize-target`), so a drag re-renders nothing; `pointerup`
    commits `Resize (Set _)` once and, after that render, removes the
    panel's own variable (Esc while dragging puts it back). It also
    sends the window's size (`Resize (Viewport _)`, again on every
    viewport change). `Pref_cache` is the browser's copy of the user's
    preferences (`prigh-web.prefs`: `{owner, legacy, prefs}`, restored
    only for the backend and user it belongs to) and `prigh-web.theme`,
    the document's theme attributes and variables, which `index.html`'s
    inline script puts back before the first paint (also before signing
    in); `Page_theme` applies `Apply_theme` to `<html>`, that cache and the
    terminal.
  - `bin/` — `main.bc.js` plus `index.html`, `theme.css` (every colour,
    as variables: the original theme on `:root`, `[data-theme=dark]` and
    `[data-theme=light]` overriding them, the accent presets
    `[data-accent=...]` and what derives from the accent by `color-mix`),
    `style.css`, `chat.css`, `agents.css`, `terminal.css`, `files.css`,
    `highlight.css` (which tokens take which colours) and `resize.css`
    (the handles) (other design tokens in `style.css`'s `:root`), the Recursive font cut to two woff2 instances (sans and mono;
    `recursive-OFL.txt`), `terminal.js` and the
    vendored xterm.js, assembled under
    `bin/site/` and installed to `share/prigh_frontend/web` (the Nix
    wrapper exports it as `$PRIGH_WEB_ROOT`). The dev profile links
    separately compiled units with inline source maps (~49 MB, fast to
    relink); release builds (`dune build -p`, so Nix) are whole-program at
    `--opt 3`, ~2 MB (590 KB as the installed `.gz`, 500 KB `.br`).
  - `test/` — inline expect tests under node: a `Harness` drives
    `App.update`, answers RPCs and prints the commands and the rendered
    page (as text, `Html.mark`s print as the glyphs they draw: ✓ ✕ – ■);
    `test_page.ml` covers `app/`'s pure parts (`Login`, the URLs).
    `e2e/` — the Playwright e2e (see Testing).

## Neovim client (`editors/nvim/`)

prigh.nvim is an Agent Client Protocol client in Lua: it spawns `prigh acp`
(the ACP agent side of the backend) and talks JSON-RPC 2.0 over
newline-delimited stdio. Protocol and UI are separate modules so other editor
ports can mirror them:

- `acp.lua` — the transport: spawn (`vim.system`), line framing, request ids
  and pending callbacks, incoming requests answered through a `respond`
  callback, stderr into `log.lua` (`:PrighLog`), exit fails pending requests.
- `agent.lua` — the client: `initialize` (advertises `fs.readTextFile`,
  `fs.writeTextFile`, `elicitation.form`, `session.notices`, boolean config
  options, `_meta.prigh.state`), `session/new|load|list`, the last session
  per directory (`stdpath("data")/prigh/last_sessions.json`), prompts with a
  client-side queue, `session/cancel`, modes and config options, and the
  agent's requests (`fs/*`, `session/request_permission`,
  `elicitation/create`). The UI is reached only through callbacks
  (`permission`, `elicit`, `unqueue`).
- `session.lua` — one session's state, folded from `session/update` (message
  and thought chunks, tool calls by id, plan, commands, mode, config options,
  usage, title, notices) and `_prigh/state`; views subscribe to `entry`,
  `status` and `reset` events. Loading another session resets it.
- `fs.lua` — reads return a loaded buffer's text (unsaved changes included);
  writes to a loaded buffer apply only the changed hunks as one undo step,
  then `:noautocmd write`; each file's text before the agent's first write
  is kept for the review.
- `review.lua` (+ `diff.lua`) — the editor-side hunk review, independent of
  ACP: the buffer holds the new text, old lines are virtual lines, decisions
  are marks, and the rejected hunks are reverted in one undo step at the end;
  buffer edits re-diff against the base and keep decisions by hunk identity.
  The planned `_prigh/edit` (scoped edits proposed by the agent) reuses it
  with `after`.
- `render.lua`, `status.lua` (pure) and `chat.lua`, `pickers.lua`,
  `elicit.lua`, `complete.lua`, `context.lua` — the Neovim UI: the chat
  buffer re-renders only the entry that changed; permission cards take over
  the chat's keys until answered.

Tests (`editors/nvim/test`) run each case in a headless Neovim against
`fake_agent.lua`, which replays a scripted exchange and logs what the client
sent; transcripts are compared with `.expected` files (`PROMOTE=1`
re-records); the flake check `nvim-plugin` runs them.

## Data on disk

- `~/.config/prigh/auth.json` — credentials (pi-compatible).
- `~/.prigh/sessions/<stamp>_<id>.jsonl` — session logs.
- `~/.prigh/sessions/<stamp>_<id>/` — per-session data (`Session.data_dir`),
  deleted with the session and counted in its `bytes`: `subagents.jsonl`
  (the `Subagent_log`: `["Start",{id,call_id,parent,task,model,at,run}]`,
  `["Message",{id,at,turns,tool_calls,message}]`,
  `["End",{id,at,run,turns,tool_calls,result}]`, `["Run",{run}]`).
- `~/.prigh/sessions/exports/` — default `/export` output.
- `~/.prigh/history` — the TUI's prompt history on its machine (one JSON
  string per line, last 500).
- `~/.prigh/prefs.json` — the user's preferences and data kept for the
  frontends (`User_prefs`): prigh-web's `web.*` and the shared `history`.
- `~/.prigh/config.json` — `scoped_models`, `approval_mode`, `permissions`, `notify`, `snippets`, `providers`
  (custom endpoints; their keys are in `auth.json` under their names).
- `~/.prigh/AGENTS.md` — global instructions.

## Testing

Four layers, cheapest first.

1. **Pure expect tests.** Backend (`cd backend && dune build @runtest`)
   drives the loop/agent/RPC with `Faux_provider` and
   providers/HTTP/OAuth with `Fake_http_server` (an in-process Eio HTTP
   server); nothing touches the network. Frontend (`cd frontend && dune build
   @runtest`) covers protocol decoding, the client over an in-memory
   transport, the widgets and the keymap, and `App.update` scenarios that
   print the screen (`Screen.to_plain`) — pickers, login prompts, Esc/Ctrl+C,
   streaming, resize, scrolling, subagents. `test_component` runs the Bonsai
   component under `Bonsai_test.Handle` with a scripted platform.
2. **Rendered-frame snapshots.** `frontend/tui/test/test_term_frames.ml` (same
   `@runtest`) drives the real Bonsai_term driver over an in-memory tty,
   decodes Notty's output with the VT emulator in `frontend/tui/test/vt.ml`, and
   compares the grid and cursor with `Screen.to_plain`, so
   `View_of_content`/`Key_of_event` cannot diverge from the pure renderer.
3. **Protocol e2e.** `frontend/tui/e2e` (in `@runtest`, also `@e2e`) runs the real
   `main.exe serve -faux` with an isolated `-auth-file` and `HOME` through
   the real client and diffs a normalised transcript.
4. **Real terminal.** The cram test `frontend/tui/tmux-test/run.t` (in `@runtest`;
   skipped when `tmux` is absent) runs each scenario of `run.t/harness.sh`:
   the built TUI inside a detached tmux session, keys sent with `tmux
   send-keys`, panes captured and normalised (spinners, paths) into the
   cram output, so `dune promote` re-records. Scenarios: startup, prompt,
   ctrl_o, resize, quit, tools, suspend, editor, confirm, approvals (always
   allow, deny with a reason, Alt+M), plan (plan mode and approving the
   plan), question (`ask_user`), paste, reconnect (the backend is killed;
   the TUI respawns it and rejoins the session), fallback, connect_signin
   (from a spawned backend to `serve -listen -token`: refused, signed in
   through the dialog, saved, signed out, back to a local backend) and
   startup_signin (started against a server that refuses: the dialog) and
   detached (`-detached` through a `PRIGH_TMUX` wrapper for the private
   server, which was started without the caller's environment: the
   printed line, the TUI and its environment in the new session, the
   `-2` name, the pane kept open on failure, the errors before detaching).
   `frontend/tui/test/test_backend_link.ml` drives `Backend_link` against in-memory
   backends (the transports each switch opens, the `hello`s, the saved
   file, the tool host).

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

`backend/test/test_seen_files.ml` covers the overwrite guard: `write`
over unseen, changed and touched files, other sessions and unscoped calls,
`edit`'s and `apply_patch`'s notes, `*** Add File:` and `*** Move to:`
over unseen files, the bounded record, a session whose subagent writes what
the parent read, and a real `prigh tool-host` restarted in between (it has
forgotten). `test_job_input.ml` covers `Process.run ~input`, `job_input`
driving a local job, the `tool_exec`/`tool_exec_input` protocol with a host
that announced `exec_input` and with one that did not, and the stdio
worker's `input` lines. `test_web_fetch.ml` covers `Html_text` and
`web_fetch` against `Fake_http_server` (HTML, text, JSON, Latin-1, sniffed
HTML, images, binary types, redirects and loops, 404/500, bad URLs,
timeouts, refused connections, truncation and the size limit);
`test_web_search.ml` the providers' server tool streams, replay to the
same and other providers, which models get `web_search`,
and a search saved in a session and streamed to clients.

`backend/test/test_approval_rules.ml` covers rule parsing, command
splitting, globs, which calls rules cover and touch, suggestions and
`Approval.decide` across the modes; `test_approvals.ml` drives agents with
`Faux_provider` through the modes, always-allow rules surviving a reload,
deny with a reason, config rules, subagents' confirmations, plan mode (edits
refused, notes, `exit_plan_mode` approved or not, switched on mid-run),
`update_plan` and `ask_user` (answers, first answer wins, abort); and
`test_approvals_rpc.ml` the same over the RPC with two clients
(`set_mode`, `get_pending`, `tool_confirm_done`, `ask_user_respond`). The
TUI's `test_app_approvals.ml`, prigh-web's `test_approvals.ml`, the e2e's
approvals block and the tmux scenarios `approvals`, `plan` and `question`
cover the frontends.

`backend/test/test_connection.ml` covers peers that do not keep up: a
connection that never reads while its session streams thousands of events
(the run finishes, everyone is served, a rename and prompts in other
sessions too, and the connection ends once 10000 messages behind), a
failed write ending its connection, a closed connection's running
request cancelled (stdio's finishing), a slow request not holding up the
others, a newer connection from the same page taking over, two pages
opening a saved session at once getting one agent, live sessions listed
from memory, the outbox, and `Websocket.keepalive` over a socket pair (a
peer answering pings stays, a silent one and a write stuck on a peer that
does not read are shut down).

`backend/test/test_host_reconnect.ml` covers tool hosts going away: a
session stays on its disconnected host (also when another host attaches),
tool calls say it waits for the host, the host reconnecting with the same
`host_id` resumes it in the same cwd without an environment note, only
`set_active_host` moves it, an evicted session resumes on its returning
host, and a newer connection taking over a held host id. The real
`prigh tool-host` reconnecting with its id is in `test_tool_host.ml`,
terminals routed by host id in `test_terminal_relay.ml`.

`backend/test/test_cli.ml` runs the built `prigh` binary in a sandboxed
`HOME` and prints what the user sees for wrong input: bad flag values,
running without a login, `-session` by id, broken `config.json` and
`auth.json`, a taken `-listen`/`-web` port and a tool host whose token the
backend refuses. Each error names the flag or file and what to do next.

The web layer adds three: `backend/test/test_static_site.ml` (validators and
`304`, precompressed siblings by `Accept-Encoding` and age, `HEAD`),
`backend/test/test_web.ml` (frames, fragmentation,
the RFC handshake vector, static serving and traversal, and a masked WebSocket
RPC conversation over a real loopback socket)
and `frontend/web/test/` (below; run under `node` with js_of_ocaml
because `virtual_dom`'s initialisers need a JavaScript runtime; skipped
without `node`). The flake's `web-wrapper` check runs the packaged wrapper's
`prigh -web` against a fake backend (the `serve` arguments, the site it
points at) and checks the installed site: its files, the release bundle's
size and the precompressed siblings.

prigh-web's e2e, `frontend/web/e2e/prigh_web.sh` (the flake's Linux
`web-e2e` check, against the packaged backend and site), runs
`prigh_web.mjs` in Chromium and Firefox. The driver owns the backend
(`serve -web 127.0.0.1:0 -faux-script ...` with an isolated `HOME`
and cwd) so that it can restart it, and prints normalised text snapshots
(ids, cwd, cost, context and messages' times replaced) diffed against `prigh_web.expected`:
the sign-in form and signing in with the token, the session's card, a scripted
bash call and its output, a long output folded, unfolded and folded again,
the model `read`ing a real PNG (`png.mjs` writes
it; the tool result's `img` must be a loaded `data:image/png` of the right
natural size), an image pasted and one dropped into the composer (shown,
sent, cleared, rendered in the prompt, and present with the right sizes in
the backend's copy of the message via a second WebSocket's
`get_messages`), a reload rejoining the session from `?session=`, a new
session and switching back from the sidebar (with the backend's messages
held back by a wrapped `WebSocket`, so that the prompt being sent and the
session loading show), the backend dying (the
banner) and restarting on the same port (the page reconnects to the same
session and runs another prompt), and the agents panel (a background
subagent running a synchronous one and a background job: the list, the
nested one in full via Alt+2 and its card revealed in the chat, the job's
output, a card in the chat opening its agent, its footer staying at the
panel's foot as a short window's transcript scrolls), and the terminal panel (a
real shell through tmux, on a private tmux server via `TMUX_TMPDIR` with
`SHELL=/bin/sh` and a fixed prompt: typed output, Esc reaching `cat -v`,
`stty size` matching xterm.js's rows before and after dragging the panel
taller, Ctrl+` closing it with the focus back in the editor, the replay
after reopening and after a reload, another session's shell and back, the
phone sheet), and the file browser (Ctrl+Shift+F at the session's cwd, a
text and an image preview, uploads through `setInputFiles` and replacing
one after its dialog, a file and a directory downloaded through
Playwright's `download` event (the `.tar.gz` listed with `tar`), a typed
path, new folder, rename with F2, delete after its dialog, then a file
attached from the browser, one dropped on the composer and one picked with
the paperclip, sent with a prompt and checked in what the model saw, and
the phone sheet), the panels' resize handles, and the session's card and
the composer with the chat column 700, 520 and 400px wide, then the bar
over the chat without the sidebar at 900, 600 and 400px (by bounding boxes:
nothing overlaps, sticks out of its parent or is cut off without an
ellipsis; the controls' menu under its button, closed by a choice and by
Esc), with no console or page errors. Then `accounts.mjs` runs the account switcher against `serve -tokens
alice=a,bob=b -superusers alice`: alice signs in, adds bob (each sees only
their own sessions), switches back in one click, acts as bob and comes
back, and signs bob out, leaving alice on the sign-in page. Then
`reconnect.mjs`, at phone size: a long session scrolled halfway up with a
draft and a pasted image; `context.setOffline` and back, then the backend
killed and restarted: each time the same session, the same scroll
position (within 4px), the draft and image kept, the transcript's DOM
untouched (a marker attribute survives), no page load and no sign-in
page; a reload returns to the same session, scroll position and draft,
also when the backend is out of reach as the page loads (a wrapped
`WebSocket`), until an `online` event. Then `paging.mjs`: a 40-turn
session written into the backend's `HOME` opens at its newest page (how
many earlier messages, at the end); scrolling to the top loads the pages
before, the message that showed staying in place (within 4px), up to the
first message; a 2000-line output shows cut and loads whole when unfolded;
a reload scrolled up into an older page comes back to the same place; in
a fresh tab the rail has a mark per message (those not loaded faint), a
mark's tooltip names its message, a click goes there (marked, and marked
in view on the rail), one not loaded loads the pages down to it, Ctrl+J's
list filters and goes there, and on a phone only its button shows. Then
`resilience.mjs`: a 400-turn session (a large outline) behind a wrapped
`WebSocket` that delivers messages at 8 kB/s while the page is shown again
every 2.5s (one reconnect, after which the session comes back; the old page
dropped the working connection again and again), then closes each socket
as the session's messages arrive (the page gives up after six, with the
banner, never saying "Reconnected", and opens no more sockets; the old page
opened 900 in 30s), then "Retry now" once the connection holds. Then
`stability.mjs`: nothing redrawn or shifted that did not change while the
page idles and a reply streams, the editor's height in every frame, and the
session's status in the same place in every frame as turns start, run a
tool, queue a steer and end, in the sidebar's card on a desktop and in the
bar on a phone.
`SHOTS=DIR` saves a screenshot per step, `UPDATE=1` re-records,
`ENGINES=chromium` runs one browser.
