# prigh architecture

prigh is an agentic coding harness split into an OCaml backend (`backend/`,
all the logic) and a TypeScript terminal frontend (`frontend/`, rendering and
input only). There is no plugin system: tools, subagents, providers and slash
commands are compiled in.

```
 terminal ── frontend (node, TUI) ── JSON lines on stdio ── backend `prigh serve` (OCaml, Eio)
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
  by lines and bytes.
- `Tool_bash` (streamed output, timeout, cancellation), `Tool_read`,
  `Tool_write`, `Tool_edit` (multi-edit, unique non-overlapping matches,
  atomic), `Tool_ls`, `Tool_grep`/`Tool_find` (via `rg`). `Tools.all` is the
  fixed set.
- `Tool_subagent` — a tool that runs a nested `Agent_loop` in-process with a
  role-restricted tool set (`explore` read-only, `worker` full) and a turn
  budget, streaming its progress as tool output and returning its final
  reply. It shares the parent's provider and current model/thinking.

### Harness

- `System_prompt` — built-in guidance plus environment facts plus
  `AGENTS.md`/`CLAUDE.md` files from `/` down to the cwd and
  `~/.prigh/AGENTS.md`.
- `Agent_loop` — the core loop. Per turn: build the request from the context,
  stream the assistant message (emitting `Message_start/update/end`), retry
  with exponential backoff on retryable errors, execute each tool call
  sequentially (`Tool_start/output/end`), append results, then poll `steer`
  for user messages to inject before the next turn. Stops on `End_turn`
  without tool calls, `Length`, `Error`, `Aborted`, `max_turns` or
  cancellation. Every tool call always gets a result (a `[cancelled]` one if
  needed) so the context stays valid.
- `Agent` — one conversation: owns the session, model and thinking level,
  the run lifecycle (`prompt`, `steer` = after the current turn,
  `follow_up` = after the loop ends, `abort`), automatic compaction at 80% of
  the context window, and a subscriber list receiving `Agent.Event.t`
  (`Loop of Agent_event.t | State_changed | Compacted | Notice`).
- `Session` — an append-only JSONL log forming a tree: every entry has a
  `parent`, the active conversation is the path from the root to `head`.
  Rewinding moves `head`; forking copies the active path to a new file.
  Entries are messages, model/thinking changes and compaction summaries;
  `Session.messages` is the message list for the next request with the
  compaction summary replacing everything before `kept_from`.
- `Compaction` — summarises older messages via the model and keeps a tail;
  manual (`/compact`) or automatic.

### RPC

- `Rpc_json` — the wire encoding (plain tagged objects, not the derived
  `["Ctor", ...]` form) for messages, deltas, state, models, sessions,
  auth status and events.
- `Rpc_server` — reads request lines, dispatches to `Agent` and
  `Login_manager`, writes responses and events through a single outbox
  fiber. Methods: `ping`, `prompt`, `steer`, `follow_up`, `abort`,
  `get_state`, `get_messages`, `get_entries`, `set_model`, `set_thinking`,
  `list_models`, `compact`, `new_session`, `switch_session`,
  `list_sessions`, `fork`, `rewind`, `auth_status`, `login`,
  `auth_respond`, `auth_cancel`, `logout`.

### CLI (`backend/bin/main.ml`)

`serve` (RPC), `run <prompt>` (headless, streams to stdout), `sessions`,
`login <provider> [-method]`, `logout <provider>`, `auth`. All commands share
`-auth-file`; `run`/`serve` share `-model`, `-thinking`, `-session`, `-cwd`,
`-no-tools`, `-faux`. With no explicit model or session, the default model is
the first logged-in provider's in the order anthropic, openai-codex, openai,
deepseek.

## Frontend (`frontend/src`)

- `protocol.ts` — TypeScript mirrors of `Rpc_json` with hand-written runtime
  guards for everything that arrives from the backend.
- `client.ts` — spawns the backend, frames JSON lines, correlates
  responses by id, and fans out events to subscribers. Typed wrappers for
  the common methods.
- `tui/` — a small custom TUI. The transcript lives in terminal scrollback
  and only receives complete lines; the bottom panel (streaming tail, editor,
  status line) is erased and redrawn on every change (`app.ts`). `editor.ts`
  is a multi-line editor with history, `keys.ts` parses keypresses and
  bracketed paste, `render.ts` renders transcript items, `commands.ts` holds
  the slash-command table and completion, `markdown.ts` renders markdown to
  ANSI.
- Login inside the TUI: `/login <provider> [method]` calls `login`; `auth`
  events print the URL (and open the browser), switch the editor into a
  prompt mode (masked input for secrets, numbered options for selects, Esc
  cancels), and on `done` the model is switched to that provider.

## Data on disk

- `~/.config/prigh/auth.json` — credentials (pi-compatible).
- `~/.prigh/sessions/<stamp>_<id>.jsonl` — session logs.
- `~/.prigh/AGENTS.md` — global instructions.

## Testing

Backend tests are ppx_expect tests under `backend/test`, driven by
`Faux_provider` for the loop/agent/RPC and by `Fake_http_server` (an
in-process Eio HTTP server) for providers, HTTP, and the OAuth token and
callback flows; nothing touches the network. Frontend tests use `node:test`
against the protocol guards, framing, editor, renderer and commands.
