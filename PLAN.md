# prigh: plan

Clean-room agentic coding harness. OCaml backend (`backend/`), TypeScript
frontend (`frontend/`). No plugin/extension system: tools, subagents and
commands are compiled into the backend.

## Layout

```
backend/            OCaml (dune, core, ppx_jane); library `prigh`, exe `prigh`
frontend/           TypeScript (node 22); TUI client, later web client
PLAN.md
```

## Process model

The frontend spawns `prigh serve` and talks JSON-lines over stdin/stdout
(one message per line, `\n`-terminated). This avoids needing an HTTP/WebSocket
server in OCaml (the switch has no such libraries). A web frontend later
bridges WebSocket <-> stdio in node.

```
frontend (TS)  <-- JSON lines over stdio -->  prigh serve (OCaml)
                                                  |
                                                  +-- cohttp-eio + tls --> api.deepseek.com (SSE)
                                                  +-- tools (bash/read/write/edit/grep/find/ls)
                                                  +-- subagents (nested Agent_loop, in-process)
```

Concurrency: Eio (effects, direct style). The agent loop runs in a fiber; the
RPC reader fiber handles `abort`/`steer` by resolving a `Cancellation` token,
which cancels the in-flight HTTP request or tool subprocess via
`Cancellation.protect` (`Fiber.first`). HTTP is cohttp-eio with ocaml-tls.
Tool execution is sequential per assistant turn (parallel via `Fiber.List`
later).

## Backend modules (`backend/lib`)

Foundations (no deps available in the switch, so written in-house):

- `Json` = `Jsonaf` (installed), with `ppx_jsonaf_conv` for wire types.
- `Cancellation` — token with `cancel`/`on_cancel`/`child`. (done)
- `Process` — `Eio.Process` subprocess with streamed stdout/stderr, stdin,
  cwd, extra env, timeout and cancellation (SIGKILL). (done)
- `Sse` — incremental `text/event-stream` parser. (done)
- `Http_client` — `post`/`post_stream` on cohttp-eio with tls-eio/ca-certs.
  Tested against an in-test Eio HTTP server; TLS verified live (401 from
  DeepSeek with a fake key). (done)

Model layer:

- `Content` — text / thinking / tool_call / tool_result blocks, image later.
- `Message` — `User | Assistant | Tool_result`, with usage and stop reason
  (`End_turn | Tool_use | Length | Error | Aborted`).
- `Model` — id, provider, context window, max output, cost, reasoning support.
  DeepSeek table hardcoded (deepseek-chat, deepseek-reasoner).
- `Provider_deepseek` — build OpenAI-compatible chat/completions request
  (messages, tools, stream=true), parse streamed deltas (content,
  reasoning_content, tool_calls with index-based argument accumulation, usage).
  Emits `Assistant_event` (Start / Text_delta / Thinking_delta /
  Tool_call_start / Tool_call_delta / Tool_call_end / Done / Error).
  Tested against recorded SSE fixtures, no network.
- `Auth` — `DEEPSEEK_API_KEY` from env or `{"deepseek": "<key>"}` in
  `~/.config/prigh/auth.json`. (done)

Tools (`backend/lib/tools/`):

- `Tool` — `{ spec : Tool_spec.t; run : Context.t -> Json.t -> Result.t }`;
  `Tool.execute` maps `Tool_args.Invalid`/exceptions to error results.
  `Tool_args` gives typed accessors and builds the JSON schema.
  `Truncate` bounds output by lines and bytes (head or tail). (done)
- `Tool_bash` — `bash -c` with cwd, default 10 min timeout, interleaved
  stdout/stderr streamed via `Context.on_output`, tail truncation. (done)
- `Tool_read` (offset/limit, binary detection), `Tool_write` (mkdir -p),
  `Tool_edit` (multi-edit, each old_text unique, non-overlapping, atomic),
  `Tool_ls`, `Tool_grep` and `Tool_find` (both via `rg`, gitignore-aware,
  sorted). (done)
- `Tools` — the fixed tool set; `Tool_subagent` comes later.

Harness:

- `Context` — system prompt + messages; conversion to provider wire messages.
- `Agent_loop` — the core loop: send context, stream assistant message, on
  `Tool_use` execute tool calls (validate args, `before`/`after` hooks
  internal to the loop, block/terminate semantics), append results, repeat
  until `End_turn`, abort, error, or max-turns. Queued user messages are
  injected at turn boundaries (steer = after current turn; follow-up = after
  loop ends). Emits `Agent_event` (agent_start/end, turn_start/end,
  message_start/update/end, tool_execution_start/update/end).
  Tested with a `Faux_provider` that scripts assistant responses.
- `Session` — persistent JSONL log in `~/.prigh/sessions/<cwd-hash>/<id>.jsonl`.
  Entries: header, message, model change, compaction summary, branch points
  (`parent_id` per entry, so trees are supported from day one). `Session.load`
  rebuilds the active path.
- `Compaction` — when context tokens exceed threshold, summarise older
  messages via the model and keep a tail. Manual `/compact` too.
- `System_prompt` — built-in prompt + `AGENTS.md`/`CLAUDE.md` discovery from
  cwd upward, plus `~/.prigh/AGENTS.md`.
- `Retry` — exponential backoff on 429/5xx/network for provider calls.

Subagents:

- `Subagent` — a tool whose run spawns a nested `Agent_loop` in the same
  process with its own `Context`, a restricted tool set, own session file,
  and a turn/token budget. Reports back the final assistant text. Events are
  forwarded to the client tagged with the subagent id so the UI can render
  nested progress. Pre-defined roles (e.g. `explore` read-only,
  `worker` full tools) live in `Subagent_roles`.

RPC (`backend/lib/rpc/`):

- `Rpc_protocol` — JSON message types. Client -> server: `prompt`, `steer`,
  `follow_up`, `abort`, `get_state`, `get_messages`, `set_model`,
  `set_thinking`, `compact`, `new_session`, `switch_session`,
  `list_sessions`, `fork`, `set_cwd`, `command` (slash commands resolved
  server-side). Server -> client: `response {id, ok, result|error}` and
  `event` (every `Agent_event`, plus `state_changed`, `session_changed`).
- `Rpc_server` — reads stdin lines in the reactor, dispatches, writes stdout.
  Requests are fenced by `session_id` to avoid stale responses.
- `Rpc_codec` expect tests for every message, and an end-to-end test driving
  `Rpc_server` with the faux provider over in-memory pipes.

CLI (`backend/bin`): `prigh serve` (RPC mode), `prigh run "<prompt>"`
(headless, prints final answer, useful for tests and scripts), `prigh sessions`.

## Frontend (`frontend/`)

TypeScript, node 22, strict, no `any`, erasable syntax only. Minimal deps.

- `src/protocol.ts` — types mirroring `Rpc_protocol`, with runtime validation
  of incoming JSON (hand-written guards, no schema library initially).
- `src/client.ts` — spawns backend, JSON-lines framing, request/response
  correlation, event subscription, reconnect/re-sync via `get_state`.
- `src/tui/` — own small TUI: differential line renderer over raw stdout,
  keypress parser, components (editor with history + multi-line, message
  list, tool call blocks collapsible, thinking blocks, status/footer with
  model + token usage, streaming spinner). Slash-command autocomplete fed by
  server `command` list. Keybindings in one defaults table.
- `src/markdown.ts` — small markdown-to-ANSI renderer (headings, code
  fences, lists, inline code/bold).
- `test/` — `node:test`; client framing tests against a fake backend script;
  renderer snapshot tests.

Later: web UI (Preact) reusing `client.ts` through a node WS bridge.

## Milestones

1. Backend foundations: `Cancellation`, `Process`, `Sse`, `Http`. (done)
2. `Message`/`Content`/`Model`, DeepSeek provider with fixture tests,
   `prigh run` doing a single non-tool completion end-to-end. (done; not yet
   verified against the live API - no key on this machine)
3. Tools (`bash`, `read`, `write`, `edit`, `grep`, `find`, `ls`) with expect
   tests in temp dirs. (done)
4. `Agent_loop` + faux provider tests (tool round-trips, abort, steer,
   max turns, error stop reasons, retries). (done)
5. `Session` JSONL persistence, resume, fork, rewind; `System_prompt`
   discovery. (done)
6. `Rpc_json`/`Rpc_server`; `prigh serve`; `prigh run` uses the full
   `Agent`. (done)
7. Frontend client + TUI (editor, history, slash commands, streaming,
   tool blocks, status line). (done)
8. `Compaction` (manual + automatic at 80% of the context window), retries
   with backoff on 429/5xx/network, usage/cost in state, `/model`,
   `/thinking`. (done)
9. Subagents: `Tool_subagent` with `explore` (read-only) and `worker`
   roles, 40-turn budget, progress streamed as tool output. (done)
10. Not done: `/export`, images in `read`, parallel tool execution, web
    bridge. Live DeepSeek verification only covers auth (401) since no key
    is available on this machine.

## Open decisions

- JSON schema for tool parameters: hand-written `Json.t` literals vs. a small
  typed schema DSL in OCaml that also prints the schema. Prefer the DSL once
  there are >3 tools.
- Whether `steer` interrupts mid-tool-batch (pi: after current tool) or only
  after the turn. Start with after-turn; revisit.
