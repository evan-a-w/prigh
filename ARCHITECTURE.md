# prigh architecture

How prigh fits together, and where each piece lives. For using prigh see
`README.md`; for the container, `DOCKER.md`; for builds, tests and
conventions (`./test`; `./test --full` only when the user asks),
`AGENTS.md` (and `backend/AGENTS.md`). The `.mli` files are the
detailed reference: this file says which one to open.

prigh is an OCaml backend (`backend/`, all the logic) and OCaml frontends
(`frontend/`, Bonsai: rendering, input and UI state only), all built with
OxCaml 5.2, plus a Rust desktop shell (`desktop/`). There is no plugin
system: tools, providers and slash commands are compiled in; what comes from
outside is skills (`SKILL.md` files) and the tools of MCP servers.

```
 terminal ── prigh-tui (Bonsai_term) ─── JSON lines, stdio or TCP ──┐
 browser ─── prigh-web (Bonsai_web) ──── JSON lines on a WebSocket ─┤   backend: `prigh serve` (Eio)
 desktop ─── prigh-desktop (Tauri) ── shows prigh-web ──────────────┤    ├─ Rpc_router → Rpc_server (per namespace)
 editor ──── `prigh acp` (ACP on stdio) ── prigh's RPC ─────────────┘    │    clients ↔ sessions, tool hosts
                                                                         ├─ Agent (one per session) ── Agent_loop
 `prigh tool-host` (a machine's tools) ── RPC as a tool-host client ──►  │    ├─ Provider_router ── Anthropic / OpenAI / DeepSeek / custom (HTTPS, SSE)
                                                                         │    ├─ Tools ── in process, or `tool_exec` to a tool host
                                                                         │    └─ Session (JSONL tree, ~/.prigh/sessions/)
                                                                         ├─ Login_manager, Auth_store (~/.config/prigh/auth.json)
                                                                         └─ Forges (GitHub / Forgejo REST)
```

## Repository layout

| Directory | What |
|---|---|
| `backend/bin/main.ml` | the `prigh` CLI: every subcommand |
| `backend/lib/` | the `prigh` library, grouped in subdirectories (below) |
| `frontend/protocol/` | the frontends' copy of the wire types (`prigh_protocol`) |
| `frontend/client/` | the RPC client and transports (`prigh_client`, `client/unix/`) |
| `frontend/ui/` | platform-agnostic UI logic (`prigh_ui`): the TUI's app, shared pieces |
| `frontend/tui/` | the terminal frontend (`prigh-tui`) |
| `frontend/web/` | prigh-web, the browser frontend |
| `desktop/` | Rust workspace: `core/`, `app/` (Tauri shell), `computer/` (`prigh-computer`) |
| `docker/` | the image's entrypoint and build step (see `DOCKER.md`) |
| `nix/` | flake pins, patches, desktop packaging, Home Manager module (`nix/README.md`) |

`backend/lib` is one library with `(include_subdirs unqualified)`: a
module's name does not carry its directory. `backend/AGENTS.md` lists its
groups (`agent/`, `engine/`, `composer/`, `session/`, `providers/`,
`tools/`, `host/`, `rpc/`, ...) and what each holds.

## Processes

| Process | Command | Role |
|---|---|---|
| backend | `prigh serve` | sessions, agents, providers; RPC on stdio, `-listen HOST:PORT` (TCP) and/or `-web HOST:PORT` (prigh-web, WebSocket and TCP clients on one port) |
| tool host | `prigh tool-host` | runs tools on its machine: spawned by the TUI, or standalone with `-connect` |
| TUI | `prigh-tui` | spawns `prigh serve` (stdio) or connects to a remote one, plus a local tool host |
| local web | `prigh web` | serves prigh-web locally for a remote backend, relays `/ws`, runs this machine's tool host |
| desktop sidecar | `prigh desktop` | what the desktop app starts: a connect form, then `prigh web`'s site |
| editor adapter | `prigh acp` | ACP on stdio; an in-process backend, or `-connect` to a running one |
| headless | `prigh run`, `sessions`, `login`, `logout`, `auth`, `trace` | the library without the RPC layer |

One backend serves many sessions and many clients. Every session is an
`Agent`, loaded on demand and dropped from memory when idle with no clients
(the JSONL file is the durable state). Every client is attached to exactly
one session at a time; several clients can attach to the same session and
see the same event stream. A session keeps running when its clients leave.

### Concurrency

The backend is direct-style Eio. I/O functions take `~env`
(`Eio_unix.Stdenv.base`, `Env.t` in `Import`); long-running work runs in
fibers on a switch (the RPC reader, each agent run, background tasks, a
login flow). Cancellation is explicit: a `Cancellation.t` is a promise and
`Cancellation.protect` races work against it, so `abort` stops a model
stream or a `bash` command at once. `Guarded` forks fibers on the shared
long-lived switch that log failures instead of taking the backend down.

## RPC protocol

One JSON object per line (one per text message on a WebSocket):

- request: `{id, method, params}`
- response: `{type: "response", id, ok, result | error}`
- event: `{type: "event", event, ...}` — streaming deltas, tool output,
  state changes, login prompts, notices.

The client sends `hello` first: its name and kind (`web`, `tui`, `acp`),
cwd, whether it runs tools (`tools: true`, its host id and home, `features`),
an optional session, and credentials (`-token`; with `-tokens`, also the
user). The reply carries `client_id`, `host_id` and, with users, the
namespace and signed-in user.

| Module | Role |
|---|---|
| `Rpc_json` (`rpc/`) | the wire encoding of messages, deltas, state, events (plain tagged objects) |
| `Rpc_server` | connections and sessions: live agents by session id, clients, every method's dispatch |
| `Outbox` | a per-connection queue written by one fiber, so senders never wait for a slow peer |
| `Update_coalescer` | merges pending `message_update`s of one stream for a slow reader |
| `Stream_sync` | per connection: send bare deltas when the client has every earlier piece, else a resync |
| `Rpc_router`, `Namespace` | `-tokens`: one `Rpc_server` per namespace (own home under `~/.prigh/namespaces/<name>`) |
| `User_access` | logins, `-host-tokens`, `-superusers`, `as_user`/`set_user` |
| `frontend/protocol/` | the frontends' decoders for `Rpc_json` and the `Request` encoder |

Streamed messages have a process-unique `stream` id and pieces with a `seq`;
frontends apply them with `Stream_update` (`frontend/protocol/`). Every new RPC field or method must
be mirrored in `frontend/protocol/`.

The methods are grouped in `Rpc_server` and a few helpers: session
lifecycle (`new_session`, `switch_session`, `fork`, `clone`, `import`,
`export`, `rewind`, `trim`), running (`prompt`, `steer`, `follow_up`,
`abort`, `shell`), state and config (`get_state`, `set_model`, `set_cwd`,
`set_active_host`, `get_config`, `set_config`), transcripts
(`get_messages_page`, `get_outline`, `search_sessions`), files
(`Files_rpc`: `fs_*`), git (`Git_rpc`), review (`Review_rpc`), pull
requests (`pr_*`), MCP, auth, and the Nix store (`Nix_store_rpc`). `Rpc_server.mli`
documents them.

## Tool hosts

Each session has an *active tool host*: where its `on_host` tools (`bash`,
`read`, `write`, `edit`, `apply_patch`, `ls`, `grep`, `find`, ...) and `!cmd`
shells run. It is the backend itself or a connected client that said
`tools: true` (a TUI's worker, `prigh tool-host -connect`, `prigh web`'s,
`prigh desktop`'s, `prigh acp`'s). The cwd is per host: switching hosts
switches the cwd, and `~` means the host's home.

- Identity: a host is known by its *host id* (`Host_id`: `~/.prigh/host-id`
  on its machine, `-host-id` to override), so a reconnecting or restarted
  host is the same host. A newer connection claiming a held id takes it
  over.
- Dispatch: when the active host is a client, `Agent.host_exec` sends it a
  `tool_exec` event; it streams `tool_exec_output` and answers
  `tool_exec_result`; aborts send `tool_exec_cancel`, background jobs'
  stdin goes as `tool_exec_input`.
- Host side: `Tool_host` (`host/`) is the worker loop; `Host_ops.execute`
  runs the tools (the same code the backend uses for itself) plus
  `$`-pseudo-tools: `$resolve_dir`, `$make_dir`, `$read_file`,
  `$list_paths`, `$list_dirs`, `$instructions` (AGENTS.md/CLAUDE.md and
  skills from the host), `$skill`, `$mcp_*`, `$git_diff`, `$git_forge`,
  `$git_actions`, `$fs_*`. Older hosts answer "unknown host tool".
- Long-lived programs: `$process` (`Host_process`) runs a line-oriented
  program (an agent CLI on stdio) for as long as its exec lasts, over the
  same `tool_exec` channel, so it needs nothing new in the protocol. Its
  chunks are JSON frames, one per line: up `started`, `out` (numbered
  stdout lines), rate-capped `err`/`err_dropped`; down `in`, `ack`,
  `close`, `signal`. Stdout is read only while fewer than the window's lines
  are unacknowledged, so a slow backend blocks the program on its pipe.
  Cancelling closes its stdin, sends SIGTERM and kills the tree after
  `term_grace`; the result says how it ended, with its stderr's end. With
  `mcp`, the host relays as `mcp`/`mcp_in` frames (outside the window) each
  connection to a Unix socket in a private directory that first presents
  the token reported in `started`: a `prigh mcp-bridge` the program runs as
  an MCP server, given socket and token only in its MCP config (`{{prigh}}`
  and `{{mcp_socket}}` in the arguments name them). With `secret`, the program
  gets it as one line on a socket at fd 3 (named by a variable the caller
  chose), never in its environment; `started` says so. An older host
  ignores it: `Host_transport` restarts the program with it in the
  environment and a notice. `Host_stream` is the
  backend's end: lines in and out (at most 12 MiB each), `stop`,
  `interrupt` and an `ended` saying why (exited, not installed or only on a
  login shell's `PATH`, an older host, the host gone). `$probe_programs`
  (`Host_probe`) checks without side effects which programs a host has,
  their `--version` and status command, and whether given files exist and
  variables are set, never their contents.
- Choice: a new session starts on a host chosen from the client's context
  (`Rpc_server.start_location`): the client itself if it is a tool host
  (or its `hello`'s `prefer_host`), else where its current session runs.
  The session file records every cwd/host change, so a reloaded session
  resumes on the same host. `/host` (`set_active_host`) pins a host.
- Disconnects: a dropped host keeps its in-flight calls for a grace period
  and resumes them on reconnect (`running_execs` in `hello`); heartbeats
  (`Backend_socket.heartbeat`) detect silent hosts. Model tool calls wait for
  a returning host, then fail with what to do.
- `-no-backend-host`: the backend runs no tools itself; sessions adopt the
  first host that connects, and nothing a client sends may touch the
  backend's files (`Rpc_server.session_file`).
- `Tool_on_host` (`on_host` tool): one call on another connected host
  without moving the session.

## Agent and the loop

| Module | Role |
|---|---|
| `Agent` (`agent/`) | one session: model, thinking, config, tool hosts, queues, run lifecycle, background tasks, subscribers (`Agent.Event.t`) |
| `Agent_loop` | the core loop: request → stream → tool calls → results → next turn |
| `Agent_event` | the loop's events, including recursive `Subagent*` events |
| `System_prompt` | built-in guidance, environment, AGENTS.md/CLAUDE.md, skills; built once per session and recorded |
| `Told_environment` | what the model was last told (host, cwd, tools, modes, client); changes become notes on the next user message |
| `Injected` | what the harness put into a user message (notes, attachments, pastes, synthetic messages), for display |
| `Steer_queue` | steer (after the current turn) and follow-up (after the run) queues |
| `Tool_pairing` | every tool call answered right after its message (repairs interrupted runs) |

Per turn `Agent_loop` builds the request, streams the reply (`Message_*`
events), retries transient errors (408/429/5xx, overloaded, lost or stalled
connections) up to 5 times with backoff, runs the tool calls (all in
parallel when every call is `parallel_safe`, results appended in call
order), then takes steers. Every call goes through the context's `approve`
first. It stops on a reply without tool calls, `Length`, `Error`,
`Aborted`, `max_turns`, cancellation or `halt` (the session budget). A
request rejected as too long is masked, then compacted, and retried.

`Agent` adds around it: `prompt`/`steer`/`follow_up`/`abort`/`dequeue`,
`!cmd` shells, automatic compaction after a run, model hand-over to
`fallback_models` when a usage limit is hit (`Usage_limit`), goals, reminders
and budgets, delivery of background work's reports, and eviction.

| Feature | Modules |
|---|---|
| Compaction | `Compaction` (summarise older messages, keep a tail, pin the user's instructions), `Observation_mask` (cheap first tier: mask large tool results), `Compaction_settings`, `Context_overflow`; `Compaction_eval` tunes them |
| Approvals | `Approval_mode` (`Ask`, `Auto_edit`, `Smart`, `Auto`), `Approval_rule` (Claude Code's `TOOL(PATTERN)`), `Approval` (`decide`), `Approval_judge` (the model judging calls in `Smart`), `Allowed_call` |
| Plan and chat modes | `Plan_mode` (refuses implementing calls), `Plan` (`update_plan`), `Chat_mode` (no tool host) |
| Goal, reminders, resume | `Goal` (`/goal`), `Reminder` (`/remind`), resume after a usage limit (`Agent`) |
| Review | `Review` (`/review`), `Review_comment`, `Review_state`, `Review_snapshot` |
| Side requests | `Btw` (`/btw`), `Generate_text` (commit messages, PR text, titles), `Naming`, `Session_description`, `Subagent_name` |
| User data | `Memory`/`Tool_memory` and `Custom_instructions` (on `Note_store`), `User_info`, `User_prefs` (`util/`) |
| Skills | `Frontmatter`, `Skill` (discovery in `.prigh/skills`, `.claude/skills`, `.agents/skills`; `/skill:NAME`) |
| Worktrees | `Worktree` (`host/`), `Tool_worktree`, `Agent_worktree`, `Worktree_home`, `Worktree_watch` |
| Temporary sessions | `Leftover`, `Temporary_ledger` (`rpc/`): what they made, deleted when they end |

### Engines

A session runs on an engine (`engine/`): the **harness** (prigh's own
`Agent_loop`, providers and tools; `Harness_runner` in `agent/`) or the
**composer**, which drives an external coding agent (Codex, Claude Code,
ACP agents, prigh itself) on the session's tool host through an adapter.
`Agent` stays the session object either way (log, clients, queues,
approvals, hosts, worktrees, naming); only its runs go through `Runner`
(`Engine_intf`), whose `Capabilities` say what the engine supports.

`Engines` is what a backend deploys (`-engines harness,composer -adapters
codex,claude`, or `$PRIGH_ENGINES`/`$PRIGH_ADAPTERS`; the harness alone by
default), shared by all its namespaces, so users, settings and logins are
the same on either engine. A session's engine is its last `Engine` entry
(`Engine_choice`; none: the harness), set by `new_session`'s `engine` and
`adapter`; one the backend does not deploy, or this build cannot run, can
be read but not run (`state`'s `unavailable`). `hello` lists `engines` and
`adapters` (and the `engines` feature) when there is more than the harness;
`state` carries `engine`, `adapter` and `capabilities` off the harness.

`Adapter_health` (`composer/`) says whether each adapter's CLI can run on a
host: one `$probe_programs` call per host runs its `--version` and its
login-status command (`codex login status`, `claude auth status`), never a
login or a turn. `Rpc_server` caches the statuses per host and adapter for
a minute, forgets a host's when it (re)connects (`adapters_changed`), and
answers `list_adapters {refresh?}` (every connected host, the session's
first), each with the `problem` to show ("Claude Code on laptop is not
logged in: run `claude auth login` there"); prigh never installs a CLI or
logs in. prigh-web shows them in Settings → Engines (composer deployed
only), the TUI with `/adapters`.

Codex can run on one of prigh's ChatGPT logins (`Cli_login`: the user's
choice per adapter in their config's `adapters`; `Cli_credential`: an
`Account_ref`, own or shared through `Account_shares`, resolved and
refreshed on the owner's store, its usage in that login's ledger).
`Codex_login` sends `account/login/start {type: chatgptAuthTokens}` after
`initialize`, answers `account/chatgptAuthTokens/refresh` with freshly
refreshed tokens, and pushes new ones before a turn when they near expiry;
a Codex refusing the mode keeps its own login and the session gets a
notice. `set_adapter_login` changes the choice; a live process on another
login is restarted on the same thread at the next turn.

`Engine_gate` (`rpc/`) refuses, before dispatch, the methods the attached
session's engine lacks (`Capabilities.method_feature`: fork, rewind, trim
and delete, compact, `downsize_images`, `btw`, steering subagents, plan
mode), with what to do instead, named after the adapter ("Codex sessions
can't rewind in place: fork from that message instead (/fork)"); the
`Agent` vals behind them check too, and the frontends mirror the check
(`Session_engine`) to hide or disable the controls.

The composer (`composer/`), from the bottom up:

- **Codecs** (`codex/`, `claude/`, `acp/`): pure, no I/O. Each turns its
  CLI's lines into `Adapter_event`s (`engine/`: text and thinking deltas,
  tool start/update/end with a `Tool_kind` and diffs, approval requests,
  questions, plans, usage and cost, turn end with its `Stop`, errors
  classified as usage limits, auth, overload...) and builds the lines to
  send: `Codex_codec` for `codex app-server` (JSON-RPC: threads, turns,
  `turn/steer`, approvals, `item/tool/requestUserInput`), `Claude_codec`
  for `claude` stream-json and its control protocol (`can_use_tool`,
  `AskUserQuestion`, `ExitPlanMode`, in-band `mcp_message`; argv from
  `Claude_launch`), `Acp_client_codec` for the client side of ACP v1. Their
  tests replay transcripts recorded by T3 Code (`test/fixtures/`, `NOTICE`).
- **Adapters** (`Adapter_intf.S`, found by `Composer_adapters`):
  `Codex_adapter`, `Claude_adapter` and `Acp_adapter` wrap a codec around a
  `Line_transport` (launch argv and environment, start or resume a thread,
  send, steer, interrupt, answer, `Capabilities`); `Prigh_adapter` runs the
  harness in the backend. `Acp_adapter` is strictly the spec: capabilities
  from what the agent advertises (plan mode with a `plan` mode, images when
  it takes them), prigh's tools as a stdio MCP server in `session/new`'s
  `mcpServers`, a thread continued with `session/resume`, else
  `session/load` (loaded once the replay goes quiet, as some agents never
  answer it), else anew. Commands come from `Acp_agent` (`known`, or
  `-adapters acp:name=command`); one without fails at startup.
- **`Host_transport`** spawns the CLI on the session's active tool host
  through `$process` (`Host_stream`, see Tool hosts), with the MCP side
  channel on; a CLI not installed there fails with `Host_stream.Ended`'s
  message and makes `Adapter_health` check that host again.
- **`Composer_runner`** is the composer's `Runner`: it starts the CLI on
  the first turn and keeps it between turns, restarting it and resuming the
  recorded thread (a `Thread` entry per adapter and host) when it exited or
  its host went; routes approval requests through `Services.approve`,
  questions and the plan a CLI proposes on leaving plan mode (Claude Code's
  `ExitPlanMode`, Grok's `x.ai/exit_plan_mode`) through `ask_user`; steers mid-turn where the CLI can (Codex;
  Antigravity with another `session/prompt`, Grok with `_x.ai/interject`,
  per `Acp_agent.Quirks`), between turns otherwise; and records the cost the CLI reports as spend
  (`External_model` names its replies `codex/MODEL`, priced at zero).
- **`Adapter_catalog`** holds, per host and adapter, the models and usage
  limits the CLI's own login offers (Codex's `model/list` and
  `account/rateLimits/read`, Claude Code's `initialize` and `get_usage`),
  read by running the CLI briefly (the adapter's `catalog`) when
  `list_models` or `usage_limits` is asked before a session's first turn;
  the live process's `account/rateLimits/updated` or `rate_limit_event`
  then keep them current. Never prigh's own provider logins: a change goes
  to the session's clients as `models_changed`, and they ask again.
- **`Composer_recorder`** turns the events into the `Agent_event`s and
  messages a harness run makes, each external tool under prigh's name for
  it (`Canonical_tool`: `Bash` and `commandExecution` are `bash`,
  `fileChange` is `apply_patch`, `TodoWrite` is `update_plan`, `Task` a
  nested subagent), so cards, diffs, approval rules, search and plans work
  unchanged.
- **`Injected_tools`** serves prigh's tools the agents lack (`render_ui`,
  `memory`, `custom_instructions`, `sessions`, `ask_session`, the forge
  tools, `worktree`; `ask_user` for agents without questions of their own)
  as the MCP server `prigh`: in band for Claude Code, through `prigh
  mcp-bridge` on the tool host (`Mcp_bridge`, relaying stdio to
  `$PRIGH_MCP_SOCKET`) for Codex and ACP agents. Custom instructions and
  user info go as developer instructions (Codex) or `initialize`'s
  `appendSystemPrompt` (Claude Code), never on the command line. The CLIs
  always ask before acting (Codex `untrusted`, Claude Code `default` with
  `--setting-sources=user --strict-mcp-config`, so a repository's `.claude`
  settings and hooks do not apply): prigh's approval mode, rules and Smart
  judge decide, the mode only picking Codex's sandbox (Codex still runs
  commands it knows to be read-only without asking).
- **`Handoff`** quotes the session for an adapter taking over (another
  adapter or host, a thread it cannot resume; `set_adapter` switches):
  within a token budget, the latest messages verbatim under a header saying
  where the session came from and how to read what was left out
  (`sessions`), or only what that adapter has not seen when switching back.
- **`Checkpoint`** (`$checkpoint`) keeps each run's files under hidden refs:
  rewind restores them and rolls the agent's thread back; forks fork it.
- **`Fake_cli`** (`prigh fake-cli codex|claude --script FILE`, hidden)
  stands in for a CLI in the e2e tests: it answers `--version` and the
  login-status commands, and replays a transcript against the backend's.

### Subagents and background work

- `Tool_subagent` (`tools/`): `subagent {task, tools?, model?, thinking?,
  cwd?, max_turns?, context?, name?, fork?, worktree?, host?}` runs a nested
  `Agent_loop` in process with the parent's provider. At depth 0 it runs in
  the background and returns an id (`a<n>`); deeper calls block and return
  the report. `fork` starts from the caller's conversation
  (`Subagent_fork`, `Conversation`); `worktree` gives it a git worktree;
  `host` runs its tools on another host.
- `subagent_status`, `subagent_cancel`, `subagent_message`,
  `subagent_continue` act on running or finished subagents;
  `Subagent_inbox` delivers messages to a running one.
- Shell jobs: `bash` with `background: true` at depth 0 starts a job
  (`j<n>`) running the same call through the executor, so it runs on the
  active host; `Tool_jobs` has `job_status`, `job_output`, `job_input`,
  `job_kill`. `Config.auto_background_seconds` moves long foreground
  commands to the background.
- `Background_tasks` holds both kinds per agent (in the agent's switch, so
  `abort` leaves them running) and delivers each finished task's report
  exactly once: an idle agent starts a run with it, a running loop takes
  it at the next turn boundary.
- `Subagent_log` keeps every subagent's status and transcript (in the
  session's data directory) for frontends that join late.

## Models and providers

| Module | Role |
|---|---|
| `Content`, `Message` (`session/`) | content blocks (text, thinking with opaque signature, tool calls, server tools) and `User`/`Assistant`/`Tool_result` messages, with images |
| `Assistant_event`, `Assistant_builder` (`agent/`) | the streaming delta vocabulary every provider emits, and its accumulator |
| `Model` (`providers/`) | static model table: context, output, thinking options, prices; keyed `provider/id` |
| `Custom_provider`, `Model_registry` | user-defined endpoints from `config.json`'s `providers`, merged with their `GET /models` |
| `Provider` | the interface: `stream : Request.t -> cancel -> on_event -> Message.Assistant.t`; failures are stop reasons, not exceptions |
| `Anthropic` | Messages API: server-side web search, cache breakpoints, adaptive thinking, Claude Pro/Max OAuth shaping |
| `Openai_responses` | Responses API for `api.openai.com` and the ChatGPT Codex backend; encrypted reasoning replay |
| `Openai_chat`, `Deepseek` | chat completions with per-server `Quirks`; DeepSeek and `api: chat` custom providers |
| `Native_computer`, `Computer_native` | models' built-in computer-use tools |
| `Provider_router` | the provider the agent holds: resolves credentials per request and delegates |
| `Faux_provider` | scripted replies for tests and `-faux` runs |
| `Http_client`, `Http_pool`, `Sse`, `Sse_request` | shared kept-alive HTTPS connections, SSE parsing, streaming POST |
| `Image` (`session/`), `Image_limit` | image sniffing and downscaling for what models accept |

## Authentication and git hosting

- `Auth_store` (`auth/`): `~/.config/prigh/auth.json` (pi's format), atomic
  0600 writes under an in-process mutex and a cross-process lock.
  `Credential` is an API key or OAuth tokens.
- `Provider_accounts`: several logins per provider in the same file (the
  provider's own key is account `main`, others `account:PROVIDER:ID`;
  `Account_meta` holds labels, order, default and fallback under
  `"prigh:accounts"`). Requests name an account (`Provider.Request.account`,
  pinned per session by the agent); replies record it so encrypted
  reasoning is only replayed to the account that wrote it. The agent tries
  the provider's other fallback accounts before `fallback_models`.
- `Account_shares` (`-tokens`): the logins users share with each other.
  Grants live in the owner's `Account_meta`; a recipient's request names
  `Account_ref.Shared` (`~OWNER/ID`), and the router resolves (and
  refreshes) it on the owner's store in-process, so no client sees the
  secret. `Account_ledger` keeps each login's usage per user and day (the
  owner's own and recipients'; `auth_status`, `account_usage`);
  `Login_choice` lists a user's own and shared logins for pinning and
  fallback. Revoking pushes `accounts_changed` and unpins recipients.
- `Provider_auth`: which methods each provider supports, environment
  fallbacks, `resolve` (refreshing OAuth under the lock).
- `Login_manager`: runs one login flow at a time and turns its prompts into
  `auth` events (`auth_respond`, `auth_cancel`). Flows talk to the user
  through `Auth_interaction` (`Auth_terminal` for the CLI).
- OAuth: `Pkce`, `Oauth_callback_server` (loopback redirect),
  `Oauth_common`, `Oauth_anthropic`, `Oauth_openai_codex`; `Custom_login`
  for `/login custom`; `Subscription_usage` for `/usage`.
- `forge/`: GitHub and Forgejo over REST (no `gh`). `Forge_login` (device
  flow, PKCE, or a token), `Forge_credential` (in `auth.json` as
  `forge:HOST`), `Forge_api`, `Forge_pulls`, `Forge_issues`, `Forge_ci`.
  `Forges` is the per-user service shared by the RPC server and the
  `pull_request`/`issue`/`repo` tools. Git itself runs on the tool host
  (`Git_forge`, `Git_actions`, `Git_diff` in `host/`).

## Tools

- `Tool` (`tools/`): `{spec; run}` with a `Context.t` carrying the executor
  (which `Agent` points at the active host), `approve`, `ask_user`, `ui`
  and `files` (`File_access`: the disk, or an editor's buffers under ACP).
  `Tool_spec` flags: `parallel_safe`, `destructive`, `on_host`, `freeform`.
  `Tool_args` builds schemas; `Truncate` bounds output.
- `Tools.all` is the built-in set; `Tools.for_model` adjusts it per model.
- File and shell: `Tool_bash` (on `Process`, which kills the whole process
  tree), `Tool_read`, `Tool_write`, `Tool_edit`, `Tool_apply_patch`
  (`Apply_patch`), `Tool_ls`, `Tool_grep`/`Tool_find` (`rg`). `Seen_files`
  refuses writes over files changed since the agent last read them.
- Web: `Tool_web_fetch` (`Html_text`), `Tool_web_search` (provider-side).
- Interaction: `Tool_ask_user`, `Tool_render_ui` (`Ui_components`,
  `Ui_expr`, `Ui_host`), `Tool_goal`, `Tool_memory`, `Tool_custom_instructions`.
- Sessions: `Tool_sessions`, `Tool_ask_session`/`Tool_answer_session`
  (`Session_questions`).
- Git hosting: `Tool_pull_request`, `Tool_issue`, `Tool_repo`,
  `Tool_review_reply`, `Tool_worktree`.
- MCP (`mcp/`): `Mcp_config` (`~/.prigh/mcp.json`, `.mcp.json`),
  `Mcp_settings` (the user's own servers), `Mcp_hub` (runs them on the tool
  host), `Mcp_client` (stdio or streamable HTTP), `Mcp_oauth`, `Mcp_tools`.
- Computer use (`computer/`): `Tool_computer` drives `prigh-computer`
  (`desktop/computer/`) through `Computer_helper` on a host started with
  `-computer`; `Computer_consent` asks the user first;
  `Computer_screenshots` prunes old screenshots from requests;
  `Computer_fake` for tests.

## Sessions and persistence

`Session` (`session/`) is an append-only JSONL log forming a tree: every
entry has a `parent`, and the active conversation is the path from the root
to `head`. Rewinding moves `head`; forking copies the active path to a new
file. Besides messages, entries record model and thinking changes,
compaction summaries, masking, names, descriptions, cwds with their host, the
system prompt, goal, reminders, approval mode and rules, archiving, pinning,
spend, review changes and what the model was told, and for engines the session's engine (`Engine`), its external agent's threads (`Thread`) and turns (`External_turn`), and handoffs between adapters (`Handoff`). `Session.mli` lists them.

Related: `Message_page`/`Outline`/`Trim_point` (paged transcripts and
landmarks), `Session_search` (word index over saved sessions),
`Session_link` (`?session=ID` links), `Pinned_messages`, `Session_worktree`.

### Data on disk

| Path | Contents |
|---|---|
| `~/.config/prigh/auth.json` | provider credentials (pi-compatible) and git hosts' sign-ins (`forge:HOST`) |
| `~/.prigh/config.json` | `Config`: models, approval mode and rules, providers, forges, compaction, `worktrees_dir`, ... |
| `~/.prigh/sessions/<stamp>_<id>.jsonl` | session logs |
| `~/.prigh/sessions/<stamp>_<id>/` | per-session data (`subagents.jsonl`, the `Subagent_log`) |
| `~/.prigh/sessions/<stamp>_<id>.trace.jsonl` | the session's trace (see "Tracing") |
| `~/.prigh/sessions/.temporary/<id>.json` | a live temporary session's ledger |
| `~/.prigh/sessions/exports/` | default `/export` output |
| `~/.prigh/prefs.json` | frontends' preferences and the shared prompt history (`User_prefs`) |
| `~/.prigh/memory.json`, `custom_instructions.json`, `about_you.json` | the user's notes, standing instructions and profile |
| `~/.prigh/mcp-servers.json`, `mcp.json`, `mcp-approvals.json` | MCP servers and project-server approvals |
| `~/.prigh/AGENTS.md`, `~/.prigh/skills/` | global instructions and skills |
| `~/.prigh/host-id` | this machine's tool host id |
| `~/.prigh/tui-connection.sexp` | the saved remote connection (TUI, `prigh web`) |
| `~/.prigh/history` | the TUI's local prompt history |
| `~/.prigh/namespaces/<name>/` | a namespace's home under `-tokens` |

## Web server and remote access

| Module (`rpc/`) | Role |
|---|---|
| `Web_server` | the `-web` listener: `{`-first connections are JSON-lines clients; else HTTP: `/ws` (RPC), `/terminal`, `/files/*`, the static site |
| `Websocket` | minimal RFC 6455 server and client, with keepalive |
| `Static_site` | serves `frontend/web/bin/site` (`-web-root`, `$PRIGH_WEB_ROOT`): ETags, immutable versioned assets, precompressed files |
| `File_http`, `File_tickets`, `Files_rpc` | file downloads/uploads authorised by short-lived tickets |
| `Backend_address`, `Backend_socket` | `-connect` targets (`HOST:PORT` or a prigh-web URL), reconnecting client sockets |
| `Web_connect`, `Web_proxy`, `Signin_handoff` | `prigh web`: check the backend, serve the site, relay `/ws`, `/terminal` and files, hand the page its credentials |
| `Local_site` | `Host`/`Origin` checks for the local sites of `prigh web` and `prigh desktop` |
| `Browser_signin`, `Cli_signin_grant`, `Paste_code` | signing a CLI in through the browser past a sign-in proxy |
| `Saved_connection`, `Backend_choice` | the saved connection and which backend to use |
| `Desktop_sidecar` | `prigh desktop`'s site and state (form, checking, connected, sign-in) |

The TUI has its own copies of the address parsing, WebSocket client and
saved connection in `frontend/ui/server_address.ml` and `frontend/client/unix/`:
keep them in step.

### Terminals

The browser's shell panel (`terminal/`): a `Terminal` is a tmux session on
the private server `-L prigh`, driven by one control-mode client
(`Tmux_control`), so no pty is needed. `Terminals` serves the `/terminal`
WebSocket, keyed by session. Terminals run on the session's active host:
for a client host, `Terminal_relay`/`Terminal_channel` relay frames as
`terminal_*` events.

### ACP

`prigh acp` (`acp/`): `Json_rpc` is a JSON-RPC 2.0 peer; `Rpc_client` a
connection to prigh's RPC (in process or TCP); `Acp` maps ACP sessions to
ordinary prigh clients plus one tool-host client whose file tools go
through the editor's buffers. `Acp_prompt`, `Acp_tool_call`,
`Acp_question` and `Acp_commands` translate prompts, tool calls,
questions and slash commands.

### Nix store

`Nix_store_rpc` exposes `nix_store_status`/`nix_store_gc` to superusers
only; `Nix_store` runs one collection at a time through `Nix_store_ops`
(local `nix`, or the container's root helper: `DOCKER.md`).

## Tracing

`trace/` records spans of what a run spent time on, per session.

- `Trace` — spans (id, parent, name, track, wall-clock and monotonic
  times, status, attributes). The current span is an Eio fiber-local
  binding inherited by forked fibers, so parallel tool calls and subagents
  nest under whoever started them; with no tracer, or level `Off`, every
  call is a no-op. `Trace.phase` splits phased spans (`model_request`:
  `connect`, `first_token`, `thinking`, `stream`). The level
  (`Off`/`Default`/`Detailed`) is `$PRIGH_TRACE`, else the config's `trace`.
- `Trace_file` — appends spans to `<session>.trace.jsonl`, rotating to
  `.trace.old.jsonl`.
- `Model_trace` — wraps a provider's `stream` in a `model_request` span
  (model, tokens, cost, stop reason, phases).
- `Trace_report` — the summary, an indented tree, and a Chrome/Perfetto
  trace export: `prigh trace SESSION [-perfetto FILE] [-tree] [-slowest N]`
  and the `trace` RPC method.

Spans come from `Agent.start_run` (`prompt`), `Agent_loop` (`turn`,
`model_request`, `retry_wait`, `tool`, `compaction`, `mask`), `Agent`
(`approval_wait`, `ask_user_wait`, `host_roundtrip`), `Tool_subagent`
(`subagent`, on its own track), `Background_tasks` (`job`), and at
`Detailed` `Http_client`, `Sse_request` and the providers.

## Frontends (`frontend/`)

A separate dune project from the backend: the two meet only over the wire,
so the frontends own their copy of the protocol types.

- `protocol/` (`prigh_protocol`): `Jsonaf` decoders for everything
  `Rpc_json` emits and the `Request` encoder.
- `client/` (`prigh_client`, `Async_kernel` only so it links under
  js_of_ocaml): `Transport.t` and a reconnectable `Client` (correlates
  responses, fans out events). `client/unix/`: stdio, TCP, WebSocket and TLS
  transports, `Tool_host` (spawns and proxies to `prigh tool-host`),
  `Host_id`, `Browser_signin`.
- `ui/` (`prigh_ui`, `core` + `bonsai` only): `App`, an Elm-style pure
  state machine (`update : Model.t -> Action.t -> Model.t * Command.t list`),
  and `Component`, which wraps it in Bonsai and runs commands through a
  platform. Also the shared pieces: `Commands`, `Keymap`, `Fuzzy`,
  `Picker`, `Markdown`, `Tool_summary`, `Active_host`, `Transcript`, ...
- `tui/`: `lib/` (`Term_app` runs Bonsai_term and the platform commands,
  `Backend_link` the connection, target and tool host, `Key_of_event`,
  `View_of_content`, `Tty`), `bin/` (`prigh-tui`), `e2e/` and `tmux-test/`.
- `web/` (prigh-web): its own Elm-style `App` over the same protocol and
  client, rendering HTML.
  - `lib/` (`prigh_web`, pure: `core` + `virtual_dom`): `App`, `Chat`,
    `Chat_view`, composer, sidebar, settings, files,
    review, terminal, pull requests, `render_ui` views (`Ui_*`), ...
  - `app/` (`prigh_web_app`): the page's effects — `Ws_transport`,
    `Browser` (storage, clipboard, viewport), `Login`, `Desktop_page`,
    widgets and DOM listeners, the service worker.
  - `bin/`: `main.bc.js`, `index.html`, the stylesheets (`theme.css` holds
    every colour), the PWA manifest and icons; `bin/site` is the built site
    the backend serves.
  - `test/` and `e2e/` (Playwright scripts run with node).

Switching sessions resets everything that belongs to the old one: in
prigh-web, a state with a new session id goes through one reset path in
`App`. On reconnect it keeps everything on show and reconciles
(`Chat.reconcile`).

## Desktop app (`desktop/`)

A Rust workspace apart from the dune projects:

- `core/` — the decisions, without a window: launching the sidecar
  (`launch.rs`, `login_env.rs`, `search_path.rs`), reading its ready line
  (`ready.rs`), allowed navigation (`navigation.rs`), the browser-window
  choice (`browser.rs`), failure dialogs (`failure.rs`), zoom, profile.
- `app/` — the Tauri v2 shell (`prigh-desktop`, `src/main.rs`). It has no
  frontend of its own: `ui/index.html` is a blank page shown while the
  sidecar starts, then the window shows prigh-web. The page gets no Tauri
  IPC.
- `computer/` — `prigh-computer`, the computer-use helper (fake, wlroots,
  X11, macOS, Windows backends), spoken to as JSON lines by
  `Computer_helper`.
- `test/` — the window and browser window against a fake sidecar.

The sidecar contract (the shell's only coupling to the backend):

1. The shell runs `<backend> desktop [$PRIGH_DESKTOP_ARGS]` with a stdin
   pipe it holds open (`$PRIGH_DESKTOP_BACKEND`, else `prigh` on the PATH or
   the login shell's PATH; WSL on Windows).
2. `Desktop_sidecar` prints `{"url":"http://127.0.0.1:PORT/#desktop=KEY"}`
   when ready; later lines may send the window to the backend's own
   prigh-web (`"remote": true`) or ask to open a URL externally.
3. It exits when its stdin closes or the page's Quit is used. Closing the
   window closes the pipe; the sidecar exiting ends the app.

With `PRIGH_DESKTOP_WINDOW=browser` (or `auto` on Linux when one is found)
the shell opens an installed Chromium-family browser's app window instead
of a webview. Packaging is `nix/desktop.nix`; `.github/workflows/desktop.yml`
builds installers for Linux, macOS and Windows.

## CLI (`backend/bin/main.ml`)

| Command | What |
|---|---|
| `serve` | the backend: stdio, `-listen`, `-web` (`-open`, `-web-root`), `-token`/`-tokens`, `-no-backend-host`, `-session` (the default session), `-engines`/`-adapters` |
| `tool-host` | the local tool worker; `-connect`/`-remote` for a standalone host; `-computer` |
| `web` | prigh-web locally for a remote backend (`-connect`, else `$PRIGH_CONNECT` or the saved connection) |
| `desktop` | the desktop app's sidecar |
| `acp` | the Agent Client Protocol on stdio |
| `mcp-bridge` | prigh's tools for a CLI the composer runs (started by the CLI, relaying to `$PRIGH_MCP_SOCKET`) |
| `fake-cli codex\|claude --script FILE` | hidden: a stand-in for a CLI in the e2e tests |
| `run <prompt>` | headless run streaming to stdout (confirmations denied) |
| `sessions`, `login`, `logout`, `auth` | sessions and credentials without a backend |
| `trace SESSION` | `Trace_report` over a session's trace |
| `browser-signin` | sign in past a proxy (used by prigh-tui) |

`run` and `serve` share `-model`, `-thinking`, `-session`, `-cwd`,
`-no-tools`, `-faux`. With no model given, a new session uses the config's
`default_model`, else the first logged-in provider's.
