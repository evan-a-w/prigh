See `../AGENTS.md` for the conventions; this file adds the backend's.

Build and test in the dev shell (OxCaml, shared with the frontends):
`nix develop`, then `dune build @runtest` here.

`lib/` is one library (`prigh`) whose modules are grouped in
subdirectories with `(include_subdirs unqualified)`, so a module's name
does not depend on its directory: moving a file between groups changes no
code. Put a new module in the group it belongs to:

- `agent/` — `Agent`, `Agent_loop`, `Harness_runner`, compaction,
  approvals, plans, goals, reminders, review, subagents' log, the system
  prompt, skills, memory
- `engine/` — what both engines share: `Runner` (`Engine_intf`),
  `Capabilities`, `Engines` (what is deployed), `Engine_choice`,
  `Adapter_id`, `Adapter_event`, `External_model`, `Acp_agent`
- `composer/` — the composer engine: `Composer_runner`,
  `Composer_recorder`, adapters (`Adapter`, `Prigh_adapter`), transports
  (`Line_transport`, `Host_transport`), `Injected_tools`, `Handoff`,
  `Adapter_health`, `Fake_cli`; `codex/`, `claude/` and `acp/` hold each
  CLI's codec and adapter
- `session/` — the JSONL `Session` and what it records: `Message`,
  `Content`, `Image`, `Usage`, pages, outlines and search of a transcript
- `providers/` — `Provider`, `Provider_router`, Anthropic, OpenAI, DeepSeek,
  custom and faux providers, `Model`, `Model_registry`, `Sse`, `Http_client`
- `auth/` — `Credential`, `Auth_store`, `Provider_auth`, OAuth flows,
  `Login_manager`
- `tools/` — `Tool`, `Tools`, the built-in tools (`Tool_*`) and their
  helpers (`Process`, `Apply_patch`, `Truncate`, HTML to text)
- `host/` — `Tool_host`, `Host_ops`, `Host_process`, `Host_stream`, the tool
  host's files, git and worktrees
- `forge/` — git hosting: GitHub and Forgejo sign-ins, their REST APIs,
  pull requests (`Forges`)
- `mcp/` — MCP client, config, hub and tools
- `computer/` — computer use: `Computer_action`, `Computer_info`, the
  `prigh-computer` client (`Computer_helper`, `Computer_fake` for tests),
  `Tool_computer`, `Computer_screenshots`
- `rpc/` — `Rpc_server`, `Rpc_router`, `Rpc_json`, namespaces, users and
  access, `Engine_gate`, `Web_server`, `Websocket`, static and file HTTP,
  `prigh web` and `prigh desktop`
- `acp/` — `prigh acp`, the Agent Client Protocol for editors: `Acp`
  and its `Acp_*` helpers, on `Json_rpc` and `Rpc_client`
- `nix/` — the Nix store for superusers: `Nix_store` (status, one garbage
  collection at a time), `Nix_store_ops` (nix as the backend's user, or the
  container's root helper), what Nix prints and the reports
- `terminal/` — the browser terminals: `Terminal`, `Terminals`, tmux control,
  relays
- `trace/` — tracing (see "Tracing" in `ARCHITECTURE.md`)
- `util/` — `Import`, `Config`, `Cancellation`, `Guarded`, `Utf8` and other
  small shared pieces

`test/` stays flat: its subdirectories are separate dune targets
(`fake_mcp_server/`, `terminal/`, and `full/`, the slow tier that only
`./test --full` runs, and only when the user asks; see `AGENTS.md`).

Concurrency uses Eio (effects, direct style). Functions that do I/O take
[~env:Env.t] (= Eio_unix.Stdenv.base). Tests wrap their body in
[Eio_main.run]. Cancellation is via [Cancellation.t] (promise-based) and
[Cancellation.protect], not by catching Eio's cancel exception.

Tests use the helpers in `test/tool_test_helpers.ml` ([with_sandbox] for a
temporary directory and environment, [mask] to hide paths, ids and
timestamps in printed output). `test/dune` sets `PRIGH_HOST_NAME` to
`<host>`, so the backend's own host has that name on every machine (never
one of the tool hosts the tests make up: `laptop`, `desktop`, ...).

Anything a client can name (paths, sessions, users) must respect the
server's mode: with token namespaces a client only reaches its own
namespace (`User_access`, `Rpc_router`), and without the backend tool host
nothing a client sends may touch the backend's own files
(`Rpc_server.session_file`).
