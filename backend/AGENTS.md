See `../AGENTS.md` for the conventions; this file adds the backend's.

Build and test in the dev shell (OxCaml, shared with the frontends):
`nix develop`, then `dune build @runtest` here.

`lib/` is one library (`prigh`) whose modules are grouped in
subdirectories with `(include_subdirs unqualified)`, so a module's name
does not depend on its directory: moving a file between groups changes no
code. Put a new module in the group it belongs to:

- `agent/` — `Agent`, `Agent_loop`, compaction, approvals, plans, goals,
  reminders, subagents' log, the system prompt, skills
- `session/` — the JSONL `Session` and what it records: `Message`,
  `Content`, `Image`, `Usage`, pages and outlines of a transcript
- `providers/` — `Provider`, `Provider_router`, Anthropic, OpenAI, DeepSeek,
  custom and faux providers, models, SSE and the HTTP client
- `auth/` — credentials, `Auth_store`, OAuth flows, `Login_manager`
- `tools/` — `Tool`, `Tools`, the built-in tools (`Tool_*`) and their
  helpers (processes, patches, truncation, HTML to text)
- `host/` — `Tool_host`, `Host_ops`, the tool host's files and git
- `forge/` — git hosting: GitHub and Forgejo sign-ins, their REST APIs,
  pull requests (`Forges`)
- `mcp/` — MCP client, config, hub and tools
- `rpc/` — `Rpc_server`, `Rpc_router`, `Rpc_json`, namespaces and access,
  `Web_server`, `Websocket`, static and file HTTP
- `nix/` — the Nix store for superusers: `Nix_store` (status, one garbage
  collection at a time), `Nix_store_ops` (nix as the backend's user, or the
  container's root helper), what Nix prints and the reports
- `terminal/` — the browser terminals: `Terminal`, `Terminals`, tmux control,
  relays
- `util/` — `Import`, `Config`, `Cancellation`, `Guarded`, UTF-8 and other
  small shared pieces

`test/` stays flat: its subdirectories are separate dune targets
(`fake_mcp_server/`, `terminal/`).

Concurrency uses Eio (effects, direct style). Functions that do I/O take
[~env:Env.t] (= Eio_unix.Stdenv.base). Tests wrap their body in
[Eio_main.run]. Cancellation is via [Cancellation.t] (promise-based) and
[Cancellation.protect], not by catching Eio's cancel exception.

Tests use the helpers in `test/tool_test_helpers.ml` ([with_sandbox] for a
temporary directory and environment, [mask] to hide paths, ids and
timestamps in printed output).

Anything a client can name (paths, sessions, users) must respect the
server's mode: with token namespaces a client only reaches its own
namespace (`User_access`, `Rpc_router`), and without the backend tool host
nothing a client sends may touch the backend's own files
(`Rpc_server.session_file`).
