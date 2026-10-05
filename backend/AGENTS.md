See `../AGENTS.md` for the conventions; this file adds the backend's.

Build and test in the backend dev shell (vanilla OCaml 5.3):
`nix develop .#backend`, then `dune build @runtest` here.

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
