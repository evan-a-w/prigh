# prigh

A coding agent: OCaml backend (Eio, cohttp; Anthropic, OpenAI, OpenAI
Codex/ChatGPT and DeepSeek providers) and an OCaml terminal frontend on
Bonsai_term (OxCaml). No plugins; tools and subagents are built in.

## Quick start

```
./prigh                 # builds backend + frontend if needed, then starts the TUI here
./prigh -cwd ~/proj     # ... in another directory
./prigh -faux           # scripted provider, no API calls
```

## Build

```
# backend (opam switch "prigh": OCaml 5.3, core, eio, cohttp-eio, jsonaf)
cd backend && eval $(opam env --switch=prigh) && dune build && dune build @runtest

# frontend (opam switch "prigh-ox": OxCaml 5.2 + bonsai_term; or `nix develop`)
cd tui && eval $(opam env --switch=prigh-ox) && dune build && dune build @runtest
dune build @e2e         # against the real backend binary built above

# or with Nix (patched OxCaml compiler, pinned package set) — see "Running under Nix"
nix build               # result/bin/prigh-tui
nix develop             # toolchain shell for tui/
```

## Running under Nix

The flake provides the *frontend* toolchain and binary; the backend is still
built with the vanilla `prigh` opam switch (see `HANDOFF.md` for why), so the
TUI needs to be told where that binary is.

```
source /nix/var/nix/profiles/default/etc/profile.d/nix-daemon.sh   # if nix is not on PATH

# 1. backend (once, or after backend changes)
(cd backend && eval $(opam env --switch=prigh) && dune build)

# 2a. run the Nix-built frontend against it
nix build
PRIGH_BACKEND=$PWD/backend/_build/default/bin/main.exe ./result/bin/prigh-tui
PRIGH_BACKEND=$PWD/backend/_build/default/bin/main.exe ./result/bin/prigh-tui -faux   # no API calls
# or, without building first:
PRIGH_BACKEND=$PWD/backend/_build/default/bin/main.exe nix run . -- -cwd ~/proj

# 2b. or develop: the shell has ocamlopt/dune/bonsai_term/ocamlformat for tui/
nix develop
./prigh -faux                       # builds tui/ with the shell's dune, spawns the backend
cd tui && dune build @runtest        # frontend tests; `dune build @e2e` needs the backend build
```

`prigh-tui` flags: `-faux`, `-session PATH`, `-model ID`, `-thinking LEVEL`,
`-cwd DIR`, `-auth-file PATH`, `-backend PATH` (same as `$PRIGH_BACKEND`), and
`-- <extra backend args>`. Without `-backend`/`PRIGH_BACKEND` it looks for
`backend/_build/default/bin/main.exe` relative to its own location, which works
for the dune build in `tui/` but not for the copy in `result/`.

The first `nix build`/`nix develop` evaluation is slow (opam-nix resolves the
pinned package set through import-from-derivation) and the first build compiles
the patched OxCaml compiler plus ~200 packages (about an hour on a small
machine); both are cached afterwards.

## Use

Log in to at least one provider (credentials go to
`~/.config/prigh/auth.json`, same shape as pi's `auth.json`):

```
backend/_build/default/bin/main.exe login anthropic        # Claude Pro/Max (browser OAuth)
backend/_build/default/bin/main.exe login anthropic -method api_key
backend/_build/default/bin/main.exe login openai-codex     # ChatGPT Plus/Pro (browser OAuth)
backend/_build/default/bin/main.exe login openai           # OPENAI_API_KEY
backend/_build/default/bin/main.exe login deepseek         # DEEPSEEK_API_KEY
backend/_build/default/bin/main.exe auth                   # status; logout <provider> to remove
```

The same is available inside the TUI as `/login [provider] [api_key|oauth]`,
`/logout <provider>` and `/auth`. A stored credential wins; otherwise
`ANTHROPIC_OAUTH_TOKEN`/`ANTHROPIC_API_KEY`, `OPENAI_API_KEY` and
`DEEPSEEK_API_KEY` are used. OAuth tokens are refreshed automatically
(under a cross-process lock) when they are within five minutes of expiry.
Model ids can be qualified as `provider/id` (e.g. `openai-codex/gpt-5.5`).

Then:

```
tui/_build/default/bin/main.exe                # interactive TUI (PRIGH_BACKEND or the dune build)
backend/_build/default/bin/main.exe run "explain this repo"    # headless
backend/_build/default/bin/main.exe sessions   # list saved sessions
backend/_build/default/bin/main.exe serve      # JSON-lines RPC on stdio
```

TUI: Enter sends (or steers while a run is active), Alt+Enter/Ctrl+J inserts a
newline, Esc closes a dialog or aborts, Tab completes `/commands` (or opens the
command picker), Up/Down browse history, PageUp/PageDown scroll, Ctrl+O
expands tool output, Ctrl+C twice quits. `/model`, `/thinking`, `/sessions`,
`/login` and `/logout` open fuzzy pickers; `/model` also accepts a display
name, id, `provider/id` or unique prefix. `/help` lists commands and keys.

Sessions are JSONL trees under `~/.prigh/sessions/`. Project instructions are
read from `AGENTS.md`/`CLAUDE.md` files between `/` and the working
directory, plus `~/.prigh/AGENTS.md`.

## Layout

- `backend/lib` — `Agent_loop` (stream, run tools, repeat), `Agent`
  (session, queues, abort, compaction), `Tools` (bash, read, write, edit, ls,
  grep, find) and `Tool_subagent`, providers (`Anthropic`, `Openai_responses`,
  `Deepseek`, picked per request by `Provider_router`), auth (`Auth_store`,
  `Provider_auth`, `Oauth_anthropic`, `Oauth_openai_codex`, `Login_manager`),
  `Session` JSONL log, `Rpc_server`.
- `backend/test` — expect tests, driven by `Faux_provider` and an in-process
  HTTP server; no network.
- `tui/` — the frontend: `protocol/` (wire types), `client/` (Async RPC over
  a `Transport`), `ui/` (platform-agnostic Bonsai logic: `App` state machine,
  `Editor`, `Picker`, `Transcript`, `Render` to styled `Content`), `term/`
  (Bonsai_term views + backend process), `test/`, `e2e/`.
- `ARCHITECTURE.md` — how the pieces fit together; `PLAN.md` — milestone status.
# prigh
