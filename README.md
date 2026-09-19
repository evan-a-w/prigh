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

# or with Nix (backend + patched OxCaml frontend) — see "Running under Nix"
nix build               # result/bin/prigh wrapper (includes backend + TUI)
nix develop             # toolchain shell
```

## Running under Nix

The flake builds both executables. The default package/app is a small wrapper
that points the TUI at the Nix-built backend.

```
source /nix/var/nix/profiles/default/etc/profile.d/nix-daemon.sh   # if nix is not on PATH

nix run . -- -cwd ~/proj
nix run . -- -cwd ~/proj -faux       # scripted provider, no API calls

nix build                            # result/bin/prigh
./result/bin/prigh -cwd ~/proj

nix build .#backend                  # backend only: result/bin/prigh
nix build .#tui                      # TUI only: result/bin/prigh-tui

nix develop
cd tui && dune build @runtest
```

`prigh-tui` flags: `-faux`, `-session PATH`, `-model ID`, `-thinking LEVEL`,
`-cwd DIR`, `-auth-file PATH`, `-backend PATH` (same as `$PRIGH_BACKEND`), and
`-- <extra backend args>`. The Nix wrapper sets `PRIGH_BACKEND` to the
Nix-built backend by default; set `PRIGH_BACKEND` or pass `-backend` to override
it.

The first `nix build`/`nix develop` evaluation is slow (opam-nix resolves the
pinned package set through import-from-derivation) and the first build compiles
both OCaml toolchains plus their package sets (about an hour on a small
machine); both are cached afterwards.

## Use

Log in to at least one provider (credentials go to
`~/.config/prigh/auth.json`, same shape as pi's `auth.json`):

```
nix run .#backend -- login anthropic        # Claude Pro/Max (browser OAuth)
nix run .#backend -- login anthropic -method api_key
nix run .#backend -- login openai-codex     # ChatGPT Plus/Pro (browser OAuth)
nix run .#backend -- login openai           # OPENAI_API_KEY
nix run .#backend -- login deepseek         # DEEPSEEK_API_KEY
nix run .#backend -- auth                   # status; logout <provider> to remove
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
