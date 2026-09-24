# prigh

A coding agent: OCaml backend (Eio, cohttp; Anthropic, OpenAI, OpenAI
Codex/ChatGPT and DeepSeek providers) and an OCaml frontend — the same
Bonsai logic mounted either in the terminal (Bonsai_term, OxCaml) or in a
browser (Bonsai_web, js_of_ocaml). No plugins; tools and subagents are built
in.

## Quick start

```
./prigh                 # builds backend + frontend if needed, then starts the TUI here
./prigh -cwd ~/proj     # ... in another directory
./prigh -faux           # scripted provider, no API calls
./prigh -web            # the same UI in a browser (serves it on 127.0.0.1:7788 and opens it)
```

## Build

```
# backend (opam switch "prigh": OCaml 5.3, core, eio, cohttp-eio, jsonaf)
cd backend && eval $(opam env --switch=prigh) && dune build && dune build @runtest

# frontend (opam switch "prigh-ox": OxCaml 5.2 + bonsai_term; or `nix develop`)
cd tui && eval $(opam env --switch=prigh-ox) && dune build && dune build @runtest
# @runtest includes the e2e and tmux tests against the backend binary built above,
# and the web-layer tests when `node` is on PATH (they run under js_of_ocaml)

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

`prigh -web [serve options]` runs the backend with the browser frontend
(`prigh serve -web 127.0.0.1:7788 -open`); `PRIGH_WEB_ROOT` points at the
built assets (the Nix wrapper sets it).

`prigh-tui` flags: `-faux`, `-session PATH`, `-model ID`, `-thinking LEVEL`,
`-cwd DIR`, `-auth-file PATH`, `-backend PATH` (same as `$PRIGH_BACKEND`),
`-connect HOST:PORT`, `-token SECRET`, `-tools local|remote`, `-name NAME`,
and `-- <extra backend args>`. The Nix wrapper sets `PRIGH_BACKEND` to the
Nix-built backend by default; set `PRIGH_BACKEND` or pass `-backend` to override
it.

The first `nix build`/`nix develop` evaluation is slow (opam-nix resolves the
pinned package set through import-from-derivation) and the first build compiles
both OCaml toolchains plus their package sets (about an hour on a small
machine); both are cached afterwards.

## Remote backend, several frontends

The backend can run on another machine and serve many sessions and
frontends at once:

```
# on the server (keep it in tmux; sessions live in ~/.prigh/sessions there)
prigh serve -listen 0.0.0.0:7777 -token sekrit

# on each laptop
prigh-tui -connect server:7777 -token sekrit -cwd ~/proj
prigh-tui -connect server:7777 -token sekrit -session <id>   # join a session
```

By default a connected frontend runs the session's tools (bash, file
edits, ...) on *its own* machine, in the `-cwd` it was started with, by
spawning `prigh tool-host` locally (so the prigh binary must be installed
there too; `-tools remote` runs them on the backend instead). Every session
has one *active tool host*; `/host` lists the backend and the connected
frontends and switches between them (asking for the working directory to
use on the new host, prefilled with the current one and checked there
before switching), and the status line shows `tools:<name>` when tools run
elsewhere or `tools:offline` when the active host has disconnected (tool
calls then fail until you pick another host). Several frontends can attach
to one session (`/sessions` marks live ones) and all see the same stream; a
session keeps running when its frontends disconnect. Plain TCP with a shared
token: bind to localhost and use an SSH tunnel on untrusted networks.

## In a browser

The frontend also runs as a web page, with the same keys, commands, pickers
and transcript as the terminal (it renders the same cell grid). The backend
serves it and speaks the RPC protocol over a WebSocket at `/ws`:

```
./prigh -web                      # local: serves http://127.0.0.1:7788/ and opens it
./prigh -web -faux -cwd ~/proj    # ... any `prigh serve` options after -web

# remote: on the server (or PRIGH_WEB_LISTEN=0.0.0.0:7788 ./prigh -web -token sekrit)
prigh serve -web 0.0.0.0:7788 -token sekrit
# then open http://server:7788/?token=sekrit (or type the token into the
# connect form, which remembers it in localStorage)
prigh-tui -connect server:7788 -token sekrit -cwd ~/proj   # terminals use the same port
```

The `-web` port also accepts the terminal frontend's plain JSON-lines
connections, so one port serves browsers and terminals on the same sessions
(`-listen` is only needed for a TCP-only port).

The page connects to its own origin by default; `?backend=ws://host:port/ws`
points a page served from one place at a backend elsewhere, `?session=ID`
joins a session and `?name=` names the frontend in `/host`. Tools run on the
backend (a browser cannot host them; `/host` still switches to any connected
tool host). Prompt history lives in `localStorage`; Ctrl+Z and Ctrl+G have no
browser equivalent and say so. Plain `ws://` with a shared token: bind to
localhost and use an SSH tunnel or a TLS-terminating proxy on untrusted
networks.

If the backend goes away (the spawned process dies, or the TCP connection
drops) the TUI reconnects on its own — immediately, then with exponential
backoff capped at 10s — rejoining the same session and respawning the
backend when it was spawned; `/retry-backend-connection` retries at once and
Ctrl+C quits meanwhile.

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

### Keys

| Key | Action |
|---|---|
| Enter | send the prompt; accept the highlighted item (for command arguments only once you have typed a filter or moved the highlight — `/model` Enter Enter opens the picker) |
| Alt+Enter | queue a follow-up to run after the current turn |
| Ctrl+J / Alt+J | insert a newline |
| Esc | close the dialog, abort the running turn, or scroll back to the bottom |
| Tab | accept the highlighted completion; on an empty prompt, list the commands |
| Up / Down | move up/down (editor line, history, or list row) |
| Alt+Up | pop the last queued steer/follow-up back into the editor |
| Left / Right | move the cursor |
| Alt+B / Ctrl+Left | move back one word |
| Alt+F / Ctrl+Right | move forward one word |
| Alt+D | delete the next word |
| Home / Ctrl+A | start of line |
| End / Ctrl+E | end of line |
| PageUp / PageDown | scroll the transcript / list a page |
| mouse wheel | scroll the transcript (selecting text needs Shift) |
| Ctrl+Up / Ctrl+Down | jump to the previous/next user message |
| Backspace / Ctrl+H | delete the character before the cursor |
| Delete | delete the character under the cursor |
| Ctrl+K / Ctrl+U | delete to the end / start of the line |
| Ctrl+W / Alt+Backspace | delete the word before the cursor |
| Ctrl+Y / Alt+Y | paste the most recent kill / replace it with an older kill |
| Ctrl+_ | undo |
| Ctrl+O | cycle transcript verbosity |
| Ctrl+R | complete a file path at the cursor (paths are listed on the active tool host, under the session cwd) |
| Ctrl+F | search the transcript |
| Ctrl+G | edit the prompt in `$VISUAL`/`$EDITOR` |
| Ctrl+L | pick a model |
| Ctrl+P / Alt+P | cycle to the next / previous scoped model |
| Ctrl+T | cycle the thinking level |
| Ctrl+N | picker: toggle the named-only / logged-in-only filter |
| Ctrl+X | copy the last assistant message |
| Ctrl+Z | suspend to the shell |
| Shift+Tab | cycle subagent focus: main → agent 1 → … → main |
| Alt+1…9 | focus agent N |
| Ctrl+C | clear the editor, or abort the running turn; again to quit |
| Ctrl+D | quit |

Ctrl+O cycles the transcript verbosity:

| Level | Shows |
|---|---|
| quiet | user and final assistant text only; tool calls and subagents collapse to one line |
| normal | thinking (first 3 lines), tool calls with a short result tail, subagent live tails |
| verbose | full thinking, tool arguments and results, every nested subagent event |

`/verbosity [quiet|normal|verbose]` sets it directly; the status line shows
`view:`.

A `subagent` tool call gets its own transcript. Shift+Tab cycles focus main →
agent 1 → …, Alt+1…9 jumps, and `/agents` opens a picker; Esc returns to main.
Finished agents stay cyclable until the next prompt. The status line shows an
`agents:` strip.

Typing `/` opens inline autocomplete (commands, then per-command arguments
such as models, sessions and directories). Typing `@` completes file paths
under the session's working directory; on submit the referenced files are sent as attachments and the
backend appends them to the user message as `<file>` blocks. `!cmd` runs a
shell command through the backend (streamed output shown as a tool item) and
adds it and its output to the context; `!!cmd` runs it without adding to the
context.

Submitted prompts are kept in `~/.prigh/history` (one JSON string per line,
last 500) and loaded on start; secrets from login prompts are never recorded.
`~/.prigh/config.json` holds `scoped_models` (the models Ctrl+P cycles
through; `/scoped-models` edits it) and `confirm_tools` (ask before
destructive `bash`/`write`/`edit`; `/confirm on|off`).

### Slash commands

| Command | Action |
|---|---|
| `/help`, `/hotkeys` | commands and keys / keys only |
| `/model [name\|id\|provider/id]` | pick or switch the model |
| `/scoped-models` | pick the models Ctrl+P cycles through |
| `/login [provider] [api_key\|oauth]`, `/logout [provider]`, `/auth` | credentials |
| `/thinking [off\|on\|low\|high\|max]` | set the thinking level |
| `/verbosity [quiet\|normal\|verbose]` | set the transcript verbosity |
| `/confirm [on\|off]` | ask before destructive tools |
| `/compact` | summarise older messages to free context |
| `/new`, `/clear`, `/quit` | new session / clear transcript / exit |
| `/name [text]` | set the session name |
| `/session` | show session statistics |
| `/sessions` | pick a saved session (Ctrl+N named-only, Ctrl+D delete) |
| `/switch [path]` | switch to a saved session |
| `/cd [path]` | change the working directory |
| `/fork`, `/rewind`, `/tree`, `/clone` | branch the session tree |
| `/export [path]`, `/import [path]` | markdown or `.jsonl` |
| `/agents` | focus a subagent |
| `/host [name\|backend]` | pick where tools run, and the directory there |
| `/abort` | abort the current run |
| `/retry-backend-connection` | reconnect to the backend now |
| `/state` | show session state |

`/model` also accepts a display name, id, `provider/id` or unique prefix, and
`/login`/`/logout`/`/thinking`/`/sessions` open fuzzy pickers.

`-faux-script FILE` (a backend flag, passed through the TUI after `--`) plays
a JSON array of scripted provider replies, so demos and tests run without API
calls; it implies `-faux`.

Sessions are JSONL trees under `~/.prigh/sessions/`. Project instructions are
read from `AGENTS.md`/`CLAUDE.md` files between `/` and the working
directory, plus `~/.prigh/AGENTS.md`. The system prompt is built once, at a
session's first run, and recorded in the session so the cached prompt prefix
survives `/cd`, `/host` and backend restarts; later directory or host changes
reach the model as a short note on the next message, leaving it to re-read
the instructions if that seems worthwhile.

### Testing

Four layers, cheapest first; `dune build @runtest` in each project runs all
of them (the last two need the backend binary built first), and `dune
promote` accepts new output everywhere:

1. **Pure expect tests** — `cd backend && dune build @runtest` drives the
   agent loop, tools and RPC with `Faux_provider` and the providers/HTTP/OAuth
   flows with an in-process EIO HTTP server; `cd tui && dune build @runtest`
   covers protocol/client decoding, the widgets, the keymap, and `App.update`
   scenarios that print the rendered screen (`Screen.to_plain`).
2. **Driver-frame snapshots** — `tui/test/test_term_frames.ml`, run by the
   same `cd tui && dune build @runtest`: drives the real Bonsai_term driver
   over an in-memory tty, decodes Notty's output with `tui/test/vt.ml`, and
   compares it with `Screen.to_plain`.
3. **Protocol e2e** — `tui/e2e` (also `dune build @e2e`) runs the real
   `backend/_build/default/bin/main.exe serve -faux` through the real client
   with an isolated `HOME`/`-auth-file` and diffs a normalised transcript.
4. **Real terminal** — the cram test `tui/tmux-test/run.t` (skipped without
   `tmux`) runs each scenario of `run.t/harness.sh` with the built TUI inside
   tmux and compares the captured panes, including a backend kill and
   reconnect.

## Layout

- `backend/lib` — `Agent_loop` (stream, run tools, repeat), `Agent`
  (session, queues, abort, compaction), `Tools` (bash, read, write, edit, ls,
  grep, find) and `Tool_subagent`, providers (`Anthropic`, `Openai_responses`,
  `Deepseek`, picked per request by `Provider_router`), auth (`Auth_store`,
  `Provider_auth`, `Oauth_anthropic`, `Oauth_openai_codex`, `Login_manager`),
  `Session` JSONL log, `Rpc_server`.
- `backend/test` — expect tests, driven by `Faux_provider` and an in-process
  HTTP server; no network.
- `tui/` — the frontend: `protocol/` (wire types), `client/` (Async_kernel RPC
  over a `Transport`; `client_unix/` has the stdio/TCP transports and the
  tool host), `ui/` (platform-agnostic Bonsai logic: `App` state machine,
  `Editor`, `Picker`, `Transcript`, `Render` to styled `Content`), `term/`
  (Bonsai_term views + backend process), `web/` + `web-app/` + `web-bin/`
  (DOM rendering, key mapping, WebSocket transport, the js_of_ocaml page),
  `test/`, `test-web/`, `e2e/`.
- `ARCHITECTURE.md` — how the pieces fit together; `PLAN.md` — milestone status.
# prigh
