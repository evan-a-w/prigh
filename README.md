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
./prigh -prigh-web      # prigh-web, the browser-native UI (127.0.0.1:7790)
```

## Build

```
# backend: vanilla OCaml 5.3, core, eio, cohttp-eio, jsonaf
nix develop .#backend -c sh -c 'cd backend && dune build @runtest'

# frontend: OxCaml 5.2 + bonsai_term (the default dev shell)
nix develop -c sh -c 'cd tui && dune build @runtest'
# @runtest includes the e2e and tmux tests against the backend binary built above,
# and the web-layer tests (they run under node, via js_of_ocaml)

nix build               # result/bin/prigh wrapper (includes backend + TUI)
```

The two need different compilers, so build each in its own shell. Without
Nix, opam switches with the same packages work too (`backend/prigh.opam`,
`tui/prigh_tui.opam`; the OxCaml one needs `nix/fix-floatarithmem.patch`).

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

nix develop                          # TUI toolchain (OxCaml)
nix develop .#backend                # backend toolchain (OCaml 5.3)
```

`prigh -web [serve options]` runs the backend with the browser frontend
(`prigh serve -web 127.0.0.1:7788 -open`); `PRIGH_WEB_ROOT` points at the
built assets (the Nix wrapper sets it). Likewise `prigh -prigh-web [serve
options]` (`nix run . -- -prigh-web -cwd ~/proj`) runs `prigh serve
-prigh-web 127.0.0.1:7790 -open` (`$PRIGH_PRIGH_WEB_LISTEN`) with
`PRIGH_PRIGH_WEB_ROOT` set to the installed prigh-web site.

`prigh-tui` flags: `-faux`, `-session PATH`, `-model ID`, `-thinking LEVEL`,
`-cwd DIR`, `-auth-file PATH`, `-backend PATH` (same as `$PRIGH_BACKEND`),
`-connect HOST:PORT`, `-token SECRET`, `-user NAME` (with `-tokens`),
`-tools local|remote`, `-name NAME`,
and `-- <extra backend args>`. The Nix wrapper sets `PRIGH_BACKEND` to the
Nix-built backend by default; set `PRIGH_BACKEND` or pass `-backend` to override
it.

The first `nix build`/`nix develop` evaluation is slow (opam-nix resolves the
pinned package set through import-from-derivation) and the first build compiles
both OCaml toolchains plus their package sets (about an hour on a small
machine); both are cached afterwards.

The flakes name the binary cache `prigh.cachix.org`, which has the OxCaml
toolchain and packages prebuilt, so that most of that is downloaded instead.
Nix asks before using a flake's settings: answer `y` (and save it), or pass
`--accept-flake-config`. With a multi-user Nix install it is only used if you
are in `trusted-users`, or if `https://prigh.cachix.org` and its key are in
the system's `trusted-substituters`/`trusted-public-keys`; otherwise add them
to `/etc/nix/nix.conf` (NixOS: `nix.settings`) as `extra-substituters` and
`extra-trusted-public-keys`. The key is in `flake.nix`.

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
elsewhere or `tools:offline` when the active host has disconnected. A
session never leaves its host on its own: while the host is away tool calls
fail with a message saying it waits for the host to reconnect, and when the
host comes back (a frontend or `prigh tool-host` reconnecting or restarted
on the same machine, which is the same host) the session carries on there,
in the same directory; `/host` picks another host meanwhile. This holds
across the session leaving memory and backend restarts too: a session
records its host with its directory. A new session (`/new`, `/fork`,
`/clone`, an import, or the one a frontend starts in) starts on the
frontend's own machine, in the directory you were in there (its `-cwd` for
the first); from the web UI,
where the session you were on runs. A fork or clone of a session you moved
with `/host` stays where you put it. A machine's identity is the id in
`~/.prigh/host-id` (created on first use; edit it to choose one), shared by
its TUIs and `prigh tool-host`: with two TUIs open, the newer one runs the
machine's tools and the older one takes over again if the newer one quits. Several frontends can attach
to one session (`/sessions` marks live ones) and all see the same stream; a
session keeps running when its frontends disconnect. Plain TCP with a shared
token: bind to localhost and use an SSH tunnel on untrusted networks.

A tool host doesn't need a TUI: `prigh tool-host -connect server:7777 -token
sekrit -cwd ~/proj [-name NAME] [-host-id ID]` connects on its own
(reconnecting when the connection drops; it is the machine's host, by
`~/.prigh/host-id`, unless `-host-id` names another) and hosts the session's tools *and* its `>_` terminal, so
a browser-only user can pick it with `/host`. The web UI's terminal always
runs on the session's active host, relayed through the backend.

### Several users (namespaces)

```
prigh serve -web 0.0.0.0:7788 -tokens 'me=sekrit,alice=0ther'   # or $PRIGH_TOKENS
```

Each `name=token` entry is a user: log in with the name as the user name
and the token as the password. The web UI's connect form asks for both
(and remembers them in the browser); `/signout` or the page's `sign out`
button forgets them and shows the form again, so someone else can log in.
The TUI and tool hosts take `-user NAME -token TOKEN` (`$PRIGH_USER`,
`$PRIGH_TOKEN`); the token alone is still accepted, so scripts need not send
a user name, but a user name that doesn't match the token is refused like a
wrong password. Both UIs show the logged-in user as `user:<name>` in the
status line. Outside this mode the user name is ignored (leave it empty) and
`/signout` only forgets the saved token; in the terminal TUI `/signout`
just explains that it is browser-only (restart with other credentials).

Each user has its own namespace: its own provider logins, sessions,
config (`default_model`, scoped models, ...), global `AGENTS.md`, connected
tool hosts and terminals, under `~/.prigh/namespaces/<name>/` (the name
`default` keeps the usual `~/.prigh` and `~/.config/prigh` paths). Clients
in one namespace never see another's. In this mode provider keys are never
read from the environment; log in per namespace with `/login`.

`-no-backend-host` (or `PRIGH_NO_BACKEND_HOST=1`) removes the backend's own
tool host: nothing runs on the server, every session's tools and terminal
run only on hosts its namespace has connected (`prigh tool-host -connect`
or a TUI), so namespaces cannot reach each other's files through the
server.
Then the server also refuses clients' paths outside their sessions
directory (`/switch`, `/import`, deleting sessions), and `/export` writes
on the tool host.

`-superusers NAME,...` (`$PRIGH_SUPERUSERS`) lets those users act as any
other: `/setusr NAME` (in the TUI, the web UI, pi-web and prigh-web, where
the account menu's "Act as…" does it too) switches the
connection to NAME's sessions, logins and tool hosts (the status line shows
`user:<me> as <them>`), `/setusr` alone lists the users and `/setusr <me>`
switches back. Everyone else gets an error. `-host-tokens NAME=TOKEN,...`
(`$PRIGH_HOST_TOKENS`) adds tokens that sign in as NAME but never as a
superuser, for tool hosts that should not hold the user's own token (the
Docker image starts one per user with one).

## In a browser

The frontend also runs as a web page, with the same keys, commands, pickers
and transcript as the terminal (it renders the same cell grid). The backend
serves it and speaks the RPC protocol over a WebSocket at `/ws`:

```
./prigh -web                      # local: serves http://127.0.0.1:7788/ and opens it
./prigh -web -faux -cwd ~/proj    # ... any `prigh serve` options after -web

# remote: on the server (or PRIGH_WEB_LISTEN=0.0.0.0:7788 ./prigh -web -token sekrit)
prigh serve -web 0.0.0.0:7788 -token sekrit
# then open http://server:7788/ and type the token into the connect form's
# Password field (the browser remembers it in localStorage; /signout forgets it)
prigh-tui -connect server:7788 -token sekrit -cwd ~/proj   # terminals use the same port
```

The `-web` port also accepts the terminal frontend's plain JSON-lines
connections, so one port serves browsers and terminals on the same sessions
(`-listen` is only needed for a TCP-only port).

The page connects to its own origin by default; `?backend=ws://host:port/ws`
points a page served from one place at a backend elsewhere, `?session=ID`
joins a session (the page keeps it up to date, so a reload stays in the
session) and `?name=` names the frontend in `/host`. Tools run on the
backend (a browser cannot host them; `/host` still switches to any connected
tool host). Prompt history lives in `localStorage`; Ctrl+Z and Ctrl+G have no
browser equivalent and say so. The `>_` button (top right) opens a shell on
the backend machine in the session's directory, in a panel under the app
(it needs `tmux` on the backend; the nix package brings its own). Closing
the panel keeps the shell, so reopening it or reloading the page finds it
again; it is killed after 10 minutes with no page attached, when it exits,
or when the backend stops. Plain `ws://` with a shared token: bind to
localhost and use an SSH tunnel or a TLS-terminating proxy on untrusted
networks.

### pi's web UI (deprecated)

pi-web is deprecated: use prigh-web (below), which covers everything it
does. It still works but gets no new features.

A second, separate browser frontend lives in `pi-web/`: pi's own web UI
(Preact/TypeScript) on the prigh backend, which speaks pi's RPC protocol for
it (`Pi_rpc`). It has its own flake:

```
nix run ./pi-web -- -faux -cwd ~/proj     # serves it on 127.0.0.1:7789 and opens it
./prigh -pi-web -faux                      # development (npm build + dune-built backend)
prigh serve -pi-web 0.0.0.0:7789 -token sekrit   # remote; the page asks for the token
```

One backend can serve both web UIs (on different ports, since their `/ws`
speak different protocols) with shared sessions; the pi-web wrapper also
knows where the Bonsai site is, so this is the one-liner to run at boot
(e.g. a `@reboot` crontab entry):

```
cd ~/dev/prigh && PRIGH_PI_WEB_LISTEN=0.0.0.0:7789 nix run ./pi-web -- -web 0.0.0.0:7777 -token sekrit -cwd ~/dev/prigh > ~/prigh.stdout 2> ~/prigh.stderr
# Bonsai UI on :7777, pi-web on :7789
```

See `pi-web/README.md` for what was kept, dropped and mapped.

### prigh-web

`tui/prigh-web/` is a third browser frontend, in OCaml like the TUI but
built for the browser rather than mounting the terminal UI: a real DOM
(Bonsai_web, js_of_ocaml) with a session sidebar, a chat transcript with
markdown, collapsible tool calls and their images, model and thinking
selectors, and a composer that takes pasted or dropped images, `/`
commands, `@` paths and `!cmd` like the TUI. Messages show when they were
sent, in the browser's time zone (`14:32`, `Yesterday 14:32`, `3 Oct
14:32`; the full date on hover): under your messages and in a reply's
footer, with a separator where the day changes. It speaks
prigh's own RPC (the same `/ws` protocol as `-web`), so it gets every
backend feature without a translation layer like pi-web's.

Subagents and background jobs have their own panel, a resizable column
right of the chat (drag its left edge; a full-screen sheet on a phone):
the session's subagents as a tree (nested ones under the background agent
that started them), running first, with their task, model, elapsed time,
turns and current tool, and the background jobs with their last line.
Selecting one shows a subagent's live transcript and report (with Cancel
and "Show in chat", which scrolls to its card) or a job's output (with
Kill). It opens from the status line ("2 agents running"), the top bar's
robot button, a subagent's card in the chat, `/agents [n|id]` and `/jobs
[id]`, and Alt+1…9 (the Nth listed; Alt+] and Alt+[ step through them, Alt+0 closes
it, Esc goes back from outside the editor). Unlike the TUI, Shift+Tab is
left to the browser (it moves the focus back), and on macOS Option+digit
types its character instead. What finished before your latest prompt
folds under "Earlier"; switching sessions empties the panel. `/agents cancel
<n|id>` and `/jobs kill <id>` open the item there and stop it.

```
./prigh -prigh-web -faux -cwd ~/proj           # development: dune-built site and backend, 127.0.0.1:7790
nix run . -- -prigh-web -cwd ~/proj            # the packaged site and backend
prigh serve -prigh-web 0.0.0.0:7790 -token sekrit   # remote; the page asks for the token
```

`-prigh-web-root DIR` (`$PRIGH_PRIGH_WEB_ROOT`) points at the site;
without it the backend looks next to its dune build and in
`../share/prigh/prigh-web`. The page keeps `?session=` in the address bar
(a reload rejoins the session), reconnects with backoff when the backend
goes away, and takes `?backend=ws://host:port/ws` like the `-web` page.
One backend can serve all three web UIs on different ports. In Docker it
is `PRIGH_MODE=prigh-web` (port 7790, see `DOCKER.md`).

The terminal button in the top bar (or `/terminal`, or Ctrl+`) opens a
shell where the session's tools run (the active `/host`, in its
directory) in a panel under the chat: drag its top edge to resize it (the
height is remembered), a full-screen sheet on a phone. Keys typed in it
are the shell's, Esc included; Ctrl+` closes it (so does its ×) and
gives the keyboard back to the editor. Like the `-web` page's `>_` panel
it is a tmux session, so closing the panel, reloading the page (which
reopens it) or losing the connection keeps the shell; it is killed after
10 minutes with no page attached. It follows the session, `/host` and
the user you act as; when the shell exits, "New shell" starts another,
and when there is none (no tmux on the host, a host that has gone) the
panel says why and what to do.

prigh-web has every slash command of the table below, as pickers and
dialogs rather than lines in a transcript: `/session` is a dialog of the
session's details and statistics; `/fork`, `/rewind` (with a confirmation)
and `/tree` (the whole tree, branches indented) pick a message;
`/scoped-models` is a checklist (Tab or a click checks a model, Enter
saves); `/cd`, `/host`'s directory, `/export` and `/import` ask for a path
with completions from the backend and keep the dialog open, with the error,
when the backend refuses it; `/agents` and `/jobs` open the agents panel
(above); `/btw` answers in a panel above the composer (Esc closes it);
`/verbosity` (Ctrl+O cycles) hides tool output and thinking (quiet) or
unfolds everything (verbose); `/copy` (Ctrl+X with nothing selected) copies
the last reply. `/skills` is a picker of the skills where the tools run
(name, description, directory) whose Enter puts `/skill:name ` in the
editor for you to add the arguments; typing `/skill:` completes their
names (listed once per session, directory and tool host). A message that
invoked a skill shows as a folded `skill name` card (its location and
instructions; verbose unfolds it) above the arguments you typed, and
`/fork` and `/tree` show it as typed. `/mcp` lists the MCP servers with
their status, tool count or error and config file, the configuration's
problems under them: Enter approves (and starts) a project's server,
restarts a failed one, or lists a ready one's tools; `/mcp reconnect`
restarts the failed ones and reports. `/fallback` completes a model for
each word, in order (or `off`), and shows the backend's error, with the
closest models, for one it doesn't know; `/default-dir` completes
directories on the backend's host, where new sessions start, and takes a
relative path from the backend's directory. When a model's usage runs out
and the next fallback model takes over, the transcript shows a `↪ handed
over from A to B (error)` line rather than the message as a prompt, and
`/fork` and `/tree` label it the same way. Ctrl+P/Alt+P cycle the scoped
models, Alt+T the thinking level (Ctrl+T is the browser's), Alt+↑ takes
back a queued message, Ctrl+↑/↓ jump between your messages; `/help` and
`/hotkeys` also list the TUI's keys that the browser keeps (Ctrl+C, Ctrl+Z, Ctrl+F, Ctrl+R, $EDITOR's Ctrl+G)
and what to use instead, and `/quit` says to close the tab.

The account at the bottom of the sidebar (also in the status line) opens the
account menu: switch to another saved account in one click (the page
reloads signed in as it, in its last session), add an account (the sign-in
form, keeping the others; it may be on another backend), sign out of this
one, and, for `-superusers`, act as another user and come back. The
accounts (user, token, backend, last session) are kept in localStorage
(`prigh-web.accounts`); the active one is also in `prigh.user`/`prigh.token`,
where the `-web` page keeps its login, and the sign-in page lists them.

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
nix run .#backend -- login custom           # an OpenAI-compatible endpoint (below)
nix run .#backend -- auth                   # status; logout <provider> to remove
```

To share pi's logins instead, symlink the file: `ln -s ~/.pi/agent/auth.json
~/.config/prigh/auth.json`. Don't copy it: Anthropic rotates the refresh
token on every refresh, so a copy dies as soon as the other side refreshes.
Through the symlink both use one grant; expired tokens are refreshed under
the same lock pi uses (`auth.json.lock` directory, `proper-lockfile`
protocol) and written back for the other side.

The same is available inside the TUI as `/login [provider] [api_key|oauth]`,
`/logout <provider>` and `/auth`. A stored credential wins; otherwise
`ANTHROPIC_OAUTH_TOKEN`/`ANTHROPIC_API_KEY`, `OPENAI_API_KEY` and
`DEEPSEEK_API_KEY` are used. OAuth tokens are refreshed automatically
(under a cross-process lock) when they are within five minutes of expiry.
Model ids can be qualified as `provider/id` (e.g. `openai-codex/gpt-5.5`).

### Custom OpenAI-compatible providers

Any server that speaks OpenAI's chat completions (or the Responses API, or
Anthropic's messages API) can be added as a provider: gateways such as
[aiproxy](https://github.com/labring/aiproxy), LiteLLM and OpenRouter, or
local servers such as vLLM and Ollama. `/login custom` (in every frontend;
`prigh login custom` on the CLI) asks for

1. a name: lowercase letters, digits, `-` and `_`, not a built-in provider
   (an existing custom provider's name edits it, with every answer
   prefilled);
2. the base URL: the part before `/chat/completions`, usually ending in
   `/v1` (a pasted endpoint path is removed);
3. the API style: OpenAI chat completions (most servers), OpenAI Responses or
   Anthropic messages;
4. the API key, which may be empty for servers that need none.

It then lists `GET {base_url}/models` and reports what it found ("Found 2
models: ..."); if that fails it shows the error (connection refused, HTTP
401, ...) and offers to save anyway, change the settings or cancel. Nothing
is written until the end: the definition goes to `~/.prigh/config.json`
under `providers`, the key to `auth.json` under the provider's name. The
models then appear in `/model` as `<name>/<model id>` (e.g.
`aiproxy/gpt-4o`) and in `prigh models`; `prigh run -model
aiproxy/gpt-4o ...` works too. `/auth` lists custom providers with their URL
and key source (prigh-web's list has Edit and Remove buttons), and
`/logout <name>` asks whether to remove only the key or the provider too.

Without a stored key, `<NAME>_API_KEY` is used (the name upper-cased, `-` as
`_`: `my-proxy` reads `MY_PROXY_API_KEY`), except with `-tokens`, where keys
are never read from the environment. A `401`/`403` reply says how to fix the
key.

The config can also be written by hand; changes are picked up on the next
`/model` (or `list_models`). Per-model entries override what the server's
list says (or add models it does not list); unlisted fields default to a
128k context, 16k output, no thinking, images accepted and an unknown price
(shown as such):

```json
{
  "providers": {
    "aiproxy": {
      "base_url": "https://aiproxy.example.com/v1",
      "api": "chat",
      "headers": { "X-Team": "infra" },
      "models": [
        { "id": "gpt-4o", "context_window": 128000, "max_output": 16384,
          "images": true, "cost": { "input": 2.5, "output": 10, "cache_read": 1.25 } },
        { "id": "deepseek-r1", "name": "DeepSeek R1 (aiproxy)", "thinking": true,
          "images": false }
      ]
    },
    "ollama": { "base_url": "http://localhost:11434/v1" }
  }
}
```

- `aiproxy`: `/login custom`, name `aiproxy`, base URL
  `https://<your aiproxy>/v1`, chat completions, and an aiproxy token as the
  key (or `export AIPROXY_API_KEY=...` and leave it empty). Its models are
  then `aiproxy/<model>`.
- Ollama: `ollama serve`, then `/login custom` with name `ollama`, base URL
  `http://localhost:11434/v1`, chat completions and no key;
  `/model ollama/llama3.2`.

`api` is `chat` (default), `responses` or `anthropic`; `headers` are sent
with every request; `cost` is in dollars per million tokens. A context
window the server reports (`context_length`, `context_window` or
`max_model_len`) is used when the config gives none. Problems in the config
(a missing `base_url`, an unknown field, a model list that failed) are
shown as notices saying what to fix.

Then:

```
tui/_build/default/bin/main.exe                # interactive TUI (PRIGH_BACKEND or the dune build)
backend/_build/default/bin/main.exe run "explain this repo"    # headless
backend/_build/default/bin/main.exe sessions list          # saved sessions
backend/_build/default/bin/main.exe sessions delete ID...  # ids may be prefixes
backend/_build/default/bin/main.exe sessions prune -dry-run                        # empty sessions
backend/_build/default/bin/main.exe sessions prune -max-messages 2 -cwd /tmp       # filters: -older-than DAYS, -prompt TEXT
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

The transcript reads as a log of work: each of your prompts opens a turn
with a yellow `▌` bar and a blank line before it, and the agent's steps hang
below it. Thinking is dim italic on a `┆` rail. A tool call is one line, its
outcome marked in the gutter (`✓` done, `✕` failed, `…` running, `■`
cancelled), then its name, the argument that says what it did (`read
src/retry.ml`, `bash $ make test`, `subagent <task>`) and chips: `5 lines`,
`+4 −2` for an edit, `exit code 1`, `job j1`. Its output or diff is indented
under it.

Ctrl+O cycles the transcript verbosity:

| Level | Shows |
|---|---|
| quiet | user and assistant text (only the first line of intermediate text); tool calls and subagents collapse to their line, with failures' first output lines |
| normal | thinking (first 3 lines), tool calls with the head of their output or diff (`read` and `write` show only their line), subagent live tails |
| verbose | full thinking, tool arguments and results, every nested subagent event |

`/verbosity [quiet|normal|verbose]` sets it directly; the status line shows
`view:`.

The terminal does not draw images: each one in a prompt or a tool result shows
as a line like `[image: image/png, 34.2 KB]` (in quiet, the tool line counts
them: `✓ read shot.png  1 image`).

Subagents run in the background. A `subagent` tool call returns at once
(`started agent a1 (...)`) and the main agent's turn carries on or ends, so
you can keep talking to it. When a subagent finishes (or fails, or is
cancelled) its report is handed to the main agent as a message starting
`[subagent a1 finished]`: an idle agent starts a turn for it, a running one
gets it after its current tool calls (several finishing together arrive as one
message). The model also has `subagent_wait` (block on some or all of them,
optionally with a timeout; what it returns is not delivered again),
`subagent_status` and `subagent_cancel`. Subagents started by a subagent stay
synchronous (and cannot delegate further).

Each subagent gets its own transcript. Shift+Tab cycles focus main → agent 1 →
…, Alt+1…9 jumps, and `/agents` opens a picker (Enter focuses, Ctrl+D cancels
the highlighted one); `/agents cancel <n|id>` cancels by number or id; Esc
returns to main. Agents stay cyclable while they run and until their report
has been delivered, then until the next prompt. The status line shows an
`agents:` strip, with `bg` while agents run and the main agent is idle; a
delivered report shows as `↩ subagent a1 finished "task"` and the report's
first lines.

Long commands run as background jobs: the model calls `bash` with
`background: true` (the system prompt tells it to for anything that may take
more than about a minute: builds, test suites, deployments, servers,
watchers) and gets `started job j1: ...` back at once. The job runs on the
session's active tool host like any `bash` call (the backend, or a frontend's
`prigh tool-host`), with no default timeout; its output is kept in memory
(the last 1 MB). When it exits, a message like `[job j1 exited 0] make test`
plus the last 40 lines of output reaches the main agent exactly like a
subagent report (an idle agent starts a turn; reports finishing together, jobs
and subagents alike, arrive as one message). Killed jobs report `killed`, a
host that disconnects fails its jobs (`failed: tool host disconnected`). The
model also has `job_status`, `job_output` (peek at recent output, paging back
with `offset`), `job_wait` (block on some or all jobs; returned reports are
not delivered again) and `job_kill`. Subagents' `bash` has no `background`.

The status line shows `jobs:N` with a spinner while jobs run and the agent is
idle. `/jobs` opens a picker (id, state, elapsed time, command, last output
line): Enter prints the job's recent output into the transcript, Ctrl+D kills
it; `/jobs <id>` and `/jobs kill <id>` do the same directly. A delivered job
report shows as `↩ job j1 exited 0 "make test"` with its output tail.

Esc/abort stops the main turn only; background subagents and jobs keep
running (reports that are ready when you abort wait for your next message).
They belong to their session: `/new` or switching sessions leaves them running
in the old session (which stays live until they finish and deliver there).
Stopping the backend kills running jobs and loses running subagents; nothing
is resumed, and the session simply has no report for them. Job ids continue
after a restart (`j3` after a session that already had `j1` and `j2`).
Headless `prigh run` waits for running subagents and jobs, and the turns that
their reports start, before it exits (a server started in the background
keeps it running until the model kills it, or you interrupt it).

Typing `/` opens inline autocomplete (commands, then per-command arguments
such as models, sessions and directories). Typing `@` completes file paths
under the session's working directory; on submit the referenced files are sent as attachments and the
backend appends them to the user message as `<file>` blocks. `!cmd` runs a
shell command through the backend (streamed output shown as a tool item) and
adds it and its output to the context; `!!cmd` runs it without adding to the
context; `!&cmd` starts it as a background job (also while a turn is running)
whose exit is reported to the agent like the agent's own jobs.

Models see images. The `read` tool returns PNG, JPEG, GIF and WebP files
(recognised by content, not by name) as images the model can look at, and an
`@shot.png` attachment adds the image to the prompt; pi-web also sends images
pasted or dropped into its editor. Images over 2000x2000 px or about 3.4 MB
are downscaled with ImageMagick (`magick` or `convert`) or macOS `sips` on
the machine that reads them (the tool host); without one, images up to the
providers' hard limit (8000x8000 px) are sent as they are and larger ones
fail with a message saying how to shrink them. Anthropic and OpenAI models get
the images; DeepSeek's cannot see them and are told one was left out. Images
are stored in the session file.

Submitted prompts are kept in `~/.prigh/history` (one JSON string per line,
last 500) and loaded on start; secrets from login prompts are never recorded.
`~/.prigh/config.json` holds `scoped_models` (the models Ctrl+P cycles
through; `/scoped-models` edits it), `confirm_tools` (ask before
destructive `bash`/`write`/`edit`; `/confirm on|off`) and
`default_model`/`default_thinking` (what new sessions start with unless
`-model`/`-thinking` is given; `/change_default` saves the current ones).
Without a `default_thinking`, sessions start with thinking `on`; models that
cannot think ignore it. A fallback model keeps the session's thinking level.

`fallback_models` is a chain of models that take over from each other
(`/fallback MODEL...` sets it; names, ids and prefixes work as for
`/model`, and are saved as `provider/id` keys). When a provider says the
current model's usage allowance or balance is exhausted, or it has no
credentials, the run does not stop: the session switches to the next model
of the chain after the current one (the first if the current one is not in
it), the transcript shows `↪ handed over from A to B`, and B carries on with
the task. At the end of the chain the run stops with a note saying so
(`/model` picks another). Without a `default_model`, new sessions start on
the chain's first model. `/fallback` shows the chain and `/fallback off`
clears it. `default_cwd` (`/default-dir PATH`, `/default-dir off`) is the
directory, on the backend's host (`~` allowed), that a session starts in
when the backend starts one for a frontend, instead of the backend's
working directory; it is ignored if it is not a directory there. Like the
rest of `config.json`, both are per namespace
(`~/.prigh/namespaces/<name>/config.json` under `-tokens`).

### Slash commands

| Command | Action |
|---|---|
| `/help`, `/hotkeys` | commands and keys / keys only |
| `/model [name\|id\|provider/id]` | pick or switch the model |
| `/scoped-models` | pick the models Ctrl+P cycles through |
| `/change_default` | save the current model and thinking level as the default for new sessions |
| `/fallback [off\|model...]` | show or set the models that take over, in order, when a model's usage runs out (each word completes a model) |
| `/default-dir [off\|path]` | show or set the directory new sessions start in |
| `/login [provider\|custom] [api_key\|oauth]`, `/logout [provider]`, `/auth` | credentials; `/login custom` adds an OpenAI-compatible endpoint |
| `/thinking [off\|on\|low\|high\|max]` | set the thinking level |
| `/verbosity [quiet\|normal\|verbose]` | set the transcript verbosity |
| `/confirm [on\|off]` | ask before destructive tools |
| `/compact [instructions]` | summarise older messages to free context, focusing on the instructions |
| `/new`, `/clear`, `/quit` | new session / clear transcript / exit |
| `/name [text]` | set the session name |
| `/session` | show session statistics |
| `/sessions` | pick a saved session (Ctrl+N named-only, Ctrl+D delete) |
| `/switch [path]` | switch to a saved session |
| `/cd [path]` | change the working directory |
| `/fork`, `/rewind`, `/tree`, `/clone` | branch the session tree |
| `/export [path]`, `/import [path]` | markdown or `.jsonl` |
| `/agents [cancel <n>]` | focus a subagent (Ctrl+D in the picker cancels one), or cancel subagent n |
| `/jobs [<id>\|kill <id>]` | list background jobs (Enter shows recent output, Ctrl+D kills one), or show/kill one |
| `/host [name\|backend]` | pick where tools run, and the directory there |
| `/abort` | abort the current run |
| `/btw <question>` | ask a side question without interrupting the run |
| `/skills`, `/skill:NAME [args]` | pick a skill (Enter puts `/skill:NAME ` in the editor for its arguments) / run one |
| `/mcp [reconnect]` | MCP servers and their state: Enter approves a project's server or lists a ready one's tools; `reconnect` restarts failed ones |
| `/retry-backend-connection` | reconnect to the backend now |
| `/state` | show session state |
| `/copy` | (prigh-web) copy the last reply to the clipboard |

`/model` also accepts a display name, id, `provider/id` or unique prefix, and
`/login`/`/logout`/`/thinking`/`/sessions` open fuzzy pickers.

`/skill:` completes the names of the skills found where the session's tools
run (fetched once per session, directory and tool host), and is sent like a
prompt (a steer or follow-up while a turn runs). The transcript shows an
invoked skill as `skill NAME` followed by its arguments; verbose (Ctrl+O)
adds the skill's file. A name the backend does not know comes back as an
error listing the closest ones, with the text back in the editor. `/mcp`
also shows configuration problems, and says where to configure servers when
there are none.

`/btw` works while a turn is running: one extra model call (no tools) answers
from the session's current context, streaming into a box above the editor
that Esc dismisses (without aborting the run). The question and answer are
never added to the conversation; their cost counts towards the session's.

`-faux-script FILE` (a backend flag, passed through the TUI after `--`) plays
a JSON array of scripted provider replies, so demos and tests run without API
calls; it implies `-faux`.

Sessions are JSONL trees under `~/.prigh/sessions/`; each entry records when
it was written (sessions from before that still load, without times), and
the markdown `/export` puts each message's time (UTC) in its heading.
Project instructions are
read from `AGENTS.md`/`CLAUDE.md` files between `/` and the working
directory, plus `~/.prigh/AGENTS.md`. When the tool host has `nix` on its
PATH (as in the Docker image), the system prompt also tells the model to get
missing tools from nixpkgs (`nix shell`, `nix run`, `nix profile install`)
rather than apt or sudo. The system prompt is built once, at a session's
first run, and recorded in the session so the cached prompt prefix survives
`/cd`, `/host` and backend restarts; later directory or host changes reach
the model as a short note on the next message, leaving it to re-read the
instructions if that seems worthwhile.

### Skills

A skill is a directory with a `SKILL.md` whose YAML frontmatter has a
`name` (default: the directory's) and a `description` saying when to use
it ([Agent Skills](https://agentskills.io), as in Claude Code and pi).
They are found on the tool host in `.prigh/skills/`, `.claude/skills/` and
`.agents/skills/` of the working directory and each of its ancestors, then
the same under the home directory (the closest wins a name; skill
directories may be nested a few levels, as in `~/.claude/skills/synced/`).
The system prompt lists them with their paths, and the model reads a
skill's file when a task matches it; `disable-model-invocation: true` keeps
a skill out of that list. `/skill:NAME ARGS` invokes one yourself: the
message sent is the skill's file followed by ARGS, and an unknown name is
refused with the closest ones. `/skills` lists them.

### MCP servers

MCP servers are configured in Claude Code's `mcpServers` format, in the
tool host's `~/.prigh/mcp.json` and in a project's `.mcp.json` (in the
working directory or an ancestor; the closest definition of a name wins):

```json
{
  "mcpServers": {
    "fs": { "command": "npx", "args": ["-y", "@modelcontextprotocol/server-filesystem", "."],
            "env": { "DEBUG": "${DEBUG:-0}" } },
    "docs": { "type": "http", "url": "https://example.com/mcp",
              "headers": { "Authorization": "Bearer ${DOCS_TOKEN}" } }
  }
}
```

Stdio servers run on the session's tool host (a project's in the project's
directory, the user's in the home directory); HTTP ones use the streamable
HTTP transport. Strings may use `${VAR}` and `${VAR:-default}`. Servers
start when a run first needs them and keep running for every session on
that host; their tools are offered to the model as
`mcp__<server>__<tool>` (read-only ones may run in parallel; the others
count as destructive for `/confirm`). A project's servers run commands that
came with the project, so they only start once you approve them in `/mcp`
(the approval lasts until their definition changes; it is kept in
`~/.prigh/mcp-approvals.json`). Problems, failed starts and servers awaiting
approval are reported once each; `/mcp` shows every server with its status
and tools, and `/mcp reconnect` restarts failed ones. `-no-tools` turns MCP
off.

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

The browser frontends have their own: `tui/test-web` and
`tui/prigh-web/test` run under node in `@runtest`, and the Playwright e2es
(`tui/e2e-web`, `tui/prigh-web/e2e`, `pi-web/e2e`) drive Chromium and
Firefox against the real backend; the flake runs them as checks.

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
  `prigh-web/` (the DOM-native browser frontend), `test/`, `test-web/`,
  `e2e/`.
- `ARCHITECTURE.md` — how the pieces fit together; `DOCKER.md` — the
  container; `AGENTS.md` — conventions for working on the code.
