# prigh

A coding agent. The backend is OCaml (OxCaml, Eio) and talks to Anthropic,
the OpenAI API, ChatGPT (a Plus/Pro subscription), DeepSeek and any OpenAI- or
Anthropic-compatible endpoint. You drive it from a terminal UI, from
prigh-web in a browser (installable as an app, phone included), from a
desktop app, or from an editor over ACP. There are no plugins: tools,
subagents and background jobs are built in.

What you get:

- **One backend, many frontends.** Sessions live in the backend and keep
  running when frontends disconnect; several frontends can share one.
- **Tools run where you are.** A session's tools run on a *tool host*: the
  backend's machine, a connected laptop or a `prigh tool-host` anywhere.
- **Subagents and background jobs.** Subagents can get their own git
  worktree; long commands become background jobs, so the agent never stalls.
- **Git workflow.** Worktrees, `/diff`, a review page with line comments,
  `/commit`, `/push`, and pull requests, issues and CI on GitHub and
  Forgejo/Gitea through your sign-in (no `gh` needed).
- **Approval modes.** `ask`, `auto-edit`, `smart` (a model judges risky
  calls), `auto`, read-only `plan` and tool-free `chat`, refined by Claude
  Code-style allow/ask/deny rules.
- **Long runs.** Automatic masking and compaction, budgets, `/goal`,
  reminders, fallback models, resume after usage limits.
- **Configuration, not plugins.** Skills, MCP servers, memory, snippets,
  `AGENTS.md`/`CLAUDE.md`.
- **Multi-user** servers and opt-in **computer use**.

## Quick start

```
./prigh                 # build the backend and frontends if needed, then start the TUI here
./prigh ~/proj          # ... in another directory (or -cwd ~/proj)
./prigh -faux           # scripted provider: no API calls, no login
./prigh -web            # prigh-web: serves http://127.0.0.1:7790/ and opens it
```

The first time, log in to a provider (`prigh login anthropic`, or `/login`
inside any frontend). See [Providers and sign-in](#providers-and-sign-in).

## Install

### With Nix

```
nix profile install github:evan-a-w/prigh       # `prigh` on the PATH
prigh ~/proj                                    # the TUI
prigh -web -cwd ~/proj                          # prigh-web
prigh login anthropic                           # backend subcommands work too

nix run github:evan-a-w/prigh -- ~/proj         # without installing
```

The default package is a wrapper: `prigh [DIR]` runs the TUI, `prigh -web`
prigh-web, and the backend's subcommands (`acp`, `run`, `serve`,
`tool-host`, `web`, `desktop`, `sessions`, `login`, `logout`, `auth`,
`models`, `trace`) the backend (a directory so named: `prigh ./NAME`). It
appends its own `git` to `PATH`. Other outputs: `.#backend`, `.#frontend`,
`.#prigh-desktop` (see [Desktop app](#desktop-app)), `.#prigh-computer`
and `homeManagerModules.default`.

**Binary cache.** A first build from scratch takes about an hour, so the
flake names the cache `https://attic.caduc.co/prigh`, filled from every
push to `main`. Answer `y` when Nix asks about the flake's settings (or
pass `--accept-flake-config`). On a multi-user install, be in
`trusted-users` or add it to `nix.conf` as `extra-substituters` with
`extra-trusted-public-keys = prigh:n6wPXDyh+4CS5uoNgXQ3QvKjOSLE7ED87qIiTSvih1A=`.

### From source

```
nix develop                     # the OxCaml 5.2 toolchain for backend/ and frontend/
./prigh                         # builds both with dune, then runs the TUI
nix build                       # result/bin/prigh
```

Without Nix, `./prigh` uses an OxCaml opam switch named `prigh-ox` with the
packages of `backend/prigh.opam` and `frontend/prigh_frontend.opam` (plus
the patches in `nix/`; see `nix/README.md`).

### Docker

The image `ghcr.io/evan-a-w/prigh` runs the backend with prigh-web, with a
Linux user per prigh user:

```
export PRIGH_TOKEN=$(openssl rand -hex 32)
docker compose up -d
```

See [DOCKER.md](DOCKER.md) for modes, users, isolation, installing tools,
reverse proxies and updates.

## Terminal UI

```
prigh-tui [DIR]                                 # (./prigh, or the Nix wrapper's `prigh`)
prigh -detached ~/proj                          # in a new detached tmux session prigh-<dir>
prigh ~/proj -- -faux-script FILE               # extra backend arguments after --
```

| Flag | Meaning |
|---|---|
| `-cwd DIR`, `-session PATH`, `-model ID`, `-thinking LEVEL`, `-faux` | where, which session, model and thinking; scripted provider |
| `-connect HOST:PORT\|URL`, `-token`, `-user`, `-header` | a remote backend ([below](#remote-backends-and-tool-hosts)) |
| `-local`, `-forget` | spawn a local backend this once / forget the saved connection |
| `-tools local\|remote`, `-name NAME` | where tools run; this machine's name in `/host` |
| `-backend PATH`, `-auth-file PATH` | the backend binary (`$PRIGH_BACKEND`); credential store |

The screen is the transcript, the editor and a status line (model,
thinking, context, cost, mode, user, tool host, queued messages, agents).
Alt+L shows an agents sidebar on wide terminals.

## In a browser

prigh-web (`frontend/web/`) is the browser frontend. It is a real DOM app
(Bonsai_web, js_of_ocaml) that the backend serves, and it uses the same RPC
over a WebSocket at `/ws`, so it has every backend feature:

```
./prigh -web                          # local: serves http://127.0.0.1:7790/ and opens it
./prigh -web -faux -cwd ~/proj        # any `prigh serve` options after -web
nix run . -- -web -cwd ~/proj         # the packaged site and backend

# remote: on the server
prigh serve -web 0.0.0.0:7790 -token sekrit
# then open http://server:7790/ and give the token as the password.
prigh-tui -connect server:7790 -token sekrit -cwd ~/proj   # terminals use the same port
```

`$PRIGH_WEB_LISTEN` changes the local address. `-web-root DIR`
(`$PRIGH_WEB_ROOT`) points at the built site; the Nix wrapper sets it.

Beyond the TUI it has:

- a sessions sidebar (pinned, recent, archived; each session's worktrees
  under it; right-click or long-press for menus) and the control centre
  (Ctrl+K), one fuzzy filter over sessions, settings and commands;
- panels: a terminal on the tool host (Ctrl+\`), a file browser with
  transfers (Ctrl+Shift+F), a code editor (Alt+E), the review page with
  line comments, commit, push and pull requests (Alt+R), and subagents
  and jobs (Alt+1…9);
- a message menu (right-click) to copy, delete or trim the context, with
  undo; temporary sessions (Ctrl+Shift+O); a settings page whose
  preferences are kept per user on the backend;
- reconnection in place (draft, panels and dialogs kept) and fast loading
  of long sessions.

**Installing it as an app** ("Install app" in Chrome and Edge, "Add to
Home Screen" on a phone, "Add to Dock" in Safari): a service worker keeps
the app shell, so it opens with the backend down. Browsers allow this only
on `https://` or `http://localhost`: `./prigh -web` (with or without
`-connect`) qualifies; for a backend reached directly use HTTPS
(`tailscale serve --bg 7790`, a reverse proxy, see DOCKER.md) or
`ssh -L 7790:localhost:7790 server`. On plain `http://` elsewhere the page
works without installing, notifications or the Clipboard API.

### prigh-web for a backend elsewhere

You can serve the page from your own machine for a backend on another one.
Nothing of a backend runs locally, the session's tools run on this machine,
and you are signed in without typing the token:

```
./prigh -web -connect server:7790 -token sekrit -cwd ~/proj
./prigh -web -remote                                 # later: the saved connection
./prigh -web -connect wss://prigh.example.com/ws     # a backend behind a TLS proxy
./prigh -web -remote -tools remote                   # tools on the backend instead
```

This runs `prigh web` (`prigh web -help`): it checks the backend, serves
the site on localhost relaying the RPC, terminal and file transfers, opens
a one-time `#signin=CODE` link (the token is never printed), runs a tool
host in-process and shares the TUI's saved connection. With a `HOST:PORT`,
the terminal and file transfers need the backend's `-web` port.

## Desktop app

prigh-desktop is prigh-web in a window of its own: a small
[Tauri](https://tauri.app) app (`desktop/`). It runs `prigh desktop` while
the window is open and stops it when the window closes. `prigh desktop`
connects to the saved backend, or shows a connect form, and is this
machine's tool host, so sessions run their tools here, as with a TUI.

```nix
# Home Manager (flake input `prigh`):
imports = [ prigh.homeManagerModules.default ];
programs.prigh.enable = true;          # prigh, codex, opencode, bwrap on PATH (agents.*: claudeCode,
                                       # on when unfree is allowed; extraPackages)
programs.prigh.desktop.enable = true;  # "prigh" in the application menu
# also desktop.ui, .window, .browser (table below), .extraArgs,
# .useSystemNixpkgs, .disableDmabuf (troubleshooting), .computerUse
```

You can also install it without Home Manager:
`nix profile install github:evan-a-w/prigh#prigh-desktop` (it brings its
own prigh). The `desktop` CI workflow builds installers (`.deb`,
`.AppImage`, `.dmg`, `.msi`, `.exe`). These contain only the app, which
runs the `prigh` on the login shell's PATH, so install prigh as well.

| Setting | Values |
|---|---|
| `-ui` / `desktop.ui` | `remote` (default): the backend's own prigh-web, where you sign in once. `local`: prigh-web served by this prigh and signed in for you |
| `PRIGH_DESKTOP_WINDOW` / `desktop.window` | `auto` (default): on Linux, an installed Chromium-family browser's app window if there is one, else the webview. `browser`: always that. `webview`: the system webview (WebKitGTK, WKWebView, WebView2) |
| `PRIGH_DESKTOP_BROWSER` | which browser (Chrome, Chromium, Brave, Edge or Vivaldi) |
| `PRIGH_DESKTOP_BACKEND` | the prigh to run |
| `PRIGH_DESKTOP_ARGS` | arguments for `prigh desktop`, e.g. `-cwd '/my proj' -name laptop` |
| `PRIGH_DESKTOP_WSL_DISTRO` | Windows: the WSL distribution |

- **Windows:** the app runs `prigh desktop` inside
  [WSL](https://learn.microsoft.com/windows/wsl/install) (`wsl --install`,
  then install prigh in the distribution, e.g. with Nix).
- **Tokens:** never in Home Manager options or `PRIGH_DESKTOP_ARGS` (the
  Nix store is world-readable); sign in from the app, which saves the
  connection. Sign-in proxies (authentik, Cloudflare Access) get "Sign in
  with your browser". Unencrypted addresses to another machine need
  confirming: prefer `wss://` or an SSH tunnel.
- **Troubleshooting:** slow window on NixOS: a browser window or
  `useSystemNixpkgs` (`PRIGH_DESKTOP_GPU_INFO=1` shows WebKit's GPU page);
  blank or flickering: `WEBKIT_DISABLE_DMABUF_RENDERER=1`. The app logs to
  stderr, prefixed `prigh-desktop:`.

Build it from `desktop/`: run `nix develop .#desktop`, then `cargo tauri
dev` or `cargo tauri build` in `desktop/app`.

## Remote backends and tool hosts

The backend can run on a server and serve many sessions and frontends at
once:

```
# on the server (sessions live in ~/.prigh/sessions there)
prigh serve -web 0.0.0.0:7790 -token sekrit     # browsers and terminals on one port
prigh serve -listen 0.0.0.0:7777 -token sekrit  # or a TCP-only (JSON lines) port

# on a laptop
prigh-tui -connect server:7790 -token sekrit ~/proj
prigh-tui ~/other                         # same backend: the connection was saved
prigh-tui -connect https://prigh.example.com/ ~/proj   # behind a TLS proxy (= wss://…/ws)
```

`-connect`, `-token` and `-user` are saved in `~/.prigh/tui-connection.sexp`
(`$PRIGH_CONNECT`, `$PRIGH_TOKEN`, `$PRIGH_USER` override them, unsaved).
In the TUI, `/connect` switches backend, `/signin` and `/signout` manage
the token.

`prigh serve` speaks plain TCP and HTTP with a shared token: on untrusted
networks bind to localhost and use an SSH tunnel or an HTTPS reverse proxy
(see DOCKER.md). If a proxy wants a browser sign-in, the TUI offers to do
it through prigh-web (`prigh browser-signin -connect URL` by hand).

### Tool hosts

Each session has one active tool host. By default, a frontend connected to
a remote backend runs the session's tools on its own machine by spawning
`prigh tool-host`, so prigh must be installed there too. `-tools remote`
runs them on the backend instead.

- `/host` moves the session to another connected machine (and directory);
  the status line shows `tools:NAME`, red when it is offline. A session
  whose host goes away waits for it (calls wait up to a minute) and never
  moves on its own.
- A machine's identity is `~/.prigh/host-id`; its name is `-name`, else
  `$PRIGH_HOST_NAME`, else the hostname.
- A standalone tool host (it also serves the web UI's terminal):
  `prigh tool-host -connect server:7790 -token sekrit -cwd ~/proj`, or
  `prigh tool-host -remote` for the saved connection.
- The model can run single calls on another machine (`on_host`) or start a
  subagent there, but every such call waits for your approval, in every
  mode, until you have named that machine.

### Several users (namespaces)

```
prigh serve -web 0.0.0.0:7790 -tokens 'me=sekrit,alice=0ther'   # or $PRIGH_TOKENS
```

Each `name=token` is a user (log in with the name and token), with their
own logins, sessions, config, `AGENTS.md` and tool hosts under
`~/.prigh/namespaces/<name>/`; provider keys are then never read from the
environment. `-no-backend-host` runs nothing on the server (tools only on
hosts each user connects), `-superusers NAME,...` may act as others
(`/setusr`) and clean the Nix store (`/nix-gc`), and `-host-tokens
NAME=TOKEN,...` sign in as NAME but never as a superuser, for tool hosts.

### Engines

A backend runs sessions on the **harness**, prigh's own agent loop calling
the model's API (the default), and, when you deploy it, the **composer**,
which hands the work to a coding agent installed and signed in on the
session's tool host: the Codex CLI (`codex app-server`), Claude Code
(`claude`, stream-json), an ACP agent (`acp:opencode`, `acp:antigravity`
(`agy acp`), `acp:grok` (Grok Build), `acp:gemini`, `acp:goose`, or
`acp:mine=my-agent --acp`) or `prigh`, prigh's own harness as an adapter.
prigh keeps the session: its log (messages, tool calls, models, usage and
the cost the agent reports), naming, search, worktrees, approvals and the
UI, so a Codex or Claude Code session looks and works like any other. Their
tools run on the tool host you choose, and prigh's own tools they lack
(`render_ui`, `memory`, `sessions`, pull requests and issues, worktrees,
...) are served to them as an MCP server named `prigh`; your custom
instructions and what prigh knows about you go to them too.

```
prigh serve -engines harness,composer -adapters codex,claude
```

(or `$PRIGH_ENGINES`, `$PRIGH_ADAPTERS`; the first engine is the default for
new sessions). Users, settings and sign-ins are the same on either engine,
so one backend can run both or you can run one of each. With the composer
deployed, prigh-web's new-session screen has an engine chip, `/new codex`
(or `/new` for a picker) starts a session on an adapter, and `/engine`
switches a session to another agent (or the harness), which gets the
conversation so far quoted in a handoff ahead of your next message, as does
an agent taking over on another tool host. The model chip and status line
say which agent runs it (`Codex · gpt-5.2-codex`; a cost Codex does not
report shows as `subscription`). Codex and Claude Code sessions rewind files
too (in a git repository) and fork their thread. What an agent cannot do is
refused with what to do instead, its controls hidden. A session made on an
engine the backend does not run can still be read there.

The agent CLIs ask prigh before they act, so your approval mode, rules and
Smart mode apply to their commands and edits (Codex runs commands it knows
to be read-only without asking). A repository's `.claude` settings and
hooks are not loaded. Settings → Engines in prigh-web, and `/adapters` in
the TUI, show whether each CLI is installed and logged in on each tool host,
and what to run there when not ("Claude Code on laptop is not logged in:
run `claude auth login` there"); prigh never installs one or logs in for
you.

### Computer use

The `computer` tool drives a screen, mouse and keyboard through
`prigh-computer` (wlroots Wayland compositors, X11, Windows). It is off
unless the tool host owning the screen opts in (`prigh tool-host
-computer`, `prigh web -computer`, `prigh desktop -computer`), and the
first action of each run asks you, in every mode; moving the mouse or
pressing a key yourself ends control.

## Editors (ACP)

`prigh acp` speaks the [Agent Client Protocol](https://agentclientprotocol.com)
on stdio, so any ACP editor can use prigh as its agent. By default it runs
its own backend in-process. With `-connect HOST:PORT -token T [-user U]` it
joins a running `prigh serve` instead, so the editor's sessions are
ordinary prigh sessions. It takes `-model`, `-thinking`, `-cwd`,
`-no-tools`, `-max-budget-usd`, `-faux` and `-name`. Log in first with
`prigh login`.

Editors get sessions, prompts with files and images, tool calls with diffs,
permission requests, the modes, model and thinking options and the main
slash commands. MCP servers the editor sends are ignored: prigh uses its
own ([MCP servers](#mcp-servers)).

## Providers and sign-in

```
prigh login anthropic                  # Claude Pro/Max (browser OAuth)
prigh login anthropic -method api_key
prigh login openai-codex               # ChatGPT Plus/Pro (browser OAuth)
prigh login openai                     # API key
prigh login deepseek                   # API key
prigh login custom                     # an OpenAI-compatible endpoint (below)
prigh auth                             # status;  prigh logout PROVIDER
prigh models                           # the models you can use
```

Inside any frontend, the same commands are `/login [provider]`,
`/logout`, and `/auth`. Credentials go to `~/.config/prigh/auth.json`. A
stored credential wins; otherwise prigh reads `ANTHROPIC_OAUTH_TOKEN` or
`ANTHROPIC_API_KEY`, `OPENAI_API_KEY` and `DEEPSEEK_API_KEY`. OAuth tokens
refresh automatically. Model ids can be qualified as `provider/id`.

The file has the same shape as pi's `auth.json`. To share pi's logins,
symlink it (`ln -s ~/.pi/agent/auth.json ~/.config/prigh/auth.json`)
rather than copying it, because Anthropic rotates refresh tokens.

A provider can have several logins (accounts): `/login PROVIDER as LABEL`
adds one (prigh-web: Settings → Models, Add login). `/accounts` lists them
to use, rename, make the default, reorder or log out of (prigh-web: each
login's Manage), and `/account` picks the session's. The first stays under pi's key;
the others are `account:PROVIDER:ID`, with labels, order and the default
in `"prigh:accounts"`. `/model openai-codex/gpt-6-sol@work` pins a login
for the session. When a usage limit stops a run, prigh carries on with the
provider's next login that allows fallback (OAuth ones by default, API
keys only if you turn it on) before handing over to `fallback_models`.
A composer session on Codex, Claude Code or an ACP agent runs on that
CLI's own login, except that Codex can run on one of your ChatGPT logins in
prigh (yours or one shared with you): Settings → Engines, or `/adapters
login codex [LOGIN|own]` in the TUI. prigh hands Codex short-lived access
tokens over its stdin (never a file on the host), refreshes them, and
counts the usage on that login; the status line reads `Codex · MODEL@LOGIN`.
A Codex too old for that runs on its own login, with a notice to update it.
Claude Code can run on one of your Anthropic logins likewise: it gets the
access token on a private descriptor (`CLAUDE_CODE_OAUTH_TOKEN_FILE_DESCRIPTOR`),
not in its environment, so the commands it runs never see it; a tool host
whose prigh predates that gets it in `CLAUDE_CODE_OAUTH_TOKEN`, with a notice
to update prigh there. An API key goes in `ANTHROPIC_API_KEY`.

With `prigh serve -tokens`, a user can share one of their built-in
providers' logins with other users: `/accounts share LOGIN [with USER]`
(prigh-web: the login's Share…, a picker of the other users) and
`/accounts unshare LOGIN from USER`. The others see it as "LABEL (from
OWNER)" to pick or pin (`/account`, the model chip), or hide; they never
get its key, and what they spend on it is theirs, though its owner sees it
per user. Revoking moves their sessions back to their default login.
Host-token connections cannot manage logins, and `-no-sharing` (or
`PRIGH_SHARING=off`) turns sharing off.

### Custom OpenAI-compatible providers

Any server that speaks OpenAI chat completions, the Responses API or
Anthropic messages can be added: LiteLLM, OpenRouter, aiproxy, vLLM,
Ollama and so on. `/login custom` asks for a name, a base URL (usually
ending in `/v1`), the API style and a key (which may be empty). It then
lists the server's models. The models appear as `<name>/<model id>`
(`/model ollama/llama3.2`). Without a stored key, prigh reads
`<NAME>_API_KEY`. You can also write the definition by hand in
`~/.prigh/config.json`:

```json
{ "providers": {
    "aiproxy": { "base_url": "https://aiproxy.example.com/v1", "api": "chat",
                 "models": [ { "id": "gpt-4o", "context_window": 128000,
                               "cost": { "input": 2.5, "output": 10 } } ] },
    "ollama": { "base_url": "http://localhost:11434/v1" } } }
```

`api` is `chat` (the default), `responses` or `anthropic`. `cost` is in
dollars per million tokens. Gemini works through its chat endpoint
(`https://generativelanguage.googleapis.com/v1beta/openai`): prigh sends
each tool call's thought signature back, and the documented placeholder
for tool calls Gemini did not write (another model's history).

## Configuration

| Where | What |
|---|---|
| `~/.prigh/config.json` | settings (below); most are also set by a command or prigh-web's settings page |
| `~/.config/prigh/auth.json` | provider and git-host credentials |
| `~/.prigh/sessions/` | sessions (JSONL trees), each with a `.trace.jsonl` (`/trace`, `prigh trace`) |
| `~/.prigh/AGENTS.md`, `AGENTS.md`/`CLAUDE.md` from `/` down to the working directory | project instructions (`/init` writes one) |
| `~/.prigh/mcp.json`, a project's `.mcp.json` | MCP servers |
| `.prigh/skills/`, `.claude/skills/`, `.agents/skills/` | skills |
| `~/.prigh/history` | prompt history |

Common `config.json` keys:

| Key | Meaning | Command |
|---|---|---|
| `default_model`, `default_thinking` | what new sessions start with | `/change_default` |
| `scoped_models` | the models Ctrl+P cycles | `/scoped-models` |
| `fallback_models` | models that take over when usage runs out | `/fallback` |
| `approval_mode`, `default_chat_mode` | the mode new sessions start in | `/confirm` |
| `permissions` | allow/ask/deny rules | edit by hand; `/permissions` shows them |
| `approval_model`, `generate_model`, `naming_model`, `compaction_model` | models for `smart` approvals, drafting text, naming sessions, summaries | `/approval-model`, `/generate-model`, `/naming`, `/compaction` |
| `compact_at_tokens`, `mask_observations` | when to compact, and whether to mask old tool output | `/compaction` |
| `max_budget_usd` | default cost budget | `/default-budget` |
| `worktrees_dir` | where `/worktree new` puts worktrees (default `~/.prigh/worktrees`) | |
| `notify`, `snippets`, `ask_user_timeout_seconds`, `auto_background_seconds` | notifications, snippets, question timeout, when commands go to the background | `/notify`, `/snippet` |
| `providers`, `forges` | custom providers; git host OAuth apps | `/login custom`; see [Git hosting](#git-hosting) |

Thinking is `off`, `default` (the provider decides) or an effort level
(`minimal` … `max`). Each model offers only some of these; `/thinking`
lists them.

## Using prigh

### Prompts

| Prefix | Effect |
|---|---|
| `/command` | slash command, with completion of commands and arguments |
| `@path` | attach a file (completed on the tool host) |
| `#name` | insert a snippet |
| `!cmd` | run a shell command and add it and its output to the context |
| `!!cmd` | run it without adding it to the context |
| `!&cmd` | start it as a background job |

Enter sends a prompt, or steers the run if one is going (after its current
tool calls). Alt+Enter queues a follow-up for when the turn ends. Esc
aborts the turn. Subagents and jobs keep running.

### Keys

These are the essentials. `/help` (or `/hotkeys`) lists every binding in
each frontend.

| Key | TUI | prigh-web |
|---|---|---|
| Enter / Alt+Enter | send or steer / queue a follow-up | same |
| Esc | close dialog, back to the main agent, abort | close dialog, abort |
| Ctrl+L | pick a model | model menu |
| Ctrl+P / Alt+P | next / previous scoped model | same |
| Ctrl+T (web: Alt+T) | cycle thinking level | same |
| Alt+M | cycle mode: ask → auto-edit → smart → auto → plan → chat | same |
| Ctrl+O | cycle transcript verbosity | same |
| Ctrl+F | search the transcript | the browser's find (the page holds the whole transcript) |
| Ctrl+↑ / Ctrl+↓ | previous / next of your messages | same |
| Ctrl+G | edit the prompt in `$EDITOR` | |
| Shift+Tab, Alt+1…9 | focus a subagent | Alt+1…9: subagents panel |
| Ctrl+X | copy the last reply | same |
| Ctrl+C / Ctrl+D | clear or abort; twice to quit / quit | |
| Ctrl+K | kill to end of line | control centre |
| Ctrl+J | newline | jump to a message |
| Ctrl+\`, Ctrl+Shift+F | | terminal, file browser |
| Alt+R, Alt+E, Ctrl+Shift+O | | review page, editor, temporary session |

The TUI's editor has Emacs-style editing. In lists, typing filters
fuzzily, Enter accepts and Esc closes without changes.

### Slash commands

These are the main ones. `/help` lists them all, with their arguments.

| Area | Commands |
|---|---|
| Model | `/model`, `/thinking`, `/scoped-models`, `/change_default`, `/fallback` |
| Credentials | `/login`, `/logout`, `/auth`, `/accounts`, `/account` |
| Sessions | `/new`, `/sessions`, `/switch`, `/name`, `/fork`, `/rewind`, `/tree`, `/clone`, `/pin`, `/archive`, `/delete`, `/export`, `/import`, `/temporary` (web) |
| Context | `/compact`, `/trim`, `/delete`, `/compaction`, `/session`, `/trace` |
| Long runs | `/goal`, `/budget`, `/remind`, `/resume`, `/usage` |
| Modes | `/mode`, `/plan`, `/chat`, `/confirm`, `/permissions`, `/todos` |
| Agents | `/agents`, `/jobs`, `/queue`, `/btw`, `/abort` |
| Where | `/host`, `/cd`, `/mkdir`, `/terminal` and `/files` (web), `/default-dir`, `/default-host` |
| Git | `/diff`, `/review`, `/commit`, `/push`, `/worktree`, `/pr` |
| Project | `/init`, `/skills`, `/skill:NAME`, `/mcp`, `/memory`, `/user-info`, `/snippet` |
| Display | `/verbosity`, `/filter`, `/theme`, `/notify` |
| Connection (TUI) | `/connect`, `/signin`, `/signout` |

### Approvals and modes

| Mode | What waits for you |
|---|---|
| `ask` | destructive tools: bash, file edits, worktree changes, changes on the git host, memory writes, MCP tools that are not read-only |
| `auto-edit` | as `ask`, except that file edits run |
| `smart` | the approval model judges what `ask` would hold; edits in the session's directory run |
| `auto` | nothing (the default) |
| `plan` | read-only: edits refused, other destructive calls ask; only you leave it |
| `chat` | a conversation without the tool host |

A confirmation offers `y` (allow once), `a` (always allow, for the rest of
the session), `n` (deny) and `r` (deny with a reason). Rules in
`config.json` refine every mode. Deny rules win, then ask rules, then
allow rules:

```json
{
  "approval_mode": "auto-edit",
  "permissions": {
    "allow": ["bash(git status:*)", "bash(dune build:*)", "read"],
    "ask": ["bash(git push:*)", "edit(*.lock)"],
    "deny": ["bash(rm -rf:*)", "write(/etc/**)", "mcp__github__delete_*"]
  }
}
```

`edit` covers every file change and `read` every file read; an allow rule
must cover every part of a compound command.

### Git hosting

prigh signs in to GitHub (and Enterprise) and Forgejo/Gitea and calls their
APIs from the backend: the agent's `pull_request`, `issue` and `repo` tools
and your `/pr` use these sign-ins, so no `gh` or API credentials are needed
where tools run. Sign in with `/login github [HOST] [token]` or
`/login forgejo HOST [token]` (stored in `auth.json` as `forge:HOST`).

- **GitHub** uses the OAuth device flow, which needs your own OAuth app's
  client id (<https://github.com/settings/applications/new>, tick **Enable
  Device Flow**): `{ "forges": { "github.com": { "client_id": "Ov23li…" } } }`
  in `~/.prigh/config.json`, or `$PRIGH_GITHUB_CLIENT_ID`. Without one it
  asks for a personal access token.
- **Forgejo / Gitea** use OAuth with PKCE, with no setup on stock instances.

### Skills

A skill is a directory with a `SKILL.md` whose YAML frontmatter has a
`name` and a `description` ([Agent Skills](https://agentskills.io), as in
Claude Code and pi). Skills are found on the tool host in `.prigh/skills/`,
`.claude/skills/` and `.agents/skills/` of the working directory, its
ancestors and the home directory. The model reads a skill when a task
matches it. `/skill:NAME ARGS` runs one yourself and `/skills` lists them.

### MCP servers

MCP servers use Claude Code's `mcpServers` format. They can be defined in
prigh-web's **Settings → MCP servers** (or with `/mcp add`), in the tool
host's `~/.prigh/mcp.json`, or in a project's `.mcp.json`:

```json
{
  "mcpServers": {
    "fs": { "command": "npx", "args": ["-y", "@modelcontextprotocol/server-filesystem", "."] },
    "docs": { "type": "http", "url": "https://example.com/mcp",
              "headers": { "Authorization": "Bearer ${DOCS_TOKEN}" } }
  }
}
```

Stdio servers run on the session's tool host; HTTP ones that need OAuth
sign in from `/mcp`. Tools appear as `mcp__<server>__<tool>`. A project's
servers start only once you approve them in `/mcp`, which also shows their
state (`/mcp reconnect` restarts failed ones).

## Headless use

```
prigh run "explain this repo"          # one prompt, no UI
prigh run -session ID "go on"          # continue a saved session
prigh sessions list                    # also: delete ID..., prune -dry-run
prigh trace SESSION                    # where the time went (-perfetto out.json)
prigh serve                            # JSON-lines RPC on stdio
prigh SUBCOMMAND -help                 # each one's flags
```

## Development

The code is in `backend/` (the agent and servers, `prigh`), `frontend/`
(shared `protocol/`, `client/`, `ui/`; `tui/` and `web/`), `desktop/`
(Tauri app and `prigh-computer`), `docker/` and `nix/`. `nix develop`
gives the OxCaml toolchain; `backend/` and `frontend/` are separate dune
projects (run one dune at a time).

**Testing.** `./test` is the test suite, and the only one to run: before
pushing and before merging. It builds and tests everything, then runs a
browser smoke test and the desktop's `cargo test`, skipping whatever has
not changed (seconds to under a minute). `./test --full` adds the slow tier
and every browser e2e script in Chromium, Firefox and WebKit: run it **only
when the user explicitly asks for it**, never by default or "to be safe"
before merging.

Conventions (OCaml style, tests, pull requests, the shared machine) are in
[AGENTS.md](AGENTS.md), how the pieces fit together in
[ARCHITECTURE.md](ARCHITECTURE.md), the container in [DOCKER.md](DOCKER.md).
