# prigh

A coding agent: OCaml backend (Eio, cohttp; Anthropic, OpenAI, OpenAI
Codex/ChatGPT and DeepSeek providers) and OCaml frontends — a terminal UI
(Bonsai_term) and prigh-web, a browser UI (Bonsai_web, js_of_ocaml), both on
OxCaml. No plugins; tools and subagents are built in.

## Quick start

```
./prigh                 # builds backend + frontend if needed, then starts the TUI here
./prigh ~/proj          # ... in another directory (or -cwd ~/proj)
./prigh -faux           # scripted provider, no API calls
./prigh -detached .     # in a new detached tmux session (prigh-<dir>); returns at once
./prigh -web            # prigh-web, the browser UI (serves it on 127.0.0.1:7790 and opens it)
./prigh -web -connect server:7790 -token sekrit   # ... for a backend on another machine
```

## Build

```
# both build with OxCaml 5.2 in the one dev shell
# backend: core, eio, cohttp-eio, jsonaf
nix develop -c sh -c 'cd backend && dune build && dune build @runtest'

# frontends (the TUI and prigh-web): bonsai_term, bonsai_web
nix develop -c sh -c 'cd frontend && dune build @runtest'
# @runtest includes the e2e and tmux tests against the backend binary built above,
# and the web-layer tests (they run under node, via js_of_ocaml)

nix build               # result/bin/prigh wrapper (includes backend + TUI)

./test                  # builds both, runs every suite, the browser e2e in Chromium (~1 min)
./test --full           # ... and in Firefox: the full matrix, before merging
```

Without Nix, an OxCaml opam switch with the same packages works too
(`backend/prigh.opam`, `frontend/prigh_frontend.opam`), with the compiler patch
`nix/fix-floatarithmem.patch` and the package patches in
`nix/opam-repository` (add it as an opam repository; see `nix/README.md`).

## Running under Nix

The flake builds both executables. The default package/app is a small wrapper
that points the TUI at the Nix-built backend.

```
source /nix/var/nix/profiles/default/etc/profile.d/nix-daemon.sh   # if nix is not on PATH

nix run . -- ~/proj
nix run . -- ~/proj -faux            # scripted provider, no API calls

nix build                            # result/bin/prigh
./result/bin/prigh ~/proj

nix build .#backend                  # backend only: result/bin/prigh
nix build .#frontend                 # the frontends only: result/bin/prigh-tui, result/share/prigh_frontend/web

nix develop                          # the toolchain (OxCaml) for backend/ and frontend/
```

`prigh -web [serve options]` (`nix run . -- -web -cwd ~/proj`) runs the
backend with prigh-web, the browser frontend (`prigh serve -web
127.0.0.1:7790 -open`; `$PRIGH_WEB_LISTEN` changes the address);
`PRIGH_WEB_ROOT` points at the built site (the Nix wrapper sets it).

`prigh-tui [DIR]` starts in `DIR` (default: the current directory; `-cwd
DIR` is the same). Flags: `-faux`, `-session PATH`, `-model ID`, `-thinking
LEVEL`, `-auth-file PATH`, `-backend PATH` (same as `$PRIGH_BACKEND`),
`-connect HOST:PORT|URL`, `-header 'NAME: VALUE'`, `-token SECRET`, `-user NAME` (with `-tokens`),
`-local`, `-forget`, `-tools local|remote`, `-name NAME`,
`-detached`, `-tmux-session NAME` and `-- <extra backend args>`. The Nix
wrapper sets `PRIGH_BACKEND` to the Nix-built backend by default; set
`PRIGH_BACKEND` or pass `-backend` to override it.

`-detached` starts the TUI (with the other flags) in a new detached tmux
session and returns at once, printing how to attach:

```
$ prigh -detached ~/lix
prigh is running in tmux session prigh-lix: tmux attach -t prigh-lix
```

The session is named `prigh-<directory>` (`.`, `:` and the like become
`_`; `-2`, `-3`, ... if it is taken; `-tmux-session NAME` chooses another)
and starts in the directory. tmux is `$PRIGH_TMUX` (the Nix wrapper sets
it to its tmux, so the line names that binary) or `tmux` on `PATH`. The
session gets the caller's `PATH`, `HOME`, `PRIGH_*`, locale,
`EDITOR`/`VISUAL`, `XDG_*`, proxy, certificate and provider key
(`*_API_KEY`) variables, even from a tmux server that is already running;
other variables are the server's. From inside tmux the line says `tmux
switch-client -t ...`. Quitting the TUI closes the session; if it fails,
its error stays on screen until Enter.

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
prigh-tui -connect server:7777 -token sekrit ~/proj
prigh-tui ~/other                        # the same backend: the connection was saved
prigh-tui -session <id>                  # join a session
prigh-tui -local                         # spawn a local backend this once
prigh-tui -forget                        # forget the saved connection (and spawn)
prigh -web -remote                       # prigh-web here for the same backend (see "prigh-web for a backend elsewhere")

# a backend behind an HTTPS reverse proxy (its prigh-web URL; see DOCKER.md, "Behind a reverse proxy")
prigh-tui -connect https://prigh.example.com/ -token sekrit ~/proj   # = wss://prigh.example.com/ws
prigh-tui -connect wss://example.com/prigh/ws                        # a site under a path
```

`-connect` takes the backend's `HOST:PORT` (its `-listen` or `-web` port:
JSON lines over TCP) or its prigh-web URL: `wss://host/ws`,
`https://host/` (the same: `https://host/prigh/` is
`wss://host/prigh/ws`), `ws://` or `http://`. With a URL the TUI speaks
the same JSON lines as WebSocket text messages on the site's `/ws`, as the
browser does, over TLS for `wss`/`https`, so it can go through a
TLS-terminating reverse proxy. The server's certificate is checked against
the system's CAs (`$SSL_CERT_FILE` names another bundle). The connection
answers the server's pings and pings it when quiet, and like a TCP one it
reconnects when it drops. A proxy that wants a browser sign-in (a redirect
to a login page, a 401, an HTML page instead of the WebSocket upgrade) is
reported as such: exempt `/ws` and `/terminal` from it (prigh checks its
own token there; DOCKER.md shows how for authentik, Caddy, nginx and
Traefik), send its credentials with `-header 'NAME: VALUE'` (repeatable;
`$PRIGH_CONNECT_HEADERS`, one per line; never saved), or use an SSH tunnel
to `HOST:PORT`. The tools the TUI runs on this machine go over the same
connection.

`-connect`, `-token` and `-user` are saved in `~/.prigh/tui-connection.sexp`
(mode 0600) and used when they are not given: `-connect` to another address
replaces the saved connection (dropping its token and user), `-token` or
`-user` alone update it. `$PRIGH_CONNECT`, `$PRIGH_TOKEN` and `$PRIGH_USER`
take precedence over the saved values but are not saved. `-local` (and
`-faux`) spawn a backend without touching the saved connection.

All of that can also be done from inside the TUI, so `prigh-tui .` is
enough once a connection is saved:

- `/connect HOST:PORT` (or `/connect wss://host/ws`, any address
  `-connect` takes) switches to another backend: the old connection, its
  session, user and dialogs go (as when switching users), and the new
  address becomes the saved connection once its `hello` is answered,
  keeping the saved token and user only if it is the saved address. When
  it cannot be reached nothing changes. `/connect local` spawns a backend,
  like `-local` (the saved connection stays); `/connect` alone lists the
  current and saved addresses (Tab completes them) and, on Enter, asks for
  the address with the current one typed in. Tools follow the `-tools`
  rule as if prigh-tui had been started that way: on this machine for a
  remote backend, on a spawned one, unless `-tools` was given.
- `/signin [USER]` opens a sign-in dialog for the server: a user field
  (prefilled with the current or saved user; leave it empty for a server
  with one `-token`) and a masked token/password field (Tab, Up and Down
  move between them, Ctrl+U empties one). Enter reconnects with them and
  saves them once the server accepts them; a refusal keeps the dialog open
  with the server's error. Esc closes it and changes nothing.
- `/signout` forgets the saved token (the address and user stay, to
  prefill the dialog), drops the connection and opens the sign-in dialog.
  On a backend prigh-tui spawned there is nothing to sign in to or out of.

A server that refuses the credentials (saved, from the flags or none) at
startup, or after a reconnect, no longer stops the TUI: it shows the
sign-in dialog. Escaped, the TUI stays disconnected and the status line
says `not signed in to HOST:PORT · /signin signs in · /connect switches
server`; while connected to a remote backend it shows `server:HOST:PORT`
(`server:wss://host` for a URL) next to `user:`. `/login` and `/logout` are unrelated: they manage model
provider and git host credentials.

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
session keeps running when its frontends disconnect. `prigh serve` itself
speaks plain TCP and HTTP with a shared token (it warns when `-listen` or
`-web` is reachable beyond loopback): bind to localhost and use an SSH
tunnel or an HTTPS reverse proxy in front of `-web` (then `-connect
wss://host/ws`) on untrusted networks.

A tool host doesn't need a TUI: `prigh tool-host -connect server:7777 -token
sekrit -cwd ~/proj [-name NAME] [-host-id ID]` (`-connect` also takes a
prigh-web URL such as `wss://host/ws`, for a backend behind a TLS proxy)
connects on its own
(reconnecting when the connection drops, but exiting with what to fix
when the backend refuses its `-token`/`-user`; it is the machine's host, by
`~/.prigh/host-id`, unless `-host-id` names another) and hosts the session's tools *and* its `>_` terminal, so
a browser-only user can pick it with `/host`. The web UI's terminal always
runs on the session's active host, relayed through the backend.

### Several users (namespaces)

```
prigh serve -web 0.0.0.0:7790 -tokens 'me=sekrit,alice=0ther'   # or $PRIGH_TOKENS
```

Each `name=token` entry is a user: log in with the name as the user name
and the token as the password. The web UI's connect form asks for both
(and remembers them in the browser); `/signout` or the page's `sign out`
button forgets them and shows the form again, so someone else can log in.
The TUI and tool hosts take `-user NAME -token TOKEN` (`$PRIGH_USER`,
`$PRIGH_TOKEN`); the token alone is still accepted, so scripts need not send
a user name, but a user name that doesn't match the token is refused like a
wrong password. The TUI shows the logged-in user as `user:<name>` in the
status line; prigh-web shows it at the bottom of the sidebar and in its
settings page. Outside this mode the user name is ignored (leave it empty) and
`/signout` only forgets the saved token. In the terminal TUI, `/signin` and
`/signout` (above) sign in as someone else without restarting.

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
other: `/setusr NAME` (in the TUI and prigh-web, where the account menu's
"Act as…" does it too) switches the
connection to NAME's sessions, logins and tool hosts (the TUI's status line
shows `user:<me> as <them>`, prigh-web's sidebar and settings "acting as"), `/setusr` alone lists the users and `/setusr <me>`
switches back. Everyone else gets an error. Superusers also clean the Nix
store: `/nix-gc` shows its size and the free space and `/nix-gc run [DAYS|all]`
collects garbage, first deleting every user's profile generations older
than DAYS days (prigh-web: Settings, Nix store); in Docker through the
container's root helper (DOCKER.md, "Garbage collection"), elsewhere as
the backend's user. To everyone else (and on a backend without users)
none of this exists: no `/nix-gc`, no Settings section, and the backend
answers its methods as unknown and sends them nothing about the store. `-host-tokens NAME=TOKEN,...`
(`$PRIGH_HOST_TOKENS`) adds tokens that sign in as NAME but never as a
superuser, for tool hosts that should not hold the user's own token (the
Docker image starts one per user with one).

## In a browser

prigh-web (`frontend/web/`) is the browser frontend, in OCaml like the TUI
but built for the browser rather than mounting the terminal UI: a real DOM
(Bonsai_web, js_of_ocaml) with a session sidebar, a chat transcript with
markdown, collapsible tool calls and their images, model and thinking
selectors, and a composer that takes pasted or dropped images, `/`
commands, `@` paths, `#` snippets and `!cmd` like the TUI. Messages show when they were
sent, in the browser's time zone (`14:32`, `Yesterday 14:32`, `3 Oct
14:32`; the full date on hover): under your messages and in a reply's
footer, with a separator where the day changes. The backend serves it and
speaks prigh's own RPC protocol to it over a WebSocket at `/ws`, so it gets
every backend feature:

```
./prigh -web                      # local: serves http://127.0.0.1:7790/ and opens it
./prigh -web -faux -cwd ~/proj    # ... any `prigh serve` options after -web
nix run . -- -web -cwd ~/proj     # the packaged site and backend

# remote: on the server (or PRIGH_WEB_LISTEN=0.0.0.0:7790 ./prigh -web -token sekrit)
prigh serve -web 0.0.0.0:7790 -token sekrit
# then open http://server:7790/ and type the token into the sign-in form's
# Password field (the browser remembers it in localStorage; /signout forgets it)
prigh-tui -connect server:7790 -token sekrit -cwd ~/proj   # terminals use the same port
```

The `-web` port also accepts the terminal frontend's plain JSON-lines
connections, so one port serves browsers and terminals on the same sessions
(`-listen` is only needed for a TCP-only port). Plain `ws://` with a shared
token: bind to localhost and use an SSH tunnel or a TLS-terminating proxy
on untrusted networks.

Tool output folds to a few lines: a running command's last lines (under
how many came before), a finished one's first six (an error's last six,
where the error is), a file read or written, a listing or a diff cut
short, faded where it goes on, each with a "Show all N lines" toggle that
unfolds it in place (and "Fold to N lines", kept in view at its bottom, to
fold it again). `/verbosity verbose`
unfolds them all and `quiet` hides them; changing it, or the session,
starts the toggles afresh.

Work in progress has one sign, a small sodium lamp that breathes (still
under reduced motion): next to a running session in the sidebar, on a
running tool, subagent or job, on the status (what the agent is
doing: "Waiting for the model", "Thinking", "Writing", "Running tools", or
"Retrying in 4s (attempt 2/6): overloaded_error: Overloaded" after a
transient provider error, which the reply being streamed also shows), and on
whatever waits for the backend; nothing finished glows, and while the
connection is down only the reconnecting banner and status do.
Every action shows at once, before the backend answers: a session clicked
in the sidebar lights its lamp while the chat shows the shape of a
transcript (also while connecting, and for a new session), a prompt shows
as "Sending…" with the send button busy until the transcript has it, a
model, thinking level or approval mode chosen shows (disabled, with the
lamp) until the backend confirms it, and a command that lists something (`/login`, `/fork`,
`/tree`, `/session`, `/skills`, `/mcp`, `/setusr`…) opens its dialog with
a loading row (Esc closes it, and the late answer then opens nothing).
While the connection is down the composer is dashed and the status
says "Reconnecting…": what you write stays until it is back (sending is
refused, saying so, rather than queued).

Long sessions open fast: the page asks for the newest 60 messages only,
shows them at the end, and loads the ones before as you scroll near the
top (or press "Show earlier messages", which says how many there are),
without moving what you are reading. Tool outputs over 8 KiB come cut to
their first and last 20 lines; unfolding one loads the rest. The last
eight sessions you looked at stay in the page, so going back to one shows
it at once (then brings it up to date), and resting the pointer on a
session in the sidebar fetches its newest messages ahead of the click.
After `/compact` the session starts at the summary: what was on show
stays, with nothing older to load. A command still running in a session
you come back to (or that ran while the connection was down) keeps what it
streamed before and goes on from where it is now; its card says the output
from meanwhile shows with its result.

To get around a long session without scrolling, a rail along the chat's
right edge has a mark for each of your messages (and a brick dot for a
reply that failed, a longer rule where the context was compacted): the
one you are reading is drawn out in sodium, hovering a mark shows the
message's first line and time, and a click goes there, marking it for a
moment. Messages not loaded yet have faint marks, and jumping to one loads
the pages down to it first. The button above the rail, Ctrl+J, opens the
same messages as a list to filter (newest first; on a phone, where there
is no rail, the button floats over the chat's corner), and Ctrl+↑/↓ step
to your previous or next message, loading earlier ones past the first
loaded.

To shrink the context without summarising it, open a message's menu (yours
or a reply's): right-click it, press Shift+F10 or the Menu key on it, or
long-press it on a phone. Choose "Trim everything before this" (or press
Delete on one of your messages in the Ctrl+J list, or pick it in `/trim`);
a confirmation says how many messages and tokens it frees (Enter trims,
Esc cancels). The transcript then starts at one divider,
`✂ 142 messages trimmed · ~38k tokens  Show · Undo`:
Show unfolds the trimmed messages above it, dimmed and read-only (they are
not in the model's context), and Undo (or `/trim undo`) brings them back.
The rail draws what is trimmed as one dashed cut at its top, and the jump
list ends with a "Trimmed: 142 messages" row; both go to the divider. See
[Trimming the context](#trimming-the-context).

The same menu, on every message, has Copy (the message's text), "Trim
everything after this" and Delete; what does not apply (nothing before the
first message, nothing after the newest, anything but Copy while a run is
going) is greyed out with the reason under it. Highlighting a trim draws a
dashed line where it would cut; a right-click with text selected in the
message leaves the browser's own menu, to copy the selection. Delete takes the message out of the transcript and the model's context at
once (a reply goes with its tool calls' results; Delete on a tool call
deletes the reply that made it), and a "Message deleted · Undo" toast
brings it back (as does `/delete undo`). Trim after drops what follows a
message, up to the
newest, after the same kind of confirmation; it folds into a divider like
the one above (Show, Undo), and what you send afterwards comes below it
and stays in the context. Deleted and trimmed messages leave the rail and
the jump list.

Losing the connection (a flaky mobile network, the phone locked, the
backend restarting) changes nothing on the page: the banner sits over the
top of the chat, and once back the session's new messages are merged into
the transcript in place (keeping the older messages you had loaded),
without moving it, with the draft, attachments,
open panels and dialogs as they were. The page reconnects at once when the
network comes back or the tab is shown again (and checks a socket that
may have died while it was hidden, giving it up to a minute while answers
are still arriving on a slow network), with backoff otherwise. A
connection that keeps dropping before the session is back waits longer
each time, and after six in a row the page stops and says so ("The backend
keeps dropping the connection while loading this session"), offering
Retry now, Another session and Reload; "Reconnected" shows once the
session is back. Only the
backend refusing the saved sign-in leads to the sign-in page, saying so;
a session the backend no longer has (it restarted before saving it)
stays on show, read-only, with a banner offering a new session or the
list.

The sidebar lists the active sessions, most recently used first. Each
has its lamp (lit while its turn or its background subagents and jobs
run), its age and where it works: its repository and branch (`prigh ⎇
main`), or its directory outside a repository. A session has a repository
attached rather than living in one: sessions are not grouped by
repository. Under each session hang the worktrees that matter to it, each
a branch off a rail: those it made (`/worktree new`, the New worktree
dialog), moved into (`/cd`, `/worktree open`), checked a pull request out
in (`/pr checkout`, tagged `#7`) or made for its subagents (tagged
`subagent`), with `● 3` when one has uncommitted changes and ↑↓ commits
against its upstream; the one it works in says `here`. A worktree two
sessions use is under both; one removed (from any session, or outside
prigh) goes. On disk worktrees still go by repository
(`<worktrees_dir>/<project>/<branch>`), and `/worktree` lists all of the
repository's. The fold at the right of a session's second line (`⑂ 3`)
hides its worktrees; the folds are kept with your preferences, so they
follow you to other browsers.

A session that switched into a worktree (`/worktree open`, `/worktree
new`, `/pr checkout`, Switch session here) remembers where it came from,
its home, and its row says so quietly (`from main`, the home's branch, or
`from ~/projects/prigh`). Its menu then starts with Switch back to main
(`/worktree back`, also in the TUI, whose `/sessions` shows the home too),
which moves it back and forgets the home. Switching on from one worktree
to another keeps the first home; a `/cd` within the repository's worktrees
keeps it, and a `/cd` to the home itself or anywhere outside the
repository forgets it. The home is kept in the session, so it survives
reloads and restarts, and a fork has it too. When the home's tool host is
not connected, switching back says so; when its directory is gone,
`/worktree back main` goes to the repository's main checkout instead.

Right-click a session or a worktree (or press Shift+F10 or the context
menu key on it, long-press it on a phone, or use the ⋯ that shows while
pointing at a session) for its menu; each item names the command that
does the same. A session's: Open, Open in new tab, Rename… (`/name`),
Fork… (`/fork`), New worktree… (`/worktree new`: a branch, and a base that
defaults to the session's branch; Enter creates it under `worktrees_dir`
and the session moves there), Review changes (`/review`), Open in file
browser (`/files`), Open terminal there (`/terminal`), Copy path, Copy
session id, Archive or Unarchive (A) and Delete… (Delete). A worktree's:
Switch session here (`/worktree open`), New session here (`/worktree open
-n`), Review changes (`/review <worktree>`), Open in file browser (`/files
<path>`), Open terminal there (`/terminal <path>`, a shell of its own in
that directory), Create pull request… (the session moves there, then `/pr
new`), Copy path and Remove worktree… (`/worktree rm`; "Discard changes
and remove" when it has changes; not the one the open session works in;
Delete on a focused worktree does the same). An item on a session other
than the open one switches to it first. ↑↓ move through the menu, Enter
chooses, Esc closes it and changes nothing. Below
the list a faint "Archived 14" rule unfolds the archived sessions, muted,
with their directory. A search (Ctrl+K) finds sessions by title, prompt,
directory, repository, branch or their worktrees' branches, the archived
ones too. Where a session is in git is found on its
tool host whenever its directory changes, by reading the repository's
files (no git command), and kept in the session; a tool host running an
older prigh cannot tell, and its sessions show their directory. On a focused session A archives or unarchives it and Delete
deletes it. "Select" turns the list into
checkboxes: click or Space marks a session, Shift+click or Shift+Space the
range from the last one (across the divider), and Delete (or the bar at
the foot of the list, "Delete 3 sessions (12.4 MB)") asks to delete the
marked ones; "Delete archived" under the archived ones
(`/delete-archived`) every archived one. The confirmation says how many sessions and how much disk they use, and
which ones the backend will keep (running, open in another tab or terminal,
or the one you are in): those are not asked for, and with nothing left to
delete the dialog only says why (Close). `/archive`, `/unarchive` and `/delete` also take
sessions by name, id or path. The list follows the backend as it changes; the worktrees of the
repositories the sessions' worktrees belong to are listed (for their
changes and to drop removed ones) when the session, its directory or its
tool host changes, when a session's worktrees change, when a turn ends
and after creating or removing one.

Subagents and background jobs have their own panel, a resizable column
right of the chat (drag its left edge; a full-screen sheet on a phone):
the session's subagents as a tree (nested ones under the background agent
that started them), running first: each row is its task, the lamp while it
runs (with its live elapsed time, current tool, model and turns) or how it
ended (done, failed with the error, or cancelled, interrupted by a restart
or killed, shown quietly), and the background jobs the same way with their
last line. Selecting one shows a subagent's live transcript at the panel's
width or a job's output, with what it is pinned under it however far you
scroll: its task (and who started it), how it stands or ended, its model,
turns and time, Cancel while it runs (Kill for a job) and "Show in chat",
which scrolls to its card. It opens from the status ("2 agents running"), the
robot button at the chat's top right, a subagent's card in the chat, `/agents [n|id]` and `/jobs
[id]`, and Alt+1…9 (the Nth listed; Alt+] and Alt+[ step through them, Alt+0 closes
it, Esc goes back from outside the editor). Unlike the TUI, Shift+Tab is
left to the browser (it moves the focus back), and on macOS Option+digit
types its character instead. What finished before your latest prompt
folds under "Earlier"; switching sessions empties the panel. `/agents cancel
<n|id>` and `/jobs kill <id>` open the item there and stop it.

A running background subagent can be steered like the main agent. Showing
it in the panel leaves the composer on the main agent; its Steer button (in
the panel's footer, next to Cancel) makes it who the composer writes to: a
"Steering a1 · task" chip over the editor says so, Enter sends a steer (it
reaches the subagent after its current tool calls) and Alt+Enter a
follow-up (when it ends its turn, before it reports). That lasts while you
browse the panel or close it, until Esc in the editor, the chip's × or the
button again ("Steering") write to the main agent again. Unlike the TUI,
where focusing a subagent is itself the mode that steers it, viewing one in
prigh-web only reads it.
What waits for a subagent is listed above the composer with the main
agent's ("To a1, next step"), with Edit and remove, and Alt+↑ takes back the
last message for whoever the composer writes to. Delivered messages show in
its transcript as "You (steer)" / "You (follow-up)". A subagent that has
finished, or one nested in another, takes no messages: refusals say what to
do and keep your text in the editor, and when the subagent being steered
finishes the composer goes back to the main agent. `/agents steer <n|id>
TEXT` and `/agents follow-up <n|id> TEXT` send one without switching.

`-web-root DIR` (`$PRIGH_WEB_ROOT`) points at the site;
without it the backend looks next to its dune build and in
`../share/prigh/web`. The Nix package (`nix run . -- -web`)
installs the release build of the page (whole-program js_of_ocaml, about
2 MB of JavaScript, sent as a 500 KB brotli or 590 KB gzip file it
ships precompressed); reloads revalidate with `ETag`/`Last-Modified` and
cost a `304` per file. A dune build in the dev profile makes a ~50 MB one
with inline source maps, uncompressed, quicker to relink but slow to load. The page keeps `?session=` in the address bar
and the chat's scroll position and draft in the tab's sessionStorage (a
reload, or a phone bringing back a discarded tab, returns to the same place,
waiting for the backend if it is out of reach), reconnects when the
backend goes away (see above). It connects to its own origin by default;
`?backend=ws://host:port/ws` points a page served from one place at a
backend elsewhere, `?session=ID` joins a session and `?name=` names the
frontend in `/host`. Tools run on a tool host (a browser cannot host
them). In Docker it is `PRIGH_MODE=web` (port 7790, see `DOCKER.md`).

The terminal button at the chat's top right (or `/terminal`, or Ctrl+`) opens a
shell where the session's tools run (the active `/host`, in its
directory; `/terminal DIR`, or a worktree's "Open terminal there", a shell
of its own in DIR) in a panel under the chat: drag its top edge to resize it (the
height is remembered), a full-screen sheet on a phone. Keys typed in it
are the shell's, Esc included; Ctrl+` closes it (so does its ×) and
gives the keyboard back to the editor. It is a tmux session (on the
backend's host it needs `tmux`; the nix package brings its own), so closing
the panel, reloading the page (which reopens it) or losing the connection
keeps the shell; it is killed after 10 minutes with no page attached, when
it exits, or when the backend stops. It follows the session, `/host` and
the user you act as; when the shell exits, "New shell" starts another,
and when there is none (no tmux on the host, a host that has gone) the
panel says why and what to do.

The folder button at the chat's top right (or `/files [path]`, or Ctrl+Shift+F)
opens the file browser, a drawer right of the chat (a full-screen sheet on
a phone) on the machine where the session's tools run (the backend, the
Docker container, or a laptop connected as a tool host), in the session's
directory. The breadcrumbs (or the pencil, to type a path: absolute, `~/…`
or relative to the directory shown), the up arrow and Backspace move
around; the eye shows dot files; the columns sort by name, size or age
(directories first). A file clicked (or reached with ↑↓) is previewed
in a pane of its own left of the list, between it and the chat: text
with line numbers (the first 256 KB), images (up to 20 MB), binaries
only named. Its close button (or Esc) leaves the list alone again. On a
phone, with the drawer floating over a narrow window, or without room for
both beside the chat, the preview takes the list's place, with a back
arrow to it. The selected entry's actions: Attach (to your next message),
Download (a directory as a `.tar.gz` archive: `tar -czf` of it, paths
starting with its name), Mention (`@path` into the composer), Copy path,
Rename (F2) and Delete (Delete; it asks first, and refuses `/` and the
home directory). The upload button (or files dropped on the drawer)
uploads into the directory shown, with progress, asking before it
replaces a file; New folder makes one. Every error says what to do next,
dialogs own the keyboard (Enter accepts, Esc cancels without doing
anything), and switching session or host starts the browser afresh.

In a git repository the browser marks what changed (against `HEAD`,
untracked files too): a changed file has a dot (a sage tick once you
approved it, sodium when it changed after that), a directory the number
of changed files inside. A changed file previews as its diff, the hunks
or the whole file with the changes inline, highlighted; any text file of
the repository can be commented on.

Files go into a message too: drop them on the composer, paste them, or
pick them with its paperclip. Images (PNG, JPEG, GIF, WebP) go as images,
as before; any other file is uploaded to `.prigh/uploads/` in the
session's directory on its tool host (created with a `.gitignore` of `*`,
so git ignores it; a name in use gets `-2`, `-3`…) and attached by path,
like an `@file`: the model gets text files in `<file>` blocks and a
binary file's path and size, to inspect with its tools. Each shows as a
chip with its name, size and upload progress (× removes it) until it is
sent, and in the transcript as an `@path` chip. A message waits for its
uploads; one that failed says to remove it or attach it again. While
files upload (or a download is being prepared), the status says so
with the work lamp: "Uploading 2 files 40%".

Transfers go through the backend, so they work on any tool host: the page
asks over its signed-in connection for a one-time link (a random ticket,
valid 5 minutes, for that session's host only), then `GET
/files/download?ticket=…` or `PUT /files/upload?ticket=…` on the same port
streams the bytes, in chunks, to or from the host (uploads up to 2 GB). A
tool host running an older prigh says to update prigh there.

GitHub and Forgejo (Codeberg, or any self-hosted Forgejo or Gitea; setting
them up: [Git hosting](#git-hosting)) are in the settings page's Git hosting
section (also under `/auth`): each host
with who is signed in and how, Connect (GitHub's device code shows big,
with Copy and the page to open; Forgejo opens its authorize page), Use a
token, Disconnect, and Add host… for another Forgejo; closing the sign-in
goes back to the settings. `/login github [HOST] [token]`, `/login forgejo
HOST [token]`, `/login HOST` and `/logout HOST` do the same, and the
`/login`/`/logout` pickers list the hosts too. The branch in the session's
card at the foot of the sidebar (or `/pr [list open|closed|all]`) opens
the session's repository's pull requests: a list to filter by state and by typing; Enter opens one (`/pr
view N`) with its state, author, branches, checks, description and diff,
"Open on GitHub" and Check out (its branch in a worktree, as `/worktree`
makes them, with "Work there" to `/cd` into it; `/pr checkout N` asks the
same), Esc going
back to the list. New pull request (`/pr new [title]`) is a form
prefilled from the branch (title, description, base, draft, and "push
first" when the remote lacks commits): Ctrl+Enter creates it, a refusal
stays in the form with what you typed, and "#25 created: URL" says it
worked. Errors are the backend's, saying what to do (`/login github`, `git
remote add`…).

Every panel resizes from a handle on its inner edge: the sessions
sidebar's right edge; the left edges of the file browser's list, of its
preview and of the agents panel; the terminal's top edge; on the review
page, the edge between it and the chat beside it and the right edge of
its list of changed files. The handle shows as a faint line under the pointer and turns
sodium while it is dragged or focused; double-click it (or press Enter on
it) for the default size. It is a focusable separator: Tab to it, then
the arrow keys move it (Shift: four times as far), Home and End make the
panel as small or as large as it goes. The chat always keeps 400px (while
reviewing, the review page keeps 480px beside the chat, and its code
320px beside its file list, and the sessions sidebar steps aside, so the
file browser and the agents panel share what is left with them; the
terminal then spans the review and the chat): when the window narrows,
the side panels beside it shrink in proportion (down to their minimums) and grow back to
the sizes you chose when it widens again. The sizes are kept with your preferences on the backend (below), so
every browser you sign in on has them. On a phone the panels are sheets,
without handles.

Code is highlighted: fenced code blocks in replies (by the fence's
language, also while the reply streams), the file browser's text preview
(by the file's extension or name) and diffs (edits, patches, `/diff`: the
code on each line, old and new side each in its own context). It covers
OCaml, JavaScript/TypeScript, Python, Rust, Go, C/C++ (and Java, C#),
shell, JSON, YAML, TOML, Markdown (with its fenced code), HTML/XML (with
script and style), CSS, Nix, SQL, dune/s-expressions and diff, in the
theme's quiet colours: muted keywords, sage strings, faded comments, a
little sodium in types and numbers. A preview past 64 KiB is highlighted
up to there (the rest plain, with a note), so a large file opens without
a pause.

TeX math in replies is typeset (KaTeX, bundled with the site, so it works
offline): `$..$` or `\(..\)` inline, `$$..$$`, `\[..\]` or a ```` ```math ````
block for display, never inside code. Prices and shell variables stay text
(`$5 and $10`, `US$3`, `$HOME`; `\$` is a dollar). Math in the theme's
text colour, and display math too wide for the column scrolls sideways.
TeX KaTeX cannot read shows as written, tinted, with its error on hover;
math still streaming shows as written until it closes. Copying a
selection gives the math's TeX; the message menu's Copy, the reply as
written. The TUI shows math as written.

prigh-web has every slash command of the table below, as pickers and
dialogs rather than lines in a transcript (a dialog keeps the keyboard
until Esc closes it: Tab goes round it, the page behind is inert). A
command it refuses (a typo, an unknown model) stays in the editor for you
to correct, its error saying what to do; errors stay until dismissed (×),
the next message or command you send, or switching session. Choosing a
model whose provider is not logged in says which `/login` to run.
Enter accepts the completion popup's highlighted item; Ctrl+Enter (⌘+Enter
on a Mac) sends the composer's text exactly as typed instead, and Esc
closes the popup.
`/session` is a dialog of the
session's details and statistics; `/fork`, `/rewind` (with a confirmation)
and `/tree` (the whole tree, branches indented) pick a message;
`/scoped-models` is a checklist (Tab or a click checks a model, Enter
saves); `/cd`, `/mkdir`, `/host`'s directory, `/export` and `/import` ask for a path
with completions from the backend and keep the dialog open, with the error,
when the backend refuses it (a directory that doesn't exist yet opens a
"Create folder?" dialog instead); `/agents` and `/jobs` open the agents panel
(above); `/btw` answers in a panel above the composer (Esc closes it);
`/verbosity` (Ctrl+O cycles) hides tool output and thinking (quiet) or
unfolds everything (verbose); `/filter`, or the funnel button left of the
files', opens a menu under it that shows or hides whole kinds of the
transcript (↑↓, Space or a click toggles at once, Enter or a click outside
keeps, Esc puts back what it was; "Show all" resets): the button is accented
with how many kinds are hidden, and a line over the transcript says what is
hidden, how many items, with "Show all". Each session has its own, kept
in the session, so switching sessions switches the filter, and it stays
across reloads, browsers and forks of the session; the jump rail and
Ctrl+J still list your messages when they are hidden, and land where they
are. A hidden message's menu (copy, trim before or
after it, delete) goes with it, the dividers of trims stay, and the count
leaves out what is being deleted; `/copy` (Ctrl+X with nothing selected) copies
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
restarts the failed ones and reports. `/snippet` (Alt+N) is a picker of
your snippets whose Enter inserts the text at the caret (see Snippets
below). `/fallback` completes a model for
each word, in order (or `off`), and shows the backend's error, with the
closest models, for one it doesn't know; `/default-dir` completes
directories on the backend's host, where new sessions start, and takes a
relative path from the backend's directory. Notes the harness added (a
new tool host, plan mode turned on or off, a hand-over to the next fallback
model, a goal continuation, delivered background work, the compaction
summary) are margin notes in the transcript (`↪ Handed over from A to B`),
each expandable to the text the model saw, and `/fork` and `/tree` label
them the same way. `/diff` is a dialog of the changed files (each folds,
with its line counts); `/notify on|off`, the settings page and the
account menu control browser notifications while the tab is hidden, and the
title (`(2) prigh`) and favicon count what happened meanwhile.

The composer is one box: the editor on top and, along its bottom edge in
the box's own quiet text, the model, the thinking level ("Thinking high",
"No thinking" for a model without levels) and the approval mode ("Asks:
edits & commands", "Asks: commands", "Auto: never asks", "Plan mode",
lit), each lit with a chevron when pointed at, then attach, stop and send
on the right. A chip opens a small menu standing on it, above the composer
(below it only when there is no room above), with its command's choices
(`/model`, `/thinking`, `/mode`), the current one checked: the models under
their providers with their context windows and a filter field to type in
(the list scrolls; filtered, best match first), the thinking levels with
how hard each thinks, the modes with what each asks first. ↑↓ move, Enter
or a click takes the choice, Esc or a click beside the menu closes it
unchanged and gives the editor back the focus; ← →, Tab and a click on
another chip open the next chip's menu, one at a time. On a phone the menu
is as wide as the composer. The commands themselves open the larger
picker dialog. Ctrl+L opens the model's menu, Alt+T and Alt+M cycle the
other two; in a narrow composer the labels get short
(`high`, `ask`, `plan`, after an icon). There is no bar over the chat
while the sidebar is open: the files, terminal and agents buttons sit
quietly in the chat's top right corner (lit while their panel is open),
and the session's status is in a card at the foot of the sidebar, above
the account, under the tree of projects. The tree marks which session it
is (its row has the cwd as it is now, relative to its project or worktree,
and a rename button). The card is two quiet lines on a grid: what the agent
is doing, with whatever applies across from it (the goal, reminders, a
scheduled resume, queued messages to take back, background agents and
jobs, transfers, the verbosity when not the default, the plan chip `plan
2/5: step` that runs `/todos`); then the context's meter (click for
`/session`'s details), the tokens and the cost to the cent (its tooltip
has it exactly; or the budget). The verbosity is muted, and while the
agent works its background agents are too, without a lamp, so that one
thing at a time is lit. Under them, when there is something to say, the branch
(click for the pull requests, `/pr`) and where tools run, when there is
another host to choose (click for the `/host` picker). Each part
keeps its place, so nothing moves as a turn starts and ends. Your account
is under it, with the settings gear; the account menu also has Settings
and Commands and keys (`/help`). With the sidebar closed, and always on a
phone (where the sidebar is a drawer), a slim bar over the chat keeps the
essentials on one line: the sidebar's button and the title, what applies
(with the tool host when it is not the backend), the status, the panel
toggles and the settings gear. What applies comes before the status, and
the activity has a slot of its own width, so that nothing moves the
status; a narrower chat column drops the tokens and cost, then says what
applies with icons alone, then shows the activity as a dot; the bar never
changes height.

The settings page (the gear beside your account at the foot of the
sidebar, Settings in the account menu, `/settings` or Ctrl+,) holds what is not
about the session: the theme and accent, notifications (on or off, and
what to do when the browser has not been asked, blocks them or cannot show
them), transcript detail (`/verbosity`), tool confirmation (`/confirm`),
the providers (`/auth`), who you are signed in (and acting) as with Switch
account and Sign out, and the keyboard shortcuts. Esc closes it; its
controls keep it open.

Themes: `original` (graphite with the warm sodium accent, the default),
`dark` (neutral greys: the accent is the only colour but diffs' and
outcomes' green and red) and `light`. Every theme takes an accent: orange
(the original's), blue, green, purple, pink, teal, yellow or any colour.
The settings page shows each theme as a small picture of the page with
your accent on it, the accents as swatches and a colour picker for any
other, which previews as you drag it; `/theme [original|dark|light]` and
`/accent [name|#rrggbb]` set them, and without an argument are pickers
that show what is highlighted (Esc puts yours back). Washes, lines, focus
rings, the work lamp, code highlighting and the terminal follow the
accent and the theme.

Your preferences live on the backend, per user (`~/.prigh/prefs.json` in
your home, or your namespace's with `-tokens`): the theme and accent,
notifications, transcript detail, the panels' sizes, whether the terminal
is open, and the prompt history, which the TUI shares. Every browser you
sign in on gets them, and a change in one shows at once in the others
(the history too). The browser keeps a copy so that a reload paints your
theme before the backend answers, and for a backend that keeps none. A
browser that kept them before the backend did uploads its own when it
first signs in (unless the backend already has the user's). What stays in
the browser: the saved sign-ins (needed before there is a user to ask), and
per tab the draft and where the chat was scrolled. Acting as another user
(`-superusers`) you keep your own preferences, which are not saved on the
backend meanwhile. Steers and follow-ups sent while the agent works wait above
the composer, each tagged "Steers this turn" or "After this turn", with
Edit (back into the editor) and remove buttons; one leaving the queue says
so ("Steer delivered: …") when it reaches the transcript. A tool confirmation is a dialog with a
button per answer (y, a for always with the rule it adds, n, r for a
reason typed in the dialog, Esc going back to the buttons), the plan of
`exit_plan_mode` rendered as markdown; the model's questions are a dialog of
radio buttons or checkboxes with an "Other" field (keys as in the TUI, or
clicks). Ctrl+P/Alt+P cycle the scoped
models, Alt+T the thinking level (Ctrl+T is the browser's), Alt+↑ takes
back a queued message, Ctrl+↑/↓ jump between your messages and Ctrl+J
lists them (Delete there trims the context before one); `/help` and
`/hotkeys` also list the TUI's keys that the browser keeps (Ctrl+C, Ctrl+Z, Ctrl+F, Ctrl+R, $EDITOR's Ctrl+G)
and what to use instead, and `/quit` says to close the tab.

The account at the bottom of the sidebar (also in the settings page) opens the
account menu: switch to another saved account in one click (the page
reloads signed in as it, in its last session), add an account (the sign-in
form, keeping the others; it may be on another backend), sign out of this
one, and, for `-superusers`, act as another user and come back. The
accounts (user, token, backend, last session) are kept in localStorage
(`prigh-web.accounts`); the active one is also in `prigh.user`/`prigh.token`,
and the sign-in page lists them.

If the backend goes away (the spawned process dies, or the TCP connection
drops) the TUI reconnects on its own — immediately, then with exponential
backoff capped at 10s — rejoining the same session and respawning the
backend when it was spawned; `/retry-backend-connection` retries at once and
Ctrl+C quits meanwhile. Tool calls its local tool host is running carry on
across the reconnect: their results wait for the backend to be back.

### Reviewing the changes

`/review` (or Alt+R, or the Review button of the file browser and of
`/diff`) opens the review page in the chat's place; the chat narrows to a
column beside it and keeps working (drag the edge between them to share
the width differently). Reviewing is a focused mode: the sessions sidebar
steps aside (Ctrl+B shows it as a drawer over the page, Esc or a click
beside it hides it again) and the session's status moves to the bar over
the chat. The terminal (Ctrl+`) spans the bottom under both the review
and the chat, and the file browser and the agents panel open beside them
as usual. Closing the review brings the sidebar back as it was.

Down the review page's left are the changed files,
each with its approval; the strip in its head shows them all at a glance
(sage approved, sodium changed since approved). The current file shows
its hunks or, with Whole file (v), all of it with the changes inline, one
line number per row (a removed line's old one, struck through).
`]` / `[` (or n / p, j / k) go to the next and previous file, `a`
approves the file (again), Esc closes. It follows the agent: every edit,
write or command of the agent or its subagents, and every run's end,
reads the changes again; the arrow button reads them now.

Select the code you want changed, as you would to copy it (drag across
it, within a line or over several; on a phone, press and hold), here or in
the file browser's preview of a changed file: a Comment button floats by
the selection. Press it (or `c`), write what should change and Enter adds
it (Shift+Enter for a new line, Esc drops it), with the selected text
quoted. On a window wide enough, comments sit in a margin beside the code,
each level with its first line (or just below the one above it), so the
code never moves for them; drag the margin's edge to widen it. Narrower,
and on a phone, they show under their lines. The selected text stays
marked while the comment is there, and pointing at either the comment or
its code lights the other. Removed
lines can be commented on too: the comment says it is on the old text.
(A line's number selects the whole line, Shift+click to another.)
Comments collect until you send them (Send, or `s`): the agent then gets
one message with each comment's file, lines, the code they hold with the
selection marked in it and quoted, and the thread (`✎ 2 review comments`
in the transcript), and answers each with the `reply_to_comment` tool.
Its replies show threaded under the comment; a reply may propose a change
(exact replacements, like `edit`'s), which Accept applies to the file
where the tools run and Reject declines, and the agent hears of either
with the next comments you send. Reply in a thread to go on with it,
Resolve it when it is done, which folds it to a small mark on its line
(click it to unfold the thread), or delete it. A comment keeps its code's
text, so when the file changes around it it moves with it (and, when
only the rest of its lines changed, with the selected text); those whose
code is gone are counted in one folded line above the file ("2 outdated
comments", click to list them), as are those on lines the view does not
show.

Edit (e) switches to the file's text: edit it in place (highlighted) and
Save (Ctrl+S) writes it on the tool host; a file that changed since you
opened it is not overwritten (save again to overwrite it, or Reload).
Approving keys each file by a hash of its change, so a file approved and
then changed is no longer approved, and says so. Comments and approvals
are kept with the session (also across rewinds, reloads and restarts).
In the TUI, `/review` alone lists the changed files with their approvals
and the comments with their threads.

Each thing you do to a file in the review (approve or unapprove it,
comment, reply, accept or reject a proposal, save it in Edit) records the
file as it was then, so you can see just what changed in it afterwards:
next to Changes and Whole file, "Since latest (comment · 14:32)" (`l`)
shows the file then against the file now, in the same view (select its
current lines to comment on them). A file that changed after your last
action opens in that view, and so does one you have just commented on,
replied about or decided a proposal of: until something changes it shows
the changes under "Nothing changed since your comment on line 6 · 14:32",
then what changed since. The file list says by how much (`+3 −1 since
approved`). Latest follows your newest action on the file as you act; the
chevron beside it (or Shift+L) opens a menu (filtered as you type) of the
earlier ones, named by what you did where ("Comment on line 6 · 20:59",
"Approved · 20:41", "Edited · 20:30"), to compare with one of them
instead, and Latest to follow again. The file browser's preview of a
changed file has the same view, and the TUI's `/review` says `changed
since approved (+3 −1)`.
The texts are kept with the session's review, each distinct content once,
read where the tools run (so remote tool hosts too); files over 1 MiB,
binary files, and new contents once a session keeps 32 MiB of them record
only a digest: the review then says that the file changed, not how. A
branch reviewed without checking it out (below) has no such view.

Any worktree of the repository can be reviewed, not only the session's,
and compared with anything. The review page's head reads "Reviewing
BRANCH (PATH) against BASE (merge base SHA)", and both are pickers (click
them, or `r` and `b` on the page): menus under the line that filter as you
type, ↑ ↓ and Enter choose, Esc closes.

- **Reviewing**: the repository's worktrees (the session's marked), then
  its branches, local and remote, that are not checked out. Such a branch
  is reviewed read-only, from its commits (`git diff BASE...BRANCH`; it has
  no uncommitted or untracked changes): comments work and are sent naming
  the branch, but Edit and accepting a proposed change are disabled, and
  "Check out into a worktree" checks it out (as `/worktree new` does,
  recorded with the session; a remote branch as its local branch) and
  reviews it there.
- **Against**: `HEAD` (the uncommitted changes only), local branches,
  remote-tracking branches (`origin/main`), tags, the last 20 commits of
  what is reviewed (short id and subject), or any revision typed (the last
  choice of the menu); one git does not know is refused with the closest
  names, and the page stays on what it showed.
- **Since merge base** (the default, as a pull request shows it) or
  **Directly** (`d`): the changes since the branch left the base, or every
  difference from the base as it is now ("against origin/main directly").

`/review WORKTREE` (its path, branch or directory name, as `/worktree`
lists them; completed) and the worktree picker's Alt+R open another
worktree; `/review BRANCH --base REV [--direct]` a branch not checked out
(the completion offers them, and remote branches and tags after
`--base`). A linked worktree (a subagent's, a feature branch, a `/pr`
checkout) is compared by default with its base: the merge base with the
branch it was made from (recorded when prigh created it, as git config
`branch.<branch>.prighBase`), else the remote's default branch
(`origin/HEAD`), else `main` or `master`, so its commits show as well as
what is uncommitted and untracked. The main checkout is compared with
`HEAD`, as before. prigh-web remembers the last comparison of each
worktree or branch and what each worktree's sessions reviewed last (in
your preferences, `web.review`), and opens there next time. Comments and
approvals are kept per worktree (or branch); comments sent to the agent
name the worktree and each file's absolute path, and accepted proposals
and the editor's saves go to that worktree's files. The page reads the
reviewed worktree again whenever the agent or a subagent edits. In the
TUI, `/review` with the same arguments lists the files and comments under
what was compared ("review feat/x (not checked out) against main
directly").

#### Committing, pushing and pull requests from the review

At the page's foot, away from Send comments, the git bar shows the reviewed
worktree's branch, how
it stands with its upstream (`↑2 ↓0 origin/feat-x`, or "not pushed yet")
and how many files are staged, unstaged and untracked. Each file with
uncommitted changes has a box in the file list: approved files come ticked
(drawn hollow until git has them: they are staged when you commit), staged
ones are ticked, a file only partly staged is half-ticked. Ticking a file
stages it (`git add`), unticking a staged one unstages it, and Stage approved
stages every approved file's changes. Under the list, the commit box takes
the message: ✦ Generate (Alt+G in it) drafts it with the model from the
staged changes (the ticked files are staged first), the session's goal and
plan and the repository's recent commits (their style) and commit guidance;
it streams in, Stop or Esc keeps what came, Undo puts back what was there.
Amend changes the last commit instead (an empty message keeps its own).
Commit (Ctrl+Enter in the box) commits the ticked files; the bar says what
it did (`Committed 1a2b3c4 Fix the parser on feat/x`) and the page reads the
changes again. Push pushes the branch to its upstream, setting one (on
`origin`, else the only remote) when it has none, with the tool host's own
git credentials: prigh never asks for a password. Create PR opens the
pull-request dialog for that worktree's branch (see [Git hosting](#git-hosting)).
Stage approved, Push and Create PR are at the bar's end.
A branch reviewed without checking it out has no git bar, boxes or commit
box: check it out into a worktree to commit there.
Refusals say what to do: nothing ticked, an empty message, a detached HEAD
(`git switch -c NAME`), a push the remote rejected (pull first, e.g. `git
pull --rebase`), no remote (`git remote add origin URL`), missing
credentials or committer identity on the tool host. Paths are checked to
stay inside the worktree. In the TUI, `/commit` and `/push` do the same
(see [Git hosting](#git-hosting)).

### prigh-web for a backend elsewhere

The page can also be served from your own machine for a backend on another
one, like `prigh-tui -connect`: nothing of a backend runs locally (no
sessions, no provider logins), the session's tools run on this machine, and
you are signed in without typing the token.

```
# on the server
prigh serve -web 0.0.0.0:7790 -token sekrit
# on the laptop: serves the site on 127.0.0.1:7790 (or the next free port) and opens it
./prigh -web -connect server:7790 -token sekrit -cwd ~/proj
./prigh -web -remote                                 # later: the saved connection
./prigh -web -connect wss://prigh.example.com/ws     # a backend behind a TLS-terminating proxy
./prigh -web -remote -tools remote                   # tools on the backend, no local tool host
```

`./prigh -web` with `-connect` or `-remote` runs `prigh web` (see `prigh web
-help`); without them it is a local backend as before. It:

- checks the backend first: one that cannot be reached, a URL with no
  prigh-web, or refused credentials stop it with what to fix (`-connect`,
  `-token`, `-user`);
- serves the site locally (`-web-root`/`$PRIGH_WEB_ROOT` as for `serve
  -web`; `-listen HOST:PORT` or `$PRIGH_WEB_LISTEN` to choose where) and
  relays everything else to the backend: the RPC socket, the terminal and
  file transfers. The page only talks to its own origin, so a `wss://`
  backend behind TLS is fine from an `http://localhost` page;
- signs the page in: the URL it opens (and prints) ends in
  `#signin=CODE`, a one-time code the page trades for the token and user
  (a fragment is never sent in a request, so it is in no log, and the
  token is never printed). The page then remembers them as for a sign-in;
  the link is spent, run `prigh web` again for a new one;
- runs a tool host to the backend in-process, as `prigh tool-host
  -connect` would (`-name`, `-cwd`, `-host-id`; the machine's id from
  `~/.prigh/host-id`), and the page names it in its `hello`: its sessions
  move to this machine and new ones start here, as from a TUI.
  `-tools remote` leaves the tools on the backend;
- reuses the TUI's saved connection (`~/.prigh/tui-connection.sexp`, and
  `$PRIGH_CONNECT`, `$PRIGH_TOKEN`, `$PRIGH_USER`) by the TUI's rules, so
  `prigh-tui` and `./prigh -web -remote` go to the same backend; it is
  saved once the check passed (a URL too: prigh-tui connects to those as
  well).

Ctrl+C stops the site and the tool host. With a `HOST:PORT`, either of
the backend's ports carries the RPC, but the terminal and file transfers
need its `-web` port.

## In Neovim

[`editors/nvim`](editors/nvim/README.md) is prigh.nvim, a Neovim client
speaking the Agent Client Protocol to `prigh acp` (locally, or `prigh acp
-connect HOST:PORT -token T` for a remote backend): a chat split with
streamed answers, tool cards and permission cards, the agent reading and
editing your buffers (unsaved text included), and `:PrighReview` to accept or
reject its changes hunk by hunk.

```lua
vim.opt.rtp:append("~/src/prigh/editors/nvim")
require("prigh").setup({ cmd = { "prigh", "acp" } })   -- then :Prigh
```

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
frontend/_build/default/tui/bin/main.exe       # interactive TUI (PRIGH_BACKEND or the dune build)
backend/_build/default/bin/main.exe run "explain this repo"    # headless
backend/_build/default/bin/main.exe run -session ID "go on"     # continue a saved session (id, unique prefix or file)
backend/_build/default/bin/main.exe sessions list          # saved sessions, the archived ones last under "archived:"
backend/_build/default/bin/main.exe sessions delete ID...  # ids may be unique prefixes; nothing is deleted if one matches none or several
backend/_build/default/bin/main.exe sessions prune -dry-run                        # empty sessions
backend/_build/default/bin/main.exe sessions prune -max-messages 2 -cwd /tmp       # filters: -older-than DAYS, -prompt TEXT
backend/_build/default/bin/main.exe serve      # JSON-lines RPC on stdio
```

### The screen

The terminal UI is the transcript (starting with `prigh in DIR · /help · Esc
aborts · Ctrl+C twice quits`), a gray rule, the editor, which grows with
multi-line input up to a cap and then scrolls, and the status line (below).
Alt+L shows an agents **sidebar** left of the transcript on terminals of
120 columns and more (it is off by default, and the choice holds for the
run): the main agent, the subagents and the background jobs, each with a
dot (green running, yellow waiting for you, red failed, gray done or idle)
and a dim line of what it is doing; the focused agent is marked.

Pickers, the completion popup, confirmations, questions, the sign-in and
login dialogs, `/btw`, the queued messages and long command output
(`/help`, `/diff`, `/stats`, `/mcp`, job output, ...) take the bottom of
the transcript's area, just above the rule, the transcript's end showing
above them: opening or closing them
never moves the editor, the cursor (unless typing goes into them) or the
status line, and only the editor's own growth resizes the transcript. Long
output is a box that scrolls with Up/Down/PageUp/PageDown; Esc or Enter
closes it, and typing closes it into the editor.

Scrolled back (PageUp, the wheel, Ctrl+Up), the transcript stays where you
put it while new output arrives, and a **jump indicator** sits at its
bottom right, just above the rule and any open dialog, like prigh-web's ↓
button: `↓ bottom · End` until something arrives below, then `↓ new output`
while text streams in and `↓ 3 new` once messages, notices or tool results
are complete (in yellow; the key in gray). It names the key that goes
back: End on an empty prompt, Ctrl+End otherwise (End then moves the
editor's cursor). That key, a click on the indicator, Esc when nothing
runs, or scrolling down past the end go back to the bottom and follow new
output again.

Acknowledgements (`view: verbose`, `copied 120 chars`, `model: x`, usage
errors, `cancelled`, a failed command) are not transcript content: they show
on the right of the rule (in a box above it when too long) until the next
key press, Esc, or a few seconds pass. What belongs to the session's history
stays in the transcript: your messages, the agent's replies, tool calls and
results, the agent's errors, tool approvals (`allowed bash`), the backend's
notices (compaction, hand-overs, retries), a lost connection and a session
that no longer exists.

### Keys

| Key | Action |
|---|---|
| Enter | send the prompt (to a focused subagent: steer it); accept the highlighted item (for command arguments only once you have typed a filter or moved the highlight — `/model` Enter Enter opens the picker) |
| Alt+Enter | queue a follow-up to run after the current turn (a focused subagent's: before it reports) |
| Alt+S | send exactly what is typed, ignoring the completion popup (`/cd newdir` while it highlights an existing directory); Esc then Enter does the same |
| Ctrl+J / Alt+J | insert a newline |
| Esc | close the dialog, go back to the main agent from a subagent, abort the running turn, or scroll back to the bottom |
| Tab | accept the highlighted completion (a command, argument, `@path` or `#snippet`); on an empty prompt, list the commands |
| Up / Down | move up/down (editor line, history, or list row) |
| Alt+Up | pop the last queued steer/follow-up (the focused agent's) back into the editor |
| Left / Right | move the cursor |
| Alt+B / Ctrl+Left | move back one word |
| Alt+F / Ctrl+Right | move forward one word |
| Alt+D | delete the next word |
| Home / Ctrl+A | start of line; on an empty prompt, the transcript's top |
| End / Ctrl+E | end of line; on an empty prompt, back to the bottom of the transcript |
| Ctrl+End / click on `↓ … · End` | back to the bottom of the transcript, following new output again |
| PageUp / PageDown | scroll the transcript / list a page |
| mouse wheel | scroll the transcript (selecting text needs Shift) |
| Ctrl+Up / Ctrl+Down | jump to the previous/next user message (or the trimmed divider) |
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
| Alt+M | cycle the mode: ask → auto-edit → auto → plan (read-only) |
| Alt+N | pick a snippet to insert at the cursor (`/snippet`) |
| Ctrl+N | picker: toggle the named-only / logged-in-only filter |
| Tab / Alt+A / Alt+D | `/sessions`: mark a session / archive or unarchive it / delete every archived one |
| Ctrl+X | copy the last assistant message |
| Ctrl+Z | suspend to the shell |
| Shift+Tab | cycle subagent focus: main → agent 1 → … → main |
| Alt+1…9 | focus agent N |
| Alt+L | show or hide the agents sidebar (off by default; 120 columns and more) |
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

In lists (pickers) typing filters fuzzily, Enter accepts and Esc closes
without changes; Ctrl+D deletes sessions, cancels a subagent, kills a job or
removes a reminder,
and `/scoped-models` toggles with Space (Ctrl+A all, Ctrl+X none). `/help`
lists the commands, the prompt prefixes (`!cmd`, `!!cmd`, `!&cmd`, `@path`, `#name`)
and these keys.

`/sessions` lists the active sessions first, most recent first, each with
its repository and branch (`prigh ⎇ main`) and, beside its directory, the
worktrees that matter to it (`⎇ feat/x · ⎇ agent/fix (subagent)`; the
filter matches these too), and then, after an
`── archived ──` divider and dimmed, the archived ones. `▶` marks a running session (its
turn, or background subagents and jobs: `2 in background`), `●` one loaded
in the backend and `*` the one you are in; the list follows changes made
anywhere (other frontends, other sessions). In it Tab marks or unmarks a
session (`✓`; the status line shows how many and their size), Ctrl+D
deletes the marked sessions (or the highlighted one), Alt+A archives or
unarchives the highlighted one, Alt+D deletes every archived session, and
Ctrl+N shows only named ones (marks on the hidden ones stay). The session
you are in cannot be deleted, so Ctrl+D and `/delete` say so at once and
Alt+D leaves it out. Tab after `/switch`, `/archive`, `/unarchive` or
`/delete` completes sessions by name (or first prompt), the active ones
first, and inserts the id. Deleting always asks first, with the count and
the disk space (`Delete 3 sessions (1.2 MB)? Enter deletes, Esc keeps
them`); Esc goes back to the list unchanged. Afterwards a notice says what
was deleted and why any session was not (it is running, open in another
frontend, or the one you are in).

The status line is quiet gray: cwd, model, `think:`, `view:`, the context
(`ctx:12%/200k`, the share of the model's window; yellow from 50%, red from
80%), cost (`$1.20/$5.00` against a budget: yellow from 80%, red once
spent), and then whatever is going on — the goal (`goal:3/20`,
`goal:done`, `goal:stopped`), the session's reminders (`remind:2`), a
scheduled resume (`resumes 14:05`),
`queued:N`, `to answer:N` (confirmations and questions waiting behind an
open dialog, in yellow), `retry:2/6` (the model failed transiently and is
asked again; the transcript notes why), agents, jobs, the tool host — with
the current mode's keys and what the agent is doing (`⠋ thinking · Esc
aborts · Enter steers`, with `waiting for the model`, `writing`, `running
tools` or `retrying in 4s`; a picker's keys) in yellow. The order is fixed;
at narrow widths whole parts give way, the least important first, and the
model is never cut. `think:` and `mode:` are always there (at narrow widths
they outlast the cost and the cwd; `view:` gives way to the cwd first).
Queued steers and follow-ups (whichever frontend queued them) are listed
above the rule, one per line, until they are delivered: Alt+Up takes the
last back into the editor, and `/queue` picks any of them (Enter edits it,
Ctrl+D removes it). An unknown command
goes back into the editor as typed, the error naming the closest command
(`/modle gpt`: did you mean `/model`?), and a failed `/switch` or `/cd` puts
the command back with what to try next.

Ctrl+O cycles the transcript verbosity:

| Level | Shows |
|---|---|
| quiet | user and assistant text (only the first line of intermediate text); tool calls and subagents collapse to their line, with failures' first output lines |
| normal | thinking (first 3 lines), tool calls with the head of their output or diff (`read` and `write` show only their line), subagent live tails |
| verbose | full thinking, tool arguments and results, every nested subagent event |

`/verbosity [quiet|normal|verbose]` sets it directly; the status line shows
`view:`.

`/filter` hides whole kinds of the transcript, for reading only: the model's
context is unchanged. The kinds are your messages (`user`), the model's
replies (`assistant`), thinking, tool calls with their output (`tools`),
subagents with their work (`subagents`) and the harness's notes (`notes`:
errors, how a reply stopped, compactions, hand-overs, reports of background
work); the dividers where messages were trimmed (before or after a message) always show. Bare `/filter`
opens a checklist (Space toggles, Enter applies, Esc leaves it as it was);
`/filter tools` shows only the tool calls, `/filter -thinking -tools` hides
those, `+thinking` shows it again and `/filter all` shows everything. While
it hides something the status line says what (`filter: −thinking −tools`).
The TUI keeps it in the backend's preferences (`tui.transcript-filter`), so
it lasts across runs and sessions; prigh-web keeps one per session, in
the session (below). `/jump` still lists every message of
yours, hidden or not.

The terminal does not draw images: each one in a prompt or a tool result shows
as a line like `[image: image/png, 34.2 KB]` (in quiet, the tool line counts
them: `✓ read shot.png  1 image`).

Subagents run in the background. A `subagent` tool call returns at once
(`started agent a1 (...)`) and the main agent's turn carries on or ends, so
you can keep talking to it. When a subagent finishes (or fails, or is
cancelled) its report is handed to the main agent as a message starting
`[subagent a1 finished]`: an idle agent starts a turn for it, a running one
gets it after its current tool calls (several finishing together arrive as one
message). The model also has `subagent_status`, `subagent_cancel` and
`subagent_message` (tell a running subagent something, which it reads at its
next step); there
is no tool to block on a subagent, since its report wakes the main agent
anyway. Subagents started by a subagent stay synchronous (and cannot
delegate further).

With `fork: true` a subagent starts from its caller's conversation so far
instead of a blank one, so it needs little exploration: the model is told to
fork for work that builds on what it explored (implementing what it
investigated) and not for independent work (a review, a second opinion, a
fresh investigation). The fork gets a result for every call of the
caller's last message (for its own call: that it is the fork; for parallel
calls: that the caller runs them) and its task, framed as a forked subagent's
whose caller carries on separately. When it uses its caller's model and all
its tools, it is sent the caller's system prompt and tool list unchanged, so
the provider's prompt cache covers the whole shared conversation; the tools
it may not use at its depth (a subagent's background controls, `subagent`
for a subagent's subagent, the main agent's plan and questions) refuse with
the reason. With another model or a `tools` subset it has its own system
prompt and tools. In a `worktree`, it is told that the paths it inherited
are its caller's. Its card says `forked`.

A subagent that runs out of turns (`max_turns`, 50 by default) keeps its
context: its report (`[subagent a1 out of turns]`) says to call
`subagent_continue` with its id, optionally `max_turns` (default: its
previous limit) and a `message` from its caller, to have it carry on where
it stopped under the same id, with a fresh turn budget; its card, row and
transcript run again, its next report arrives as usual and its turns and
cost add up over its runs. Cancelling it (`subagent_cancel`) or resetting
the session forgets it. A synchronous subagent's caller can continue its own
in the same way (it blocks until the subagent reports).

A subagent can work in a git worktree of its own, when the model (or you,
asking it) sets the `subagent` tool's `worktree` option: a branch name, or
`auto` for one named after the task (`agent/fix-the-parser-bug`, `-2` when
taken). The worktree is made like `/worktree new` does, from where the
session is (in the same subdirectory), and the subagent works there; its
report ends with where the work is (`[worktree: the work is on branch
agent/fix-the-parser-bug, checked out in ...]`) so that you can review it,
merge the branch or remove the worktree. The session records the worktree
until it is removed: prigh-web shows it under the session, tagged
`subagent`, and `/worktree` lists it like any other (open it, or start a
session there, to look at the work).

The model makes worktrees itself with the `worktree` tool (main agent and
subagents alike): `create` checks a branch out at the standard place, like
`/worktree new` (an existing branch as it is, else a new one from `base`),
and the session records it (a subagent's as that subagent's, tagged
`subagent`); the model works there by its path (`cd` in bash, a
subagent's `cwd`). `list` shows the worktrees with their branch and
changes, and `remove` takes one away with `/worktree rm`'s rules: never
the main checkout or the worktree the agent or the session works in, and
one with uncommitted changes only with `force`. The system prompt tells
the model to use it rather than `git worktree add`. Should it run `git
worktree add` (or `remove`, `move`, `prune`) in bash anyway, prigh
compares the repository's worktrees before and after the command: new
ones are recorded on the session (one outside `worktrees_dir` gets a note
telling the model to use the tool next time) and removed ones forgotten.

Each subagent gets its own transcript. Shift+Tab cycles focus main → agent 1 →
…, Alt+1…9 jumps, and `/agents` opens a picker (Enter focuses, Ctrl+D cancels
the highlighted one); `/agents cancel <n|id>` cancels by number or id; Esc
returns to main. Agents stay cyclable while they run and until their report
has been delivered, then until the next prompt. The status line shows an
`agents:` strip, with `bg` while agents run and the main agent is idle; a
delivered report shows as `↩ subagent a1 finished "task"` and the report's
first lines.

You can steer a running subagent like the main agent. With it focused, the
editor talks to it (its placeholder and the status line say so: `→ a1`):
Enter sends a steer, which it reads at its next step (after its current tool
calls), Alt+Enter a follow-up, which it reads when it would otherwise report,
and Alt+Up takes the last queued one back; its queue is listed above the
rule as the main agent's is, and `/queue` edits or removes from it. Slash
and `!` commands still go to the app. `/agents steer N TEXT` and `/agents
follow-up N TEXT` send without focusing. Delivered messages show in its
transcript as `→ You (steer)` and in the main one as a quiet `→ a1: text`.
A subagent that has finished takes no more messages: the error says to tell
the main agent instead, and the text stays in the editor. The main agent can
message its subagents too (`subagent_message`).

Long commands run as background jobs: the model calls `bash` with
`background: true` (the system prompt tells it to for anything that may take
long: builds, test suites, installs, deployments, servers, watchers) and
gets `started job j1: ...` back at once. The job runs on the
session's active tool host like any `bash` call (the backend, or a frontend's
`prigh tool-host`), with no default timeout; its output is kept in memory
(the last 1 MB). When it exits, a message like `[job j1 exited 0] make test`
plus the last 40 lines of output reaches the main agent exactly like a
subagent report (an idle agent starts a turn; reports finishing together, jobs
and subagents alike, arrive as one message). Killed jobs report `killed`, a
host that drops its connection keeps its jobs if it reconnects within a
minute still running them (a TUI or `prigh tool-host` that reconnects does),
otherwise they fail (`failed: tool host disconnected`). The
model also has `job_status`, `job_output` (peek at recent output, paging back
with `offset`), `job_input` and `job_kill`; like subagents, jobs have no
tool to block on them.

Subagents have jobs too, their own (`j1`, ... per subagent): `bash` with
`background` and the job tools work the same. Finished jobs' reports reach
a subagent between its turns, and when it ends its turn while some of its
jobs still run it is woken by the next report like the main agent: it
finishes only when it ends a turn with no jobs running. Jobs still running
when it stops otherwise (cancelled, out of turns) are killed, which its
report says (`[killed when the subagent finished, still running: job j2
(sleep 60)]`). Its system prompt tells it never to sleep or poll: foreground
commands for results it needs (one blocking command to wait for something
to become ready), jobs for commands side by side, and to kill the jobs it
no longer needs (servers) before its final report. The main agent's prompt
tells it never to sleep or poll either, but to keep working or end its turn
(`job_output` shows a job's output without blocking). `prigh run` likewise
waits for the agent's jobs and subagents, and the turns their reports
start, before it exits.

The main agent never stalls on a long command.
`auto_background_seconds` in `config.json` (default 30; `0` turns it off)
is how long a foreground `bash` call of the main agent may run: one still
running then is moved to the background without being restarted. It
becomes the next job (`j3`), keeping its output so far, its process and
the tool host it runs on, and the call returns at once with `still
running after 30s, so it was moved to the background as job j3 ...` and
the last lines of output; from then on it is a job like any other
(`job_output`, `job_kill`; `job_input` says its stdin is closed), and its
report arrives when it exits. A command given no `timeout` then has none,
like a job; one with a `timeout` keeps it. The time counts from when the
command starts, not while a confirmation waits for you. The system prompt
and the tool descriptions tell the model to keep working or end its turn
rather than wait. Subagents' own foreground `bash` calls are never moved.
The tool card shows `job j3` and
`moved to the background after 30s` with the output it had then.

A job started with `stdin: true` gets a pipe for its stdin that stays
open, so the model can drive interactive programs: `job_input {id, input,
close?, wait_ms?}` writes `input` to it (closing it afterwards with
`close: true`) and returns what the job printed in response, waiting until
the output goes quiet, the job exits or `wait_ms` (default 1000, at most
30000) pass. The stdin is not a terminal, so programs that buffer or behave
differently need flags (`python3 -u -i`, `stdbuf -oL`). It works on remote
tool hosts too (`tool_exec_input`); a host running an older prigh refuses
with a message saying to update it. Other jobs (and `!&cmd`) read end of
input at once, like foreground commands, so one that reads stdin
unexpectedly does not wait forever; `job_input` to them says to start the
job again with `stdin: true`.

The status line shows `jobs:N` with a spinner while jobs run and the agent is
idle. `/jobs` opens a picker (id, state, elapsed time, command, last output
line): Enter prints the job's recent output into the transcript, Ctrl+D kills
it; `/jobs <id>` and `/jobs kill <id>` do the same directly. A delivered job
report shows as `↩ job j1 exited 0 "make test"` with its output tail.

Esc/abort stops the main turn only; background subagents and jobs keep
running (reports that are ready when you abort wait for your next message).
A foreground command is killed by an abort until it has moved to the
background; after that it is a job and keeps running.
They belong to their session: `/new` or switching sessions leaves them running
in the old session (which stays live until they finish and deliver there).
Stopping the backend kills running jobs and loses running subagents; nothing
is resumed, and the session simply has no report for them. A tool call that
was running then gets an `[interrupted: ...]` result when the session next
runs, telling the model to check what it did, so the conversation stays
valid for every provider (old sessions broken this way are repaired the
same way when they are sent). Job ids continue
after a restart (`j3` after a session that already had `j1` and `j2`).
Headless `prigh run` waits for running subagents and jobs, and the turns that
their reports start, before it exits (a server started in the background,
or run in the foreground and moved there, keeps it running until the model
kills it, or you interrupt it).

Typing `/` opens inline autocomplete (commands, then per-command arguments
such as models, sessions and directories), and `#name` completes snippets
(see Snippets). Typing `@` completes file paths
under the session's working directory; on submit the referenced files are sent as attachments and the
backend appends them to the user message as `<file>` blocks. `!cmd` runs a
shell command through the backend (streamed output shown as a tool item) and
adds it and its output to the context; `!!cmd` runs it without adding to the
context; `!&cmd` starts it as a background job (also while a turn is running)
whose exit is reported to the agent like the agent's own jobs.

`write` does not clobber files the model has not looked at: replacing an
existing file is refused unless the model has read or written that file in
this session and it has not changed since (by content), with an error that
says to read it first, or that it changed since it was last read (and how
long ago) and must be read again. `apply_patch`'s `*** Add File:` over an
existing file and `*** Move to:` onto one are refused the same way. Edits
(`edit`, `apply_patch` updates) match text exactly, so they are not
blocked, but their result notes when the file changed since the model last
saw it. The record of what was seen lives on the tool host, per session
(shared with its subagents); a restarted tool host or a switch to another
host has none, so the model is asked to read the file again.

The web: `web_fetch {url, timeout?}` fetches an http(s) URL on the session's
tool host (so `localhost` is that machine) and returns readable text: HTML
becomes markdown-ish text (headings, lists, `[links](url)`, code blocks,
tables; scripts, styles and navigation dropped, only `<main>`/`<article>`
when the page has one), other text types come as they are, images are
attached like `read`'s. It follows up to 10 redirects, reads at most 5 MB,
times out after 30 s (`timeout` up to 120), truncates long pages like
`bash` output (the full text saved to a file under `$TMPDIR/prigh-output-<uid>/`, private to the user)
and says what to do on errors (HTTP status, refused connections, timeouts,
binary types). `web_search` is the provider's own server-side search:
Anthropic's `web_search` tool (at most 5 searches per turn) and the OpenAI
Responses API's `web_search` (OpenAI and ChatGPT/Codex). The provider runs
the searches; they show in the transcript as `web_search "query"` with the
results folded underneath, and are replayed to the same provider as it sent
them (to another provider as text). Models of other providers (DeepSeek,
custom endpoints) and OpenAI's models without it (`gpt-4`, `gpt-4.1-nano`,
`o1`, `o3-mini`, ...) do not get `web_search`; they can still `web_fetch` a
search page. Neither tool is destructive, so neither asks for confirmation
in the `ask` approval mode.

Models see images. The `read` tool returns PNG, JPEG, GIF and WebP files
(recognised by content, not by name) as images the model can look at, and an
`@shot.png` attachment adds the image to the prompt; prigh-web also sends
images pasted or dropped into its composer. Images over 2000x2000 px or about 3.4 MB
are downscaled with ImageMagick (`magick` or `convert`) or macOS `sips` on
the machine that reads them (the tool host); without one, images up to the
providers' hard limit (8000x8000 px) are sent as they are and larger ones
fail with a message saying how to shrink them. Anthropic and OpenAI models get
the images; DeepSeek's cannot see them and are told one was left out. Images
are stored in the session file.

Submitted prompts are kept in `~/.prigh/history` (one JSON string per line,
last 500) and loaded on start, and on the backend with your preferences
(above), shared with prigh-web: the backend's history replaces this
machine's once it answers (a backend that has none gets this machine's),
and prompts sent from another frontend arrive as you type. Secrets from
login prompts are never recorded.
`~/.prigh/config.json` holds `scoped_models` (the models Ctrl+P cycles
through; `/scoped-models` edits it), `approval_mode` (`ask`, `auto-edit`
or `auto`: the mode new sessions start in, see below; `/confirm on|off`
sets `ask` or `auto`; files with the older `confirm_tools` still work, and
it is replaced by `approval_mode` when prigh saves the file),
`permissions` (allow/ask/deny rules, below; edited by hand) and
`default_model`/`default_thinking` (what new sessions start with unless
`-model`/`-thinking` is given; `/change_default` saves the current ones),
`notify` (`off`, `bell` or `desktop`; the TUI's `/notify`),
`worktrees_dir` (default `~/.prigh/worktrees`, on the tool host: where
`/worktree new` puts git worktrees, below),
`ask_user_timeout_seconds` (default 300, `0` = never: how long a question
from the model waits for you, above), `snippets` (texts to insert into the
editor, `/snippet` below) and
`auto_background_seconds` (default 30, `0` = never: when a foreground
command moves to the background and how long a wait lasts, above),
`generate_model` (the model ✦ Generate drafts commit messages, pull
requests, branch names and session names with; default: the session's),
`mask_observations` (default `true`: replace old tool outputs with
placeholders before summarising, see "Long runs" below),
`compact_at_tokens` (default 200000, `0` = off: compact once the context
exceeds this many tokens, if that comes before 80% of the window) and
`compaction_model` (the model that writes compaction summaries; default:
the conversation's), all three set with `/compaction` (below).
Without a `default_thinking`, sessions start with thinking `on`; models that
cannot think ignore it. A fallback model keeps the session's thinking level.

`fallback_models` is a chain of models that take over from each other
(`/fallback MODEL...` sets it; names, ids and prefixes work as for
`/model`, and are saved as `provider/id` keys). When a provider says the
current model's usage allowance or balance is exhausted, or it has no
credentials, the run does not stop: the session switches to the next model
of the chain after the current one (the first if the current one is not in
it), the transcript shows `↪ Handed over from A to B`, and B carries on with
the task. At the end of the chain the run stops with a note saying so
(`/model` picks another). Without a `default_model`, new sessions start on
the chain's first model. `/fallback` shows the chain and `/fallback off`
clears it. `default_cwd` (`/default-dir PATH`, `/default-dir off`) is the
directory, on the backend's host (`~` allowed), that a session starts in
when the backend starts one for a frontend, instead of the backend's
working directory; it is ignored if it is not a directory there.
`archive_after_days` (default 7, `0` = never) archives a session
automatically once its last message is that many days old: the session
lists show it as archived (frontends put archived ones apart). Archiving
or unarchiving by hand is recorded in the session file; a new message
unarchives it, an unarchived one stays active for another
`archive_after_days`, and a session is never archived while a run or a
background subagent or job of it is going on. Deleting a session (one, a
selection, or every archived one) removes its file and its directory
(`~/.prigh/sessions/<stamp>_<id>/`, where per-session data is kept) and
kills its terminal; it is refused for a session that is running, open in
another frontend, the one you are in or the backend's `-session`. Like the
rest of `config.json`, these settings are per namespace
(`~/.prigh/namespaces/<name>/config.json` under `-tokens`). Mistakes in the
file are reported (at `prigh run`'s start and as notices in the UIs) rather
than silently ignored: invalid JSON or a setting of the wrong type (prigh
then ignores the file's settings), an unknown setting (with the closest
known one), and a `default_model` that names no known model.

### Long runs: compaction, budget, goals, usage limits

Context never runs out mid-task. The first, cheap tier is observation
masking: once the context passes 5/8 of the compaction threshold (half of
the model's window, when it compacts at 80%; below), the outputs of
tool calls older than the last 10 turns are replaced with a one-line
placeholder (`[bash output omitted to save context: 412 lines, 18 KB; run the
tool again if you need it]`), and so are large arguments of old tool calls
(a `write`'s content, an `apply_patch` body); the model's reasoning, its
calls and your messages stay, and so do errors and outputs under 2 KB. It
masks in batches, only when that frees at least 1/8 of the threshold (a
tenth of the window; also past the point where it would summarise), so the
provider's prompt cache is not invalidated every turn, and the session
records it, so a resumed session sends the same context. The transcript
keeps showing the full outputs, with a note saying what was masked;
`/compaction mask off` (`mask_observations: false` in `config.json`) turns
it off.

When the context still crosses 80% of the model's window between the turns
of a run (the last response's tokens plus the tool results since, less what
masking freed), the older part of the conversation is summarised and the run
carries on with the summary (the transcript shows `Context compacted`); the
same happens after a run, and on `/compact` (which always summarises). When
a provider rejects a request as too long anyway (Anthropic's "prompt is too
long", OpenAI's `context_length_exceeded`, and the wordings of DeepSeek and
other OpenAI-compatible servers), old tool outputs are masked, if there are
any, and the request sent again; then the conversation is compacted and the
request sent again, keeping less the second time; only then does the run
fail, saying what to do. A compaction keeps the end of the conversation from a
turn boundary, so tool calls keep their results, and carries your
instructions over: when it falls in the middle of the work on your latest
message, that message is kept verbatim ahead of the summary, and your
earlier messages are too, long ones cut to their first 2000 characters,
the newest first up to about 6k tokens. The summary has fixed sections
(goal and constraints, decisions, failed approaches with their exact
errors, files, current state, next steps), updates the previous one rather
than summarising it again, and ends with the files read and modified,
recorded from the tool calls that succeeded (absolute paths, the 100 most
recent of each). It is written by the model from the
conversation as the agent sends it (masked), in a request that shares the
session's prompt cache; a conversation too long for that is sent as a
shortened transcript instead. Subagents mask and compact their own context
the same way.

On models with very long windows, 80% is far past where answers start to
degrade, so compaction also happens once the context exceeds
`compact_at_tokens` (default 200000; `/compaction at 150k`, or `off` for
80% only); masking then starts relative to it too (a 1M-window model
capped at 200k masks past 125k tokens). `compaction_model` (`/compaction
model MODEL`, `off` for the conversation's model) has a cheaper model
write the summaries, at its own price; it is used only when its window
holds what is summarised. It is sent the same request as the
conversation's model would be (its messages, system prompt and tools,
thinking only if it supports it, else a transcript when that does not
fit), but, being another model, it does not reuse the conversation's
prompt cache. `/compaction` alone shows all three settings. The system prompt also suggests handing
wide exploration to subagents, so the main context stays small.

#### Trimming the context

`/trim` drops everything before a message, one of yours or a reply, from
the model's context, without a summary: pick the message (newest first, each with its
time and the messages and tokens trimming there frees), confirm, and the
next request starts at it. The session file keeps everything: the
transcript folds the trimmed part into one divider (`──── 142 messages
trimmed · ~38k tokens ────`; `/trim show` unfolds it read-only in the TUI,
Show in prigh-web), and `/trim undo` brings back what the latest trim took
out (trims stack: each undo cancels one). The context use in the status line
goes down by the estimate at once. The cut is never between a tool call
and its result (trimming before a reply keeps its tool results; the
request then starts with a short note that earlier messages were removed,
as models expect the user to speak first), and it is refused while a
run is in progress (wait for it to end or abort it with Esc). It works with
compaction both ways: a `/compact` after a trim summarises, and carries
over, only your messages after it, and a trim after a compaction can start
at one of the messages it carried over, or cut into what it kept, dropping
those messages and the summary too. A fork or clone keeps the trim, and so does
`/rewind` while the trimmed-to message stays on the path (rewinding to
before it drops the trim).

`/trim after` cuts the other way: pick a message (newest first, with what
goes) and confirm, and everything after it, up to the newest message, leaves
the context, folded into a divider at that point (`/trim show` and Show
unfold it). It does not branch the session, unlike `/rewind`: what you send
afterwards comes after the divider and is in the context. `/delete` picks one
message (newest first, with its start and what goes) and drops it from the
transcript and the context at once; deleting a reply also drops its tool
calls' results (a tool result goes with its call), and `/delete undo` brings
back the latest deletion (`/delete` with a session's name or path still
deletes sessions). Deleted messages leave no divider. `/trim undo` cancels
the latest trim of either kind; trims and deletions stack, are refused
while a run is in progress, move the status line's context use by their
estimate, and survive a reload, fork and `/rewind` while their message is
on the path. The context sent to the model always stays well formed: a
request that would start with a reply (your first message deleted) starts
with a short note that earlier messages were removed, and a `/compact` afterwards
neither summarises nor carries over what is deleted or trimmed.

`/budget DOLLARS` sets a cost budget for the session (`/budget off` removes
it, `/budget` shows it): once the session has cost that much — subagents and
compactions included, counted as they happen — no further turn starts, the
run stops between turns with a note saying how to raise it, and prompts are
refused until you do. `max_budget_usd` in `config.json` (or `prigh run|serve
-max-budget-usd DOLLARS`, which wins) is the budget of sessions that set
none; `/budget` is recorded in the session.

`/goal TEXT` sets a goal and starts working on it: the agent keeps going
across turns until it judges the goal met and calls `goal_complete` with a
summary. Each run that ends normally while the goal is active is followed by
a message restating the goal and asking it to continue or declare it done,
up to `goal_max_continuations` (default 20) automatic continuations; an
abort, a failed run, the budget or that cap stop the goal (`/goal resume`
makes it active again, with fresh continuations). `/goal` shows it, `/goal
off` clears it. Messages you send meanwhile go first; the goal is recorded
in the session, so it survives restarts and reconnects.

Reminders are messages prigh sends the main agent (never a subagent) on its
own, e.g. not to stop before the work is done, or to clean up disk space now
and then. They belong to the session, recorded in it like the goal: they
survive restarts and reconnects, a fork keeps them, a new session starts
with none, and switching sessions switches to that session's.

- `/remind stop TEXT` reminds the agent when it ends its turn;
  `/remind stop N TEXT` at most N times in a row (default 3).
- `/remind every N TEXT` reminds it every N turns.
- `/remind` lists them in a picker (type to filter; Enter shows one in
  full, Ctrl+D in the TUI or Delete and the Remove buttons in prigh-web
  remove one); `/remind rm NAME` removes one (Tab completes the names) and
  `/remind off` all of them.

The status line counts them (`remind:2`; in prigh-web a "Remind 2" item that
opens the list).

Each gets a name, `r1`, `r2`, ... (never reused in the session), which the
chat shows (`※ Reminder r1: Don't stop until the tests pass. (1/3)`). An
`every` reminder is added after every N turns of the main agent, between
turns of a run or at the next run's start. A `stop` one starts a new run
when a run ends normally with nothing left to wait for: no message queued,
no running or undelivered background jobs or subagents, no goal continuing,
not aborted, failed, in plan mode or past the budget. It fires at most N
times in a row, then a note says so and it waits for your next message. The
model sees e.g. `[prigh reminder: r1, as you ended your turn (1 of 3)]
...`. Switching sessions starts the counts over. (A `reminders` list left
in `~/.prigh/config.json` from before is ignored, with a warning saying
so.)

When a run stops at a usage limit and no fallback model is left, prigh reads
when the limit resets (ChatGPT/Codex error bodies and `x-codex-*-reset-at`
headers, Anthropic's `anthropic-ratelimit-unified-*reset` headers,
`retry-after`) and resumes the run then, continuing the interrupted work;
the status line shows `resumes 14:05`. `/resume now` retries at once,
`/resume cancel` (or an abort, or a new prompt) cancels it. When the
provider does not say, a note says so and nothing is scheduled; when the
time it gives has already passed, the retry backs off (1 minute, doubling up
to an hour) instead of retrying at once.
`resume_on_limit_reset: false` in `config.json` turns this off. A scheduled
resume is recorded in the session: a backend that restarts before the reset
loads the session and resumes it on time (one whose reset passed while the
backend was down is dropped; send a message to carry on), and a session
deleted meanwhile is not resumed.

Responses API requests (OpenAI and ChatGPT/Codex) carry a `prompt_cache_key`
(the session id; a subagent's adds its id) so requests that share a prefix
reach the same prompt cache; custom OpenAI-compatible servers are not sent
it.

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
| `/login github [HOST] [token]`, `/login forgejo HOST [token]`, `/login HOST [token]`, `/logout HOST` | sign in to or out of a git host (see [Git hosting](#git-hosting)) |
| `/pr [list [open\|closed\|all]\|view N\|new [--generate] [title]\|checkout N]` | pull requests of the session's repository: pick one (Enter shows it), show one (also `/pr N`), create one from the current branch (`--generate`: the model drafts its title and body), or check one out in a git worktree |
| `/thinking [off\|on\|low\|high\|max]` | set the thinking level |
| `/verbosity [quiet\|normal\|verbose]` | set the transcript verbosity |
| `/filter [all\|KIND...\|-KIND\|+KIND]` | show only some kinds in the transcript (`user`, `assistant`, `thinking`, `tools`, `subagents`, `notes`); bare: a checklist |
| `/theme [original\|dark\|light]`, `/accent [name\|#rrggbb]` (prigh-web) | pick or set the theme and the accent colour, for you in every browser |
| `/confirm [on\|off]` | ask before destructive tools: sets the approval mode `ask` or `auto`, here and for new sessions |
| `/mode [ask\|auto-edit\|auto\|plan]` | pick or set this session's mode (Alt+M cycles) |
| `/plan [on\|off]` | toggle plan mode (read-only planning until you approve the plan) |
| `/todos` | show the model's plan (its `update_plan` steps) |
| `/permissions` | show the mode and the allow/ask/deny rules (config and this session) |
| `/compact [instructions]` | summarise older messages to free context, focusing on the instructions |
| `/trim [after\|undo\|show\|hide]` | drop everything before a message from the model's context (a picker, then a confirmation); `after`: everything after a message instead; `undo` brings the latest trim back, `show`/`hide` (TUI) unfold or fold the trimmed messages; see [Trimming the context](#trimming-the-context) |
| `/delete [undo]` | drop a message from the transcript and the model's context (a picker, newest first; a reply goes with its tool results); `undo` brings the latest deletion back; see [Trimming the context](#trimming-the-context) |
| `/compaction`, `/compaction at TOKENS\|off`, `/compaction model MODEL\|off`, `/compaction mask on\|off` | show or set when conversations are compacted (`compact_at_tokens`: also once the context exceeds TOKENS, e.g. `150k`), which model writes the summaries (`compaction_model`; `off`: the session's) and whether old tool outputs are masked first (`mask_observations`) |
| `/commit [-a] [--amend] [-m MESSAGE \| --generate]` | commit what is staged in the session's worktree (`-a`: every change to tracked files first); `--generate` drafts the message with the model into the editor, see [Git hosting](#git-hosting) |
| `/push` | push the worktree's branch to its upstream (set when it has none), with the tool host's git credentials |
| `/diff [path]` | show the uncommitted changes where the tools run (`git diff` against `HEAD`, untracked files as new), optionally of one path |
| `/review [WORKTREE\|BRANCH] [--base REV] [--direct]`, `/review [base] [instructions]` | alone or with a worktree (path, branch or name), a branch not checked out (read-only), `--base` and `--direct`: the review page (prigh-web) or the files' approvals and the comments (TUI) of that worktree or branch against its base (since the merge base, or directly), see [Reviewing the changes](#reviewing-the-changes); with a base (`HEAD`: the uncommitted changes) or instructions, have the model review the changes against `base`'s merge base for prioritised findings with file and line |
| `/init` | have the model write `AGENTS.md` for the project, or improve the existing one |
| `/notify [off\|bell\|desktop]` (TUI), `/notify [on\|off]` (prigh-web) | how to tell you that a turn ended, a tool waits for confirmation or background work finished while you look elsewhere |
| `/budget [dollars\|off]` | show or set the session's cost budget: no turn starts once it is spent |
| `/goal [text\|off\|resume]` | set a goal the agent keeps working toward until it is met; show, clear or resume it |
| `/remind [stop [N] TEXT\|every N TEXT\|rm NAME\|off]` | remind the agent when it ends its turn (at most N times in a row, default 3) or every N turns; without arguments, pick among the session's (Ctrl+D / Delete removes one); `rm` removes one, `off` all |
| `/resume [now\|cancel]` | show, run now or cancel the automatic resume after a usage limit |
| `/new`, `/clear`, `/quit` | new session / clear transcript / exit |
| `/name [text]` | set the session name |
| `/session` | show session statistics |
| `/sessions` | pick a saved session (Tab marks, Ctrl+D deletes, Alt+A archives/unarchives, Alt+D deletes all archived, Ctrl+N named-only) |
| `/archive [session]`, `/unarchive [session]` | archive or unarchive a session (id, unique id prefix, name or path; the current one without an argument) |
| `/delete session...` | delete sessions after a confirmation (or pick them in `/sessions`) |
| `/delete-archived` | delete every archived session after a confirmation |
| `/switch [path]` | switch to a saved session |
| `/cd [path]` | change the working directory (on the active tool host: `~` is that host's home, a relative path is under the current directory); a missing one is created once you confirm (`Create /x? (y/n)`; in prigh-web a "Create folder?" dialog); `/host`'s directory asks the same |
| `/mkdir [path]` | create a directory (with its parents) on the tool host and change to it |
| `/worktree [list\|new BRANCH [BASE]\|open [-n] TARGET\|back [main]\|rm [-f] TARGET]` | the repository's git worktrees: alone, a picker (Enter works there, TUI Ctrl+N / prigh-web Alt+Enter in a new session, Alt+R reviews its changes, TUI Ctrl+D / prigh-web Delete removes); `new` creates one and moves the session there; `open` moves there (`-n`: a new session); `back` moves the session back where it switched into worktrees from (`main`: the repository's main checkout, when that place is gone); `rm` removes one once you confirm (`-f`: with uncommitted changes) |
| `/fork`, `/rewind`, `/tree`, `/clone` | branch the session tree |
| `/export [path]`, `/import [path]` | markdown or `.jsonl` |
| `/agents [steer\|cancel ...]` | focus a subagent (Ctrl+D in the picker cancels one); `steer N TEXT` / `follow-up N TEXT` tell subagent N (its number or id) something; `cancel N` cancels it |
| `/jobs [<id>\|kill <id>]` | list background jobs (Enter shows recent output, Ctrl+D kills one), or show/kill one |
| `/queue` | the queued steers and follow-ups (Enter takes one back into the editor, Ctrl+D removes it) |
| `/host [name\|backend]` | pick where tools run, and the directory there |
| `/abort` | abort the current run |
| `/btw <question>` | ask a side question without interrupting the run |
| `/snippet [NAME\|add NAME [TEXT]\|rm NAME]` | insert a snippet at the cursor without sending it (alone: a picker, also Alt+N); `add` saves one (TEXT is the rest of the prompt, new lines included; without it, your last prompt), `rm` deletes one |
| `/skills`, `/skill:NAME [args]` | pick a skill (Enter puts `/skill:NAME ` in the editor for its arguments) / run one |
| `/mcp [reconnect]` | MCP servers and their state: Enter approves a project's server or lists a ready one's tools; `reconnect` restarts failed ones |
| `/retry-backend-connection` | reconnect to the backend now |
| `/connect [HOST:PORT\|URL\|local]` | (TUI) switch to another backend (`HOST:PORT` or a prigh-web URL such as `wss://host/ws`), saved for next time, or to a spawned one; alone: pick or type the address |
| `/signin [user]` | (TUI) sign in to the server: a dialog for the user and the token or password |
| `/signout` | forget the saved login and sign in again (TUI: the token; prigh-web: its sign-in form) |
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

### Git hosting

prigh signs in to GitHub (github.com, or GitHub Enterprise with `/login
github HOST`) and Forgejo or Gitea instances (codeberg.org, or any host with
`/login forgejo HOST`) and calls their APIs from the backend; no `gh` or
`tea` is needed. `/login github` uses GitHub's device flow (the login box
shows the code to enter) when OAuth is configured, otherwise it asks for a
token; `token` after the host goes straight to the token prompt. The
`/login` picker lists the known hosts under "Git hosting" (and "Forgejo:
another host…", which asks for the host), `/logout` the signed-in ones, and
`/auth` shows each host's user and method, and why OAuth is unavailable.
Accounts belong to the user, like model credentials.

Setting up the sign-in (once per backend):

- **GitHub** signs in with the OAuth device flow, which needs only the client
  id of an OAuth app (no secret), so it works the same from a terminal and a
  browser, local or remote. prigh ships no client id of its own: register one
  at <https://github.com/settings/applications/new> (any name, e.g. "prigh";
  homepage and callback URL can be `http://127.0.0.1`, they are not used),
  tick **Enable Device Flow**, and give prigh its client id, either in
  `~/.prigh/config.json`

  ```json
  { "forges": { "github.com": { "client_id": "Ov23li…" } } }
  ```

  or as `$PRIGH_GITHUB_CLIENT_ID` for the backend (the config wins). It
  asks for the `repo` scope. GitHub Enterprise: `"ghe.example.com": {"kind":
  "github", "client_id": "…"}` (API at `https://HOST/api/v3`). Without a
  client id, `/login github` asks for a personal access token instead
  (classic with `repo`, or fine-grained with Contents and Pull requests
  read/write).
- **Forgejo / Gitea** sign in with OAuth's authorization code flow with PKCE
  as a public client. Every stock Forgejo and Gitea instance pre-registers
  git-credential-oauth's public client (redirect `http://127.0.0.1`, any
  port), so this works with no setup, but the consent page names
  git-credential-oauth, and on Forgejo the token is not limited to scopes
  (Forgejo does not implement OAuth scopes yet; Gitea limits it to the
  requested `read:user write:repository`). An instance whose admin disabled
  the default applications, or to see your own app's name, needs a public
  OAuth2 application (Settings → Applications, untick "Confidential client",
  redirect URI `http://127.0.0.1`) and `"git.example.com": {"kind":
  "forgejo", "client_id": "…"}` in `forges`. The browser is sent back to a
  one-shot server on the backend's `127.0.0.1`; when prigh runs on another
  machine that page fails to load: paste its address into the login box.
  `"url": "http://git.lan:3000"` sets an instance's address when it is not
  `https://HOST`. Unknown hosts are taken for Forgejo; `"kind"` says
  otherwise.
- Either: `token` after the host (`/login forgejo codeberg.org token`) asks
  for a personal access token (Forgejo: scopes `read:user` and
  `write:repository`).

Sign-ins are stored with the model credentials, in `auth.json` under
`forge:HOST` (0600, written atomically under the same lock; per user with
`-tokens`). Forgejo's OAuth tokens last an hour and are refreshed when
needed; a failed refresh says to sign in again. Tokens never appear in
errors or logs.

`/pr` works on the repository of the session's directory on its tool host
(found from `git remote`). `/pr` (or `/pr list closed|all`) is a fuzzy
picker of pull requests (`#24 Git hosting · lloyd · feat/x → main · draft`);
Enter, or `/pr view 24` (`/pr 24`, `/pr #24`), shows its title, state,
branches, URL, description, checks and diff. `/pr new [title]` opens a form
prefilled from the branch (title, body, base, draft, and "push first" when
the branch is not on the remote yet): Tab or ↑↓ move between fields, Space
toggles, Ctrl+J starts a new line in the body, Enter creates it and the
notice gives its URL. `/pr checkout 24` checks the branch `pr-24` (from
`refs/pull/24/head`) out in a worktree where `/worktree` makes them
(`worktrees_dir`, `~/.prigh/worktrees/<project>/pr-24`) and asks whether to
`/cd` there; again later it fast-forwards that worktree. The
repository is the current branch's remote, else `origin`, else the first
remote on a known host; pull requests from forks are listed as
`owner:branch`, but `/pr new` opens one from the current branch of that
remote's repository. "Push first" runs `git push -u REMOTE HEAD:BRANCH` on
the tool host with its own git credentials (never prompting). On Forgejo a
draft is a `WIP:` title. Errors say what to do (`/login github signs in`,
`git remote add origin URL`, ...). The worktree context menu's "Create pull
request…" and the review page's Create PR open it for that worktree's
branch.

**✦ Generate.** Every field that holds a description can be drafted by the
model: the pull request's title and description (from the branch's commits
and its diff against the base, the repository's pull request guidance in
AGENTS.md, CLAUDE.md or CONTRIBUTING.md, and its
`.github/pull_request_template.md`, filled in), the review's commit message
(above), a new worktree's branch name (from the task typed in its field, else
the session's goal) and the session's name (from the conversation). In
prigh-web it is the ✦ Generate button in the field's corner (Alt+G): the
draft streams into the field, Stop or Esc keeps what came, Undo puts the text
back; it stays editable before use. In the TUI, `/pr new --generate [title]`
drafts the form's title (unless one is given) and body, Ctrl+G in the form
drafts on demand (Esc stops, Ctrl+_ swaps back to the text from before), and
`/commit --generate` drafts a commit message. One tool-less request per
draft, without thinking, on `generate_model` from `config.json` (a model name,
id or prefix, e.g. a cheap fast one like `claude-haiku-4-5`), else the
session's model; its cost counts towards the session's. Large diffs are cut
down to fit (every file keeps a share of its hunks, the rest listed with
their line counts).

**Committing and pushing in the TUI.** `/commit [-a] [--amend] -m MESSAGE`
commits what is staged in the session's worktree (`-a` first stages every
change to tracked files; `-am MESSAGE` does both). The message is the rest
of the line, newlines included, or a quoted string (`"..."` with `\"` and
`\\` escapes, or `'...'`); flags go before `-m`, which completes them.
`/commit --amend` without `-m` keeps the old message. `/commit [-a]
[--amend] --generate` drafts the message from the staged changes into a
"commit message" panel above the editor (Esc stops it), then leaves `/commit
-m "<message>"` in the editor to edit and send; Ctrl+_ brings back what you
had typed meanwhile. The transcript says what happened (`committed 1a2b3c4
Fix the parser on main`); a refused commit goes back into the editor with
the reason. `/push` pushes the worktree's branch to its upstream (set when
there is none) and says `pushed main to origin` or why not.

`/diff` runs `git` on the active tool host, in the session's directory, and
shows the changes by file with their line counts (the TUI in the
transcript, prigh-web in a dialog); a diff over 1 MB is cut short.
`/review BASE` and `/init` start a run with a prompt the backend writes (the
review prompt includes the diff when it is small enough, else tells the
model how to read it); like a prompt they queue while a run is going, and
an error (nothing to review, an unknown base, not a repository) says what
to do. The transcript shows them as notes (`⚑ Review of uncommitted
changes`, `▤ Write or improve AGENTS.md`), not as the long prompt.

`/worktree` works with the git worktrees of the repository the session is
in, on the active tool host. `/worktree new BRANCH [BASE]` puts a new one
at `<worktrees_dir>/<project>/<branch>` (`~/.prigh/worktrees/prigh/feat-x`
for the branch `feat/x` of a checkout named `prigh`; `/` and unusual
characters become `-`): a new branch from BASE (default: where the session
is), an existing local branch checked out as it is (no BASE then), or,
without BASE, the one remote branch of that name, tracked. The project
directory records which repository it belongs to (`.prigh-repo`), so
another checkout with the same name gets `prigh-2`, and a branch whose
directory is taken gets `-2`. A branch already checked out elsewhere is
refused with where it is. The list shows each worktree's branch, path,
uncommitted changes (`*`), commits ahead of and behind its upstream, and
the sessions working in it; `TARGET` is a branch, a path or a directory
name, completed with Tab, and an unknown one gets the closest matches.
Removing asks first (saying what would be lost and which sessions work
there), keeps the branch, and is refused for the main checkout, for the
worktree the session itself is in (open another first) and, without
`-f`, for one with uncommitted changes or locked; a worktree whose
directory was deleted is pruned. prigh-web's sidebar shows, under each
session, the worktrees it made, moved into or gave its subagents (above),
including those its agents made with the `worktree` tool or `git worktree
add`.

Snippets are texts you insert into the editor often, kept in `config.json`
and shared by every frontend:
`"snippets": [{"name": "review", "text": "Review the diff for bugs..."}]`
(names are one word and unique, texts non-empty and may span lines; a bad
entry is reported with where it is and what to fix). `/snippet` or Alt+N
opens a picker filtering names and texts; Enter inserts the text at the
cursor, without sending it, and Esc changes nothing. `/snippet NAME`
inserts one directly (Tab completes names; an unknown one is refused with
the closest names). `/snippet add NAME TEXT` saves (or replaces) one: TEXT
is everything after the name, new lines included (Ctrl+J in the TUI,
Shift+Enter in prigh-web), and without it the last prompt you sent is
saved. `/snippet rm NAME` deletes one. Both save through `set_config` with
only `snippets`, and say what they did.

In the editor, `#` at the start of a word followed by a letter or digit
completes snippet names (fuzzily, each with its text's first line), like `@`
completes paths: Tab or Enter replaces `#name` with the snippet's text (new
lines included), the cursor after it, without sending. Nothing pops up for
a `#` inside a word, `# Title` or `##`, or a `#123` that names no snippet,
so they type and send as written.

Notifications: the TUI's `/notify` sets `notify` in `config.json` (`bell`
by default; `desktop` sends OSC 9, or OSC 777 to foot and urxvt, wrapped
for tmux, which needs `set -g allow-passthrough on`). It turns on focus
reporting and only notifies while the terminal says it is not focused
(in tmux, `set -g focus-events on`); `/notify` alone says what it does and
whether the terminal reports focus. prigh-web uses the browser's
notifications while the tab is hidden: `/notify on` (or the settings page's
switch, or the account menu's "Turn notifications on") asks the browser for
permission, `/notify off`
turns them off (kept with your preferences on the backend); the tab's title
and favicon count what happened while it was hidden, notifications on or
not. A blocked or unsupported browser (plain http other than localhost) is
named with what to do in the settings page, whose switch shows whether
notifications are on and turns them on or off.

Text the harness adds to what the model reads is recorded with the message
(`origin` in the session file and the RPC: environment notes about a new
tool host or directory, or tools a model change added or removed; attached `@` files; and the messages
it writes in your place: reports of finished background work, a fallback
model's hand-over, goal starts and continuations, the resume after a usage
limit, `!cmd` output, the compaction summary, `/review` and `/init`). The
model still sees exactly the same text; the frontends show these as quiet
one-line notes (`⇄ Tools now run on laptop`, `◷ Goal: continuing (3/20)`)
above or instead of your message, which shows only what you typed. Verbose
(Ctrl+O) in the TUI, and clicking the note in prigh-web, shows the raw
text. Sessions written before this are recognised by their wording and
shown the same way.

`/btw` works while a turn is running: one extra model call (no tools) answers
from the session's current context, streaming into a box above the composer
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
rather than apt or sudo. It tells the main agent that it is the
orchestrator: substantial work (a feature, a multi-file change, a long
investigation) goes to subagents with self-contained tasks, independent
ones in parallel, while it plans, coordinates, reviews their reports and
talks to you, doing small things itself; subagents are not told this. The
system prompt is built once, at a session's
first run, and recorded in the session so the cached prompt prefix survives
`/cd`, `/host` and backend restarts; later directory or host changes reach
the model as a short note on the next message, leaving it to re-read the
instructions if that seems worthwhile. The session records what the model
was last told, so the note still comes if you switch to another session,
reconnect or restart the backend first, and none comes when you changed
things back.

### Approvals, plan mode and questions

Each session has an approval mode, always shown in the status line
(`mode:ask`, `mode:auto-edit`, `mode:auto`, `mode:plan`; in prigh-web
beside the model, along the composer's bottom edge) and switched with
`/mode` or Alt+M (ask → auto-edit → auto → plan → ask):

- `ask`: destructive tools (`bash`, `write`, `edit`, `apply_patch`,
  `job_input`, which writes to a running job that may run what it reads,
  `worktree`'s `create` and `remove` (not `list`), and MCP tools that are
  not read-only) wait for your answer.
- `auto-edit`: file edits run without asking; `bash`, `job_input`,
  `worktree create`/`remove` and MCP tools still ask.
- `auto`: nothing asks (the default, as before).

A new session starts in the config's `approval_mode`; a session keeps its
mode (and the rules below) in its file, so it survives a reload or a backend
restart, and forks take it along. Subagents' tool calls are checked like the
main agent's (their confirmations name the subagent).

A confirmation offers: `y` allow once; `a` always allow, for the rest of the
session, the rule shown (for `bash`, the program, with its subcommand for
tools like `git`, `npm`, `cargo` or `dune`, e.g. `bash(git status:*)`; for
file edits, `edit`; for other tools, the tool); `n` deny; `r` deny with a
reason you type, which the model sees as the tool's result. A confirmation
answered in another frontend closes in the others, and one still waiting
reappears after a reconnect.

Rules in `config.json`'s `permissions` refine every mode, written like
Claude Code's:

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

`TOOL` alone covers every call of the tool (`*` in a name matches anything,
so `mcp__github__*` is a server's tools); `edit` stands for every file
change (`edit`, `write`, `apply_patch`) and `read` for every file read
(`read`, `ls`, `grep`, `find`). A `bash` pattern `PREFIX:*` matches commands
that are `PREFIX` or start with it and a space; other patterns are globs.
Commands are split at `;`, `&`, `&&`, `|`, `||` and newlines: allow rules
must cover every part (`git status && rm x` is not allowed by
`bash(git status:*)`), and never cover commands with `$(...)`, backquotes,
`<(...)` or output redirections (other than to `/dev/null`), while a deny or
ask rule matching any part applies. File tools' patterns are globs over the
paths a call touches (`**` across directories; relative to the session's
directory; `~/` is the home directory). Deny rules win, then ask rules,
then allow rules; a denied call is refused with a message the model sees.
`/permissions` lists the rules in force; invalid ones are reported and left
out. There is no per-project config: rules are per user (per namespace).

Plan mode (`/plan`, `/mode plan` or Alt+M) is for investigating and
planning before changing anything: file edits are refused, and every other
destructive tool asks (in every approval mode) unless an allow rule covers
it, so read-only commands can run once you allow them. The model is told
when plan mode starts and ends. It keeps its steps with `update_plan` and,
when the plan is ready, calls `exit_plan_mode` with it: the plan shows as a
confirmation, and approving it leaves plan mode (back to the approval mode
you had) for the model to implement it; `n` or `r` keep it planning, with
your feedback. An active `/goal` gets no automatic continuations while plan
mode is on (the model cannot make progress on it, only plan); leaving plan
mode carries on with it.

The model's plan (`update_plan`: steps pending, in progress or done, as in
Codex) is session state: the status line shows `plan 2/5: current step`
(`(+2 running)` when several run in parallel) while steps remain, `/todos` shows it whole, and the transcript shows every
update as a checklist.

With `ask_user` the model asks you one or more questions, each with
optional choices (one, or several to pick) and room for your own answer. A
dialog takes the keyboard: ↑/↓ choose, Space picks, typing writes your own
answer, ←/→ move between questions, Enter answers (the last one sends), Esc
declines (the model is told you declined); the box lists these keys. The first frontend to answer wins and the others' dialogs close;
an unanswered question comes back after a reconnect, and aborting the run
withdraws it. A question nobody answers times out, after
`ask_user_timeout_seconds` in `config.json` (default 300; `0` = never): the
model is told you did not answer and carries on with its best judgement,
saying what it assumed, the dialog closes and a notice says so. The dialog
shows when that happens (`unanswered, the agent carries on by itself at
14:05`). The model can give one question a shorter `timeout_seconds` of
its own (30 to 300), but never a longer one than yours, and only you can
turn the timeout off. Subagents cannot ask (nor plan, nor leave plan mode): they
put open questions in their report for the main agent to ask. Headless
`prigh run` has nobody to answer, so it refuses confirmations (the model is
told to have them allowed in the config) and questions (the model decides
itself).

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
count as destructive: they ask first in the `ask` and `auto-edit` modes). A project's servers run commands that
came with the project, so they only start once you approve them in `/mcp`
(the approval lasts until their definition changes; it is kept in
`~/.prigh/mcp-approvals.json`). Problems, failed starts and servers awaiting
approval are reported once each; `/mcp` shows every server with its status
and tools, and `/mcp reconnect` restarts failed ones. `-no-tools` turns MCP
off.

### Testing

`./test` runs everything below (the dune tests one project after the other,
prigh-web's browser e2e in Chromium alongside); `./test --full` runs the
browser e2e in Firefox as well, like the flake's checks. Run the quick one
before pushing and the full one before merging.

Four layers, cheapest first; `dune build @runtest` in each project runs all
of them (the last two need the backend binary built first), and `dune
promote` accepts new output everywhere:

1. **Pure expect tests** — `cd backend && dune build @runtest` drives the
   agent loop, tools and RPC with `Faux_provider` and the providers/HTTP/OAuth
   flows with an in-process EIO HTTP server; `cd frontend && dune build @runtest`
   covers protocol/client decoding, the widgets, the keymap, and `App.update`
   scenarios that print the rendered screen (`Screen.to_plain`).
2. **Driver-frame snapshots** — `frontend/tui/test/test_term_frames.ml`, run by the
   same `cd frontend && dune build @runtest`: drives the real Bonsai_term driver
   over an in-memory tty, decodes Notty's output with `frontend/tui/test/vt.ml`, and
   compares it with `Screen.to_plain`.
3. **Protocol e2e** — `frontend/tui/e2e` (also `dune build @e2e`) runs the real
   `backend/_build/default/bin/main.exe serve -faux` through the real client
   with an isolated `HOME`/`-auth-file` and diffs a normalised transcript.
4. **Real terminal** — the cram tests `frontend/tui/tmux-test/*.t` (skipped
   without `tmux`; each a group of scenarios, run in parallel) run the
   scenarios of `harness.sh` with the built TUI inside tmux and compare the
   captured panes, including a backend kill and reconnect.

To tune compaction with data rather than guesses, `prigh compaction-eval
SESSION...` (or `-all`; a developer's tool, not in `prigh -help`) replays
saved sessions under no compaction, masking, summarising and both, and
prints what each costs (peak and total input tokens, with and without a
prompt cache, cache invalidations) and what it keeps (the user's
messages, the files touched, the errors). It manages the context as the
agent would with your `config.json` (`compact_at_tokens` sets where it
summarises, `-compact-at` overrides it); `-window`, `-threshold-tokens`,
`-keep-turns`, `-mask-min-bytes`, `-mask-at` and the other flags vary
the parameters, `-json`/`-sexp` print it for scripts, and `-live` (which
costs money) has a model write the summaries (`compaction_model`, or
`-compaction-model`, when set) and answer questions about what is left. It only reads the session files.

prigh-web has its own: `frontend/web/test` runs under node in `@runtest`,
and the Playwright e2e (`frontend/web/e2e`) drives Chromium and Firefox
against the real backend, its scripts all at once; the flake runs it as a
check.

## Layout

- `backend/lib` — one library in groups: `agent/` (`Agent_loop`: stream,
  run tools, repeat; `Agent`: session, queues, abort, compaction),
  `session/` (the `Session` JSONL log, messages), `providers/`
  (`Anthropic`, `Openai_responses`, `Deepseek`, picked per request by
  `Provider_router`), `auth/` (`Auth_store`, `Provider_auth`,
  `Oauth_anthropic`, `Oauth_openai_codex`, `Login_manager`), `tools/`
  (`Tools`: bash, read, write, edit, ls, grep, find, and `Tool_subagent`),
  `host/` (tool hosts), `mcp/`, `rpc/` (`Rpc_server`, `Web_server`),
  `terminal/`, `util/`.
- `backend/test` — expect tests, driven by `Faux_provider` and an in-process
  HTTP server; no network.
- `frontend/` — the frontends: `protocol/` (wire types), `client/`
  (Async_kernel RPC over a `Transport`; `client/unix/` has the stdio/TCP
  transports and the tool host), `ui/` (platform-agnostic Bonsai logic:
  `App` state machine, `Editor`, `Picker`, `Transcript`, `Render` to styled
  `Content`), `tui/` (the terminal frontend: `lib/` Bonsai_term views and
  backend process, `bin/`, `test/`, `e2e/`, `tmux-test/`) and `web/`
  (prigh-web, the browser frontend: `lib/`, `app/`, `bin/`, `test/`,
  `e2e/`).
- `editors/nvim/` — prigh.nvim, the Neovim ACP client (Lua; headless tests
  against a scripted fake agent: `editors/nvim/test/run.sh`).
- `ARCHITECTURE.md` — how the pieces fit together; `DOCKER.md` — the
  container; `AGENTS.md` — conventions for working on the code.
