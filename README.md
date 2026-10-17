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
(reconnecting when the connection drops, but exiting with what to fix
when the backend refuses its `-token`/`-user`; it is the machine's host, by
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
commands, `@` paths, `#` snippets and `!cmd` like the TUI. Messages show when they were
sent, in the browser's time zone (`14:32`, `Yesterday 14:32`, `3 Oct
14:32`; the full date on hover): under your messages and in a reply's
footer, with a separator where the day changes. It speaks
prigh's own RPC (the same `/ws` protocol as `-web`), so it gets every
backend feature without a translation layer like pi-web's.

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
running tool, subagent or job, on the status line ("Working"), and on
whatever waits for the backend; nothing finished glows, and while the
connection is down only the reconnecting banner and status line do.
Every action shows at once, before the backend answers: a session clicked
in the sidebar lights its lamp while the chat shows the shape of a
transcript (also while connecting, and for a new session), a prompt shows
as "Sending…" with the send button busy until the transcript has it, a
model, thinking level or approval mode chosen shows (disabled, with the
lamp) until the backend confirms it, and a command that lists something (`/login`, `/fork`,
`/tree`, `/session`, `/skills`, `/mcp`, `/setusr`…) opens its dialog with
a loading row (Esc closes it, and the late answer then opens nothing).
While the connection is down the composer is dashed and the status line
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

The sidebar lists the active sessions first (a session is running, with
its lamp lit, while its turn or its background subagents and jobs run),
each with its age and directory, then a faint "Archived 14" rule that
unfolds the archived ones, muted (a search shows the matching ones
either way). Hovering a session shows its archive (or unarchive) and delete
buttons; on a focused session A archives or unarchives it and Delete
deletes it. "Select" (or `/delete` with no argument) turns the list into
checkboxes: click or Space marks a session, Shift+click or Shift+Space the
range from the last one (across the divider), and Delete (or the bar at
the foot of the list, "Delete 3 sessions (12.4 MB)") asks to delete the
marked ones; "Delete archived" under the archived ones
(`/delete-archived`) every archived one. The confirmation says how many sessions and how much disk they use, and
which ones the backend will keep (running, open in another tab or terminal,
or the one you are in): those are not asked for, and with nothing left to
delete the dialog only says why (Close). `/archive`, `/unarchive` and `/delete` also take
sessions by name, id or path. The list follows the backend as it changes.

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
which scrolls to its card. It opens from the status line ("2 agents running"), the top bar's
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
`../share/prigh/prigh-web`. The Nix package (`nix run . -- -prigh-web`)
installs the release build of the page (whole-program js_of_ocaml, about
2 MB of JavaScript, sent as a 500 KB brotli or 590 KB gzip file it
ships precompressed); reloads revalidate with `ETag`/`Last-Modified` and
cost a `304` per file. A dune build in the dev profile makes a ~50 MB one
with inline source maps, uncompressed, quicker to relink but slow to load. The page keeps `?session=` in the address bar
and the chat's scroll position and draft in the tab's sessionStorage (a
reload, or a phone bringing back a discarded tab, returns to the same place,
waiting for the backend if it is out of reach), reconnects when the
backend goes away (see above), and takes `?backend=ws://host:port/ws` like the `-web` page.
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

The folder button in the top bar (or `/files [path]`, or Ctrl+Shift+F)
opens the file browser, a drawer right of the chat (a full-screen sheet on
a phone) on the machine where the session's tools run (the backend, the
Docker container, or a laptop connected as a tool host), in the session's
directory. The breadcrumbs (or the pencil, to type a path: absolute, `~/…`
or relative to the directory shown), the up arrow and Backspace move
around; the eye shows dot files; the columns sort by name, size or age
(directories first). A file clicked (or reached with ↑↓) is previewed:
text with line numbers (the first 256 KB), images (up to 20 MB), binaries
only named. The selected entry's actions: Attach (to your next message),
Download (a directory as a `.tar.gz` archive: `tar -czf` of it, paths
starting with its name), Mention (`@path` into the composer), Copy path,
Rename (F2) and Delete (Delete; it asks first, and refuses `/` and the
home directory). The upload button (or files dropped on the drawer)
uploads into the directory shown, with progress, asking before it
replaces a file; New folder makes one. Every error says what to do next,
dialogs own the keyboard (Enter accepts, Esc cancels without doing
anything), and switching session or host starts the browser afresh.

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
files upload (or a download is being prepared), the status line says so
with the work lamp: "Uploading 2 files 40%".

Transfers go through the backend, so they work on any tool host: the page
asks over its signed-in connection for a one-time link (a random ticket,
valid 5 minutes, for that session's host only), then `GET
/files/download?ticket=…` or `PUT /files/upload?ticket=…` on the same port
streams the bytes, in chunks, to or from the host (uploads up to 2 GB). A
tool host running an older prigh says to update prigh there.

Every panel resizes from a handle on its inner edge: the sessions
sidebar's right edge, the file browser's and the agents panel's left
edge, the terminal's top edge, and the line between the file list and
its preview. The handle shows as a faint line under the pointer and turns
sodium while it is dragged or focused; double-click it (or press Enter on
it) for the default size. It is a focusable separator: Tab to it, then
the arrow keys move it (Shift: four times as far), Home and End make the
panel as small or as large as it goes. The chat always keeps 400px: when
the window narrows, the side panels beside it shrink in proportion (down
to their minimums) and grow back to the sizes you chose when it widens
again. The sizes are kept in the browser (`prigh-web.sidebar-width`,
`files-width`, `files-split`, `agents-width`, `terminal-height` in
localStorage). On a phone the panels are sheets, without handles.

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
with its line counts); `/notify on|off` and the account menu control
browser notifications while the tab is hidden, and the title (`(2) prigh`)
and favicon count what happened meanwhile. Beside the model in the top bar
the thinking level ("Thinking high", "No thinking" for a model without
levels) and the approval mode ("Asks: edits & commands", "Asks: commands",
"Auto: never asks", "Plan mode", lit) are always shown, each opening its
picker (the modes with what each asks first). The top bar fits the chat
column, however wide the sidebar and the panels leave it: the labels get
short (`high`, `ask`, `plan`), then the thinking level and the mode become
icons (their tooltips say which), then all three fold into one menu behind
a sliders button (lit in plan mode); the session's title and cwd keep their
room, the cwd losing its middle rather than the directory it ends in.
Alt+T and Alt+M cycle them. The status line's plan chip (`plan 2/5: step`)
runs `/todos`. Steers and follow-ups sent while the agent works wait above
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
lists them; `/help` and
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
tui/_build/default/bin/main.exe                # interactive TUI (PRIGH_BACKEND or the dune build)
backend/_build/default/bin/main.exe run "explain this repo"    # headless
backend/_build/default/bin/main.exe run -session ID "go on"     # continue a saved session (id, unique prefix or file)
backend/_build/default/bin/main.exe sessions list          # saved sessions, the archived ones last under "archived:"
backend/_build/default/bin/main.exe sessions delete ID...  # ids may be unique prefixes; nothing is deleted if one matches none or several
backend/_build/default/bin/main.exe sessions prune -dry-run                        # empty sessions
backend/_build/default/bin/main.exe sessions prune -max-messages 2 -cwd /tmp       # filters: -older-than DAYS, -prompt TEXT
backend/_build/default/bin/main.exe serve      # JSON-lines RPC on stdio
```

### Keys

| Key | Action |
|---|---|
| Enter | send the prompt; accept the highlighted item (for command arguments only once you have typed a filter or moved the highlight — `/model` Enter Enter opens the picker) |
| Alt+Enter | queue a follow-up to run after the current turn |
| Alt+S | send exactly what is typed, ignoring the completion popup (`/cd newdir` while it highlights an existing directory); Esc then Enter does the same |
| Ctrl+J / Alt+J | insert a newline |
| Esc | close the dialog, abort the running turn, or scroll back to the bottom |
| Tab | accept the highlighted completion (a command, argument, `@path` or `#snippet`); on an empty prompt, list the commands |
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
| Alt+M | cycle the mode: ask → auto-edit → auto → plan (read-only) |
| Alt+N | pick a snippet to insert at the cursor (`/snippet`) |
| Ctrl+N | picker: toggle the named-only / logged-in-only filter |
| Tab / Alt+A / Alt+D | `/sessions`: mark a session / archive or unarchive it / delete every archived one |
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

In lists (pickers) typing filters fuzzily, Enter accepts and Esc closes
without changes; Ctrl+D deletes sessions, cancels a subagent, kills a job or
removes a reminder,
and `/scoped-models` toggles with Space (Ctrl+A all, Ctrl+X none). `/help`
lists the commands, the prompt prefixes (`!cmd`, `!!cmd`, `!&cmd`, `@path`, `#name`)
and these keys.

`/sessions` lists the active sessions first and then, after an `── archived
──` divider and dimmed, the archived ones. `▶` marks a running session (its
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
open dialog, in yellow), agents, jobs, the tool host — with the current
mode's keys (`⠋ working · Esc aborts · Enter
steers`, a picker's keys) in yellow. `think:` and `mode:` are always there
(at narrow widths they outlast the cost and the cwd; `view:` gives way to
the cwd first). Queued steers and follow-ups (whichever frontend queued
them) are listed above the editor, one per line, until they are delivered:
Alt+Up takes the last back into the editor, and `/queue` picks any of them
(Enter edits it, Ctrl+D removes it). An unknown command
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
optionally with a timeout; what it returns is not delivered again; it
returns after `auto_background_seconds`, below, with what is still running),
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
long: builds, test suites, installs, deployments, servers, watchers) and
gets `started job j1: ...` back at once. The job runs on the
session's active tool host like any `bash` call (the backend, or a frontend's
`prigh tool-host`), with no default timeout; its output is kept in memory
(the last 1 MB). When it exits, a message like `[job j1 exited 0] make test`
plus the last 40 lines of output reaches the main agent exactly like a
subagent report (an idle agent starts a turn; reports finishing together, jobs
and subagents alike, arrive as one message). Killed jobs report `killed`, a
host that disconnects fails its jobs (`failed: tool host disconnected`). The
model also has `job_status`, `job_output` (peek at recent output, paging back
with `offset`), `job_wait` (block on some or all jobs; returned reports are
not delivered again), `job_input` and `job_kill`.

Subagents have jobs too, their own (`j1`, ... per subagent): `bash` with
`background` and the job tools work the same, but since a subagent is not
woken after it ends its turn, its `job_wait` blocks as long as the jobs
take (its `timeout` only bounds it), finished jobs' reports reach it
between its turns, and jobs still running when it finishes are killed,
which its report says (`[killed when the subagent finished, still
running: job j2 (sleep 60)]`). Its system prompt and tool descriptions tell
it to block, never sleep: foreground commands for results it needs (one
blocking command to wait for something to become ready), jobs plus
`job_wait` for commands side by side, and to wait for what it needs before
its final report. The main agent's prompt tells it never to sleep to pass
the time either.

The main agent never stalls on a long command or a wait.
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
command starts, not while a confirmation waits for you. `job_wait` and
`subagent_wait` return after the same limit (their `timeout` can be
shorter, not longer) with what is still running, whose reports are still
delivered later, once. The system prompt and the tool descriptions tell
the model to keep working or end its turn rather than wait. Subagents'
own foreground `bash` calls are never moved, and their `job_wait` has no
such limit. The tool card shows `job j3` and
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
is resumed, and the session simply has no report for them. Job ids continue
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
through; `/scoped-models` edits it), `approval_mode` (`ask`, `auto-edit`
or `auto`: the mode new sessions start in, see below; `/confirm on|off`
sets `ask` or `auto`; files with the older `confirm_tools` still work, and
it is replaced by `approval_mode` when prigh saves the file),
`permissions` (allow/ask/deny rules, below; edited by hand) and
`default_model`/`default_thinking` (what new sessions start with unless
`-model`/`-thinking` is given; `/change_default` saves the current ones),
`notify` (`off`, `bell` or `desktop`; the TUI's `/notify`),
`ask_user_timeout_seconds` (default 300, `0` = never: how long a question
from the model waits for you, above), `snippets` (texts to insert into the
editor, `/snippet` below) and
`auto_background_seconds` (default 30, `0` = never: when a foreground
command moves to the background and how long a wait lasts, above).
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

Context never runs out mid-task. Between the turns of a run, when the last
response's tokens (plus the tool results since) cross 80% of the model's
window, the older part of the conversation is summarised and the run carries
on with the summary (the transcript shows `Context compacted`); the same
happens after a run, and on `/compact`. When a provider rejects a request as
too long anyway (Anthropic's "prompt is too long", OpenAI's
`context_length_exceeded`, and the wordings of DeepSeek and other
OpenAI-compatible servers), the conversation is compacted and the request
sent again, keeping less the second time; only then does the run fail,
saying what to do. A compaction keeps the end of the conversation from a
turn boundary, so tool calls keep their results, and when it falls in the
middle of the work on your latest message, that message is kept verbatim
ahead of the summary. Subagents compact their own context the same way.

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
| `/thinking [off\|on\|low\|high\|max]` | set the thinking level |
| `/verbosity [quiet\|normal\|verbose]` | set the transcript verbosity |
| `/confirm [on\|off]` | ask before destructive tools: sets the approval mode `ask` or `auto`, here and for new sessions |
| `/mode [ask\|auto-edit\|auto\|plan]` | pick or set this session's mode (Alt+M cycles) |
| `/plan [on\|off]` | toggle plan mode (read-only planning until you approve the plan) |
| `/todos` | show the model's plan (its `update_plan` steps) |
| `/permissions` | show the mode and the allow/ask/deny rules (config and this session) |
| `/compact [instructions]` | summarise older messages to free context, focusing on the instructions |
| `/diff [path]` | show the uncommitted changes where the tools run (`git diff` against `HEAD`, untracked files as new), optionally of one path |
| `/review [base] [instructions]` | have the model review the uncommitted changes, or those against `base`'s merge base, for prioritised findings with file and line |
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
| `/delete [session...]` | delete sessions after a confirmation (without arguments: pick them in `/sessions`) |
| `/delete-archived` | delete every archived session after a confirmation |
| `/switch [path]` | switch to a saved session |
| `/cd [path]` | change the working directory; a missing one is created once you confirm (`Create /x? (y/n)`; in prigh-web a "Create folder?" dialog); `/host`'s directory asks the same |
| `/mkdir [path]` | create a directory (with its parents) on the tool host and change to it |
| `/fork`, `/rewind`, `/tree`, `/clone` | branch the session tree |
| `/export [path]`, `/import [path]` | markdown or `.jsonl` |
| `/agents [cancel <n>]` | focus a subagent (Ctrl+D in the picker cancels one), or cancel subagent n |
| `/jobs [<id>\|kill <id>]` | list background jobs (Enter shows recent output, Ctrl+D kills one), or show/kill one |
| `/queue` | the queued steers and follow-ups (Enter takes one back into the editor, Ctrl+D removes it) |
| `/host [name\|backend]` | pick where tools run, and the directory there |
| `/abort` | abort the current run |
| `/btw <question>` | ask a side question without interrupting the run |
| `/snippet [NAME\|add NAME [TEXT]\|rm NAME]` | insert a snippet at the cursor without sending it (alone: a picker, also Alt+N); `add` saves one (TEXT is the rest of the prompt, new lines included; without it, your last prompt), `rm` deletes one |
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

`/diff` runs `git` on the active tool host, in the session's directory, and
shows the changes by file with their line counts (the TUI in the
transcript, prigh-web in a dialog); a diff over 1 MB is cut short.
`/review` and `/init` start a run with a prompt the backend writes (the
review prompt includes the diff when it is small enough, else tells the
model how to read it); like a prompt they queue while a run is going, and
an error (nothing to review, an unknown base, not a repository) says what
to do. The transcript shows them as notes (`⚑ Review of uncommitted
changes`, `▤ Write or improve AGENTS.md`), not as the long prompt.

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
notifications while the tab is hidden: `/notify on` (or the account menu's
"Turn notifications on") asks the browser for permission, `/notify off`
turns them off (kept in localStorage, `prigh-web.notify`); the tab's title
and favicon count what happened while it was hidden, notifications on or
not. A blocked or unsupported browser (plain http other than localhost) is
named with what to do. The status line's bell shows whether notifications
are on (struck through when off or blocked) and turns them on or off.

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
beside the model) and switched with
`/mode` or Alt+M (ask → auto-edit → auto → plan → ask):

- `ask`: destructive tools (`bash`, `write`, `edit`, `apply_patch`,
  `job_input`, which writes to a running job that may run what it reads,
  and MCP tools that are not read-only) wait for your answer.
- `auto-edit`: file edits run without asking; `bash`, `job_input` and MCP
  tools still ask.
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
while steps remain, `/todos` shows it whole, and the transcript shows every
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
- `editors/nvim/` — prigh.nvim, the Neovim ACP client (Lua; headless tests
  against a scripted fake agent: `editors/nvim/test/run.sh`).
- `ARCHITECTURE.md` — how the pieces fit together; `DOCKER.md` — the
  container; `AGENTS.md` — conventions for working on the code.
