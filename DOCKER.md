# Running prigh in Docker

`Dockerfile` and `docker-compose.yml` at the repository root run the prigh
backend in a container serving prigh-web (the browser UI), terminal
frontends, or an interactive TUI.

Each user (namespace) is its own Linux user in the container: its agent's
tools (bash, edits, the `>_` shell) run as that user in `/home/<name>`,
which only it can access. The backend runs as the `prigh` service user;
after startup only `nix-daemon` runs as root. Agents install tools with Nix
into a store shared by all users.

## Quick start

```
export PRIGH_TOKEN=$(openssl rand -hex 32)   # or put it in .env next to docker-compose.yml
docker compose up -d
docker compose logs -f
```

Open `http://<docker-host>:7790/`, sign in as `default` with the token as
the password, and `/login` to a provider (see [Logging in](#logging-in)).

Update with `docker compose pull && docker compose up -d` (with the default
`PRIGH_PULL_POLICY=always`, `up -d` alone pulls too). The volumes, and so
users, logins, sessions and installed tools, are kept.

### The image

The build host publishes every commit of `main` to `ghcr.io/evan-a-w/prigh`
(see [Binary cache](#binary-cache)): `:main` and `:latest` follow `main`, a
short sha (`:e9c8b23`) pins a commit. Compose runs
`${PRIGH_IMAGE:-ghcr.io/evan-a-w/prigh:main}`; an update downloads only the
changed layers.

The package must be public for anonymous pulls (GitHub: **Packages → prigh →
Package settings → Change visibility**); until then, `docker login ghcr.io`
with a token that has `read:packages`.

### Portainer

1. **Stacks → Add stack → Repository**: the repository URL, reference
   (e.g. `refs/heads/main`) and compose path `docker-compose.yml` (add
   `docker-compose.build.yml` under additional paths to build from source).
2. Under **Environment variables**, set `PRIGH_TOKEN` (or `PRIGH_TOKENS`).
3. **Deploy the stack**. "Re-pull image and redeploy" (or GitOps
   auto-update) updates it.

### Building from source

`docker-compose.build.yml` builds the image from the checkout (tagged
`prigh:local`, never pulled), with the build arguments marked "(build)" in
[Environment variables](#environment-variables):

```
docker compose -f docker-compose.yml -f docker-compose.build.yml up -d --build
# or once, in .env: COMPOSE_FILE=docker-compose.yml:docker-compose.build.yml
```

- A commit of `main` downloads the backend and frontend from the binary
  cache `attic.caduc.co/prigh` (a few hundred MB) and compiles nothing
  (`prigh: fetching backend ...`).
- What the cache lacks (a branch, or `main` before its build finished) is
  compiled after a warning (`prigh: WARNING: no binary cache ...`). Without
  the cache, compiling the OxCaml toolchain takes an hour or more and over
  8 GB of RAM.
- Building a branch repeatedly: `PRIGH_PREFETCH_DEPS=1` builds the
  toolchain in layers of its own (from `flake.nix`, `flake.lock`, `nix/`,
  `*.opam`, `dune-project`), which Docker's cache reuses while those are
  unchanged.

## Modes

`PRIGH_MODE` is a comma-separated list; one backend serves all entries, and
they share sessions.

| Entry | Port | Host port variable | What |
|---|---|---|---|
| `web` (default) | 7790 | `PRIGH_WEB_PORT` | prigh-web; also accepts `prigh-tui -connect` and `prigh tool-host -connect` |
| `server` | 7777 | `PRIGH_SERVER_PORT` | plain TCP for terminal frontends and tool hosts only |
| `tui` | – | – | an interactive TUI as the container's main process (must be the only entry) |

The old entries `prigh-web` (now `web`) and `pi-web` (removed) are refused
with a message saying so.

### TUI

- **Against the running server** (usually what you want):
  `docker compose exec prigh prigh-docker tui -user NAME -token TOKEN`
  (Portainer: **Containers → prigh → Console**). It shares the browser's
  sessions, and tools run on the user's tool host in the container. With a
  single `PRIGH_TOKEN` it signs in as `default` by itself; extra arguments go
  to `prigh-tui`. `-tools local` is refused (it runs as `prigh`); to host
  tools from a console:
  `prigh-docker as NAME prigh-tui -connect 127.0.0.1:7790 -user NAME -token TOKEN -tools local`.
- **As the container's process**: `PRIGH_MODE=tui`, then
  `docker attach prigh-prigh-1` (detach with Ctrl+P Ctrl+Q). `/quit` stops
  the container. Single-user: everything runs as `prigh` in `PRIGH_CWD`
  with the `default` namespace's logins; `PRIGH_TUI_ARGS` holds TUI flags.
- **From another machine**:
  `prigh-tui -connect <docker-host>:7790 -user NAME -token TOKEN -cwd ~/proj`
  runs tools on that machine (`-tools remote`: in the container). `prigh
  -web -connect <docker-host>:7790 -user NAME -token TOKEN -cwd ~/proj`
  serves prigh-web there the same way.

## Users

`PRIGH_TOKENS=alice=<token1>,bob=<token2>` makes several users, each signing
in with its name and token; a single `PRIGH_TOKEN` is the one user
`default`. `/signout` (or the sign-out button) returns to the sign-in form.

Each user has its own provider logins, sessions, config and tool hosts, and
a Linux user of the same name. At every start the entrypoint (as root):

- creates the Linux users (uids from 10000, stable across restarts: the
  owner of `/home/<name>`). Names: letters, digits, `_`, `-`, at most 32
  characters, not starting with a digit or `-`, and not a user or group of
  the image (`root`, `prigh`, `nobody`, …);
- creates `/home/<name>` (mode 700), the user's home and working directory;
- starts one tool host per user as that user (`-host-id container-<name>`,
  restarted if it exits); the backend runs no tools itself
  (`-no-backend-host`). New sessions adopt the user's first connected host;
  `/host` moves a session to another;
- runs the backend as `prigh`.

| Option | Effect |
|---|---|
| `PRIGH_SUPERUSERS=alice` | alice can `/setusr NAME` to act as another user (`/setusr` lists them) and clean the Nix store. Tool hosts' tokens never have superuser rights, so her agent cannot. |
| `PRIGH_NO_BACKEND_HOST=1` | no tool hosts in the container; users connect their own (below) |
| `PRIGH_ALLOW_NO_TOKEN=1` | no sign-in, one user `default` (only behind something else that authenticates) |

Connecting a tool host from your own machine (then pick it with `/host`):

```
prigh tool-host -connect <docker-host>:7790 -user <name> -token <token> -cwd ~/proj
# or with the tools in a throwaway container there (pass -host-id to keep its sessions across runs):
docker run --rm -it -v "$PWD:/work" -w /work ghcr.io/evan-a-w/prigh:main prigh tool-host -connect <docker-host>:7790 -user <name> -token <token> -cwd /work
```

## Environment variables

| Variable | Default | Meaning |
|---|---|---|
| `PRIGH_MODE` | `web` | see [Modes](#modes) |
| `PRIGH_TOKEN` | – | password of the single user `default`; this or `PRIGH_TOKENS` is **required** for network modes |
| `PRIGH_TOKENS` | – | `name=token,name2=token2`; replaces `PRIGH_TOKEN` |
| `PRIGH_SUPERUSERS` | – | comma-separated users allowed `/setusr` and Nix store cleaning |
| `PRIGH_NO_BACKEND_HOST` | – | `1`: no tool hosts in the container |
| `PRIGH_ENGINES` | `harness` | what runs sessions: `harness`, `composer` (drives Codex, Claude Code and other agents on each session's tool host: see [Engines](#engines)) or both, the first the default |
| `PRIGH_ADAPTERS` | `codex,claude` | the agents the composer drives (`codex`, `claude`, `prigh`, `acp:NAME`), e.g. `codex,claude,acp:antigravity,acp:opencode` for all the image has; users sign in to them |
| `PRIGH_SHARING` | – | `off`: users cannot share provider logins with each other |
| `PRIGH_ALLOW_NO_TOKEN` | – | `1`: start without a token |
| `PRIGH_BIND` | `0.0.0.0` | host address the ports are published on (`127.0.0.1` behind a proxy) |
| `PRIGH_WEB_PORT`, `PRIGH_SERVER_PORT` | 7790, 7777 | host ports |
| `PRIGH_ARGS` | – | extra backend arguments, e.g. `-model anthropic/claude-sonnet-4-5 -thinking high`, `-faux` |
| `PRIGH_TUI_ARGS` | – | extra `prigh-tui` arguments for `PRIGH_MODE=tui` |
| `PRIGH_CWD` | `/home/prigh` | working directory, `PRIGH_MODE=tui` only |
| `PRIGH_NIX_GC_INTERVAL` | `12h` | scheduled Nix GC: number with `s`/`m`/`h`/`d`; `0` or `off`: never |
| `PRIGH_NIX_GC_KEEP_DAYS` | `7` | profile generations older than this are deleted first; `0`: all but current; `off`: keep all |
| `PRIGH_NIX_MIN_FREE` | `15%` | Nix's `min-free` on `/nix`: size (`20G`, `512M`) or percentage; `0`: off |
| `PRIGH_NIX_MAX_FREE` | `30%` | Nix's `max-free`; same forms, more than `PRIGH_NIX_MIN_FREE` |
| `PRIGH_VOLUME_WAIT` | `2m` | how long the start waits for `/home` and `/nix` to be writable; `0`: not at all |
| `GH_TOKEN` | – | GitHub token for `git` over HTTPS, shared by users without one in `PRIGH_GH_TOKENS` |
| `PRIGH_GH_TOKENS` | – | per-user GitHub tokens, `name=token,...` |
| `PRIGH_GITHUB_CLIENT_ID` | – | your GitHub OAuth app's client id (Device Flow on), for `/login github` |
| `PRIGH_GIT_NAME`, `PRIGH_GIT_EMAIL` | – | commit author written to a user's `~/.gitconfig` if it has none |
| `PRIGH_IMAGE` | `ghcr.io/evan-a-w/prigh:main` | image to run (`:<short sha>` pins a commit) |
| `PRIGH_PULL_POLICY` | `always` | compose `pull_policy`: `always` checks at every `up`, `missing` only on `docker compose pull` |
| `PRIGH_PREFETCH_DEPS` | `0` | (build) `1`: build the toolchain in its own layers first |
| `PRIGH_UID`, `PRIGH_GID` | 1000 | (build) uid/gid of `prigh`; `/home/prigh` is chowned at start if it differs |
| `PRIGH_NIX_SUBSTITUTER`, `PRIGH_NIX_SUBSTITUTER_KEY` | `https://attic.caduc.co/prigh` and its key | (build) binary caches, space-separated; empty substituter: compile everything |
| `PRIGH_EXTRA_APT_PACKAGES` | – | (build) Debian packages baked into the image, e.g. `python3 build-essential` |
| `PRIGH_AGENT_CLIS` | `codex,claude,antigravity,opencode` | (build) the agent CLIs baked into the image ([Engines](#engines)); `none`: none |

Provider keys in the environment (`ANTHROPIC_API_KEY`, …) are ignored when
there are tokens; they apply only with `PRIGH_ALLOW_NO_TOKEN=1` or
`PRIGH_MODE=tui` (add them under `environment:` in `docker-compose.yml`).

## Volumes and data

| Path | Volume | Contents |
|---|---|---|
| `/home/prigh` | `prigh-home` | the backend's sessions, provider logins, config, prompt history (per user under `.prigh/namespaces/<name>/`) |
| `/home/<name>` | `prigh-home` | user's home and projects (clone here; `/cd` switches), `~/.ssh`, `~/.gitconfig` |
| `/nix` | `prigh-nix` | the shared Nix store, plus prigh's runtime seeded from the image |
| `/run/prigh` | tmpfs | the users (`extrausers/passwd`, `group`, `shadow`), rebuilt each start |

- `docker compose down -v` deletes the volumes, logins and sessions included.
- `prigh-nix` alone can be removed with the stack down; the next start seeds
  a fresh store and users reinstall their packages.
- Bind mounts on a separate disk: make sure the host mounts it read-write
  before Docker starts (systemd: `RequiresMountsFor=` on `docker.service`);
  otherwise the start gives up after `PRIGH_VOLUME_WAIT`, saying which.

**Files in and out.** In prigh-web, the folder button (Ctrl+Shift+F)
browses the session's directory: download files, or directories as
`.tar.gz`; upload or drop files into it. From the Docker host:

```
docker compose cp prigh:/home/alice/proj ./proj
docker compose cp ./proj prigh:/home/alice/proj && docker compose exec prigh chown -R alice: /home/alice/proj
```

## Logging in

Provider logins belong to a user: `/login` in either UI, or from the host:

```
docker compose exec prigh prigh-docker login alice anthropic                   # Claude Pro/Max (OAuth)
docker compose exec prigh prigh-docker login alice anthropic -method api_key
docker compose exec prigh prigh-docker login alice openai-codex                # ChatGPT (OAuth)
docker compose exec prigh prigh-docker login default deepseek                  # API key, single PRIGH_TOKEN
```

The OAuth redirect goes to `localhost` on your machine, not the container:
when the page fails to load, copy the full URL from the address bar and
paste it at the prompt. Credentials live under `/home/prigh` (readable only
by `prigh`) and survive rebuilds.

## Engines

The image has the coding-agent CLIs on every user's `PATH`: Codex
(`codex`), Claude Code (`claude`), Antigravity (`agy`) and OpenCode
(`opencode`), from the flake's `nixpkgs-agents` (nixos-unstable, so newer
than `nixpkgs`; Claude Code and Antigravity are unfree). To run sessions on
them as well as on the harness:

```
PRIGH_ENGINES=harness,composer
PRIGH_ADAPTERS=codex,claude,acp:antigravity,acp:opencode
```

(the first engine is the default for new sessions; `composer` alone serves
only the agents). Users and settings are the same on either engine.

- Each agent runs on the session's tool host as the user who owns it, and
  signs in as that user, from their terminal in prigh-web: `codex login`,
  `claude auth login`, `opencode auth login`; for Antigravity, `/login
  google-antigravity` in prigh (or `agy acp --login`, then `/adapters
  import antigravity`). Logins (`~/.codex`, `~/.claude`, …) stay in the
  user's home. Settings → Engines (`/adapters` in the TUI) shows each CLI's
  state per host and what to run next.
- The CLIs are part of the image's Nix seed (a layer of their own, which
  changes only when `nixpkgs-agents` does): about 1.9 GB unpacked, copied
  into `/nix` at the first start of an image that has new ones. Building
  from source, `PRIGH_AGENT_CLIS` picks them (`none` for none). A user's
  `nix profile install nixpkgs#codex` (or any other agent) comes first on
  their `PATH`.
- On a tool host outside the container (`PRIGH_NO_BACKEND_HOST=1`),
  install the CLIs on that machine.
- Codex's sandbox (bubblewrap) needs user namespaces, which Docker's
  default seccomp profile forbids without `CAP_SYS_ADMIN`, so in the
  container Codex cannot sandbox the commands it runs. Use Codex in the
  Auto access mode (no sandbox, prigh's approval rules still apply), or
  give the container a seccomp profile that allows `unshare` and `clone`
  with `CLONE_NEWUSER` if the host's kernel permits unprivileged user
  namespaces. The container's per-user isolation still applies either way.

## Git and SSH

A user's agent can use all of that user's credentials, so give it scoped,
revocable ones.

- **SSH deploy key**, generated in the container:
  `docker compose exec prigh prigh-docker ssh-key alice` creates
  `~/.ssh/id_ed25519` if missing and prints the public key (NAME may be
  omitted with one user). Add it under the repository's **Settings → Deploy
  keys** ("Allow write access" only if the agent should push).
- **HTTPS token**: `PRIGH_GH_TOKENS=alice=<token>` (or a shared `GH_TOKEN`)
  set to a fine-grained personal access token limited to the repositories
  needed. A credential helper in `/etc/gitconfig` passes it to `git` for
  `https://github.com/...`.

GitHub's host keys are pinned; other hosts are trusted on first use
(`StrictHostKeyChecking accept-new`). `GIT_TERMINAL_PROMPT=0` makes `git`
fail rather than prompt. Set `PRIGH_GIT_NAME`/`PRIGH_GIT_EMAIL` for a commit
author (a user's own `git config --global` wins).

## Installing tools

The image ships `bash git ripgrep tmux curl jq less openssh-client procps`,
ImageMagick's `magick`, the agent CLIs ([Engines](#engines)) and Nix. The root filesystem is read-only and users
are unprivileged, so there is no apt or sudo; any user gets tools from
nixpkgs as itself:

```
nix shell nixpkgs#python3 nixpkgs#nodejs -c python3 script.py   # for one command
nix run nixpkgs#cowsay -- hi                                      # a package's program
nix search nixpkgs ripgrep                                        # find a package
nix profile install nixpkgs#gh                                    # keep it on PATH
```

- `nixpkgs` is pinned to prigh's `flake.lock`, so all users get the same
  versions; packages land in the shared `prigh-nix` volume and survive
  upgrades. Store paths are world-readable: nothing secret belongs there.
- Writes go through `nix-daemon`. Users are not trusted by Nix (no extra
  substituters or keys); what `cache.nixos.org` lacks is built locally as
  `nixbld1`…`nixbld8`, without a sandbox (see [Isolation](#isolation)).
- The entrypoint seeds prigh's own store paths from `/opt/nix-seed` at
  every start. If a new image's older Nix refuses the volume's database,
  `docker volume rm <project>_prigh-nix`.
- `PRIGH_EXTRA_APT_PACKAGES` bakes Debian packages in at build time.

### Garbage collection

What users installed with `nix profile` (current generations) stays; what
they only used with `nix shell`/`nix run` is garbage. Three things collect:

| When | What |
|---|---|
| While Nix fetches or builds | with less than `PRIGH_NIX_MIN_FREE` free on `/nix`'s filesystem, deletes unused paths until `PRIGH_NIX_MAX_FREE` is free. The defaults (15%, 30%) are clamped to 1–50 GiB and 3–100 GiB; values you set are used as given. Written to `/run/prigh/nix.conf` at start. |
| Every `PRIGH_NIX_GC_INTERVAL` | deletes each user's profile generations older than `PRIGH_NIX_GC_KEEP_DAYS` (as that user), then `nix store gc` as root. First run at least 5 minutes after start, counted from the last run. |
| A superuser asks | prigh-web's Settings (Nix store), `/nix-gc run [DAYS\|all]`, or from the host `docker compose exec prigh prigh-docker nix-gc [keep\|all\|Nd]` |

Only Nix's own work triggers the first: if something else fills a shared
disk, raise the thresholds or shorten the interval. Deleted generations
cannot be rolled back to. One collection runs at a time; the last is
recorded in `/nix/var/prigh/last-gc` and each is logged.

The backend (as `prigh`) asks the entrypoint's root helper through the FIFO
`/run/prigh/nix-gc/requests` (writable only by the `prigh` group), which
accepts nothing but `gc ID keep|all|Nd USER` lines. `-nix-gc-helper DIR`
(`$PRIGH_NIX_GC_HELPER`) points the backend at it.

## Isolation

| | own user | other users |
|---|---|---|
| `/home/<name>` (700) | read/write | no access |
| `/home/prigh` (700: sessions, logins, tokens) | no access | no access |
| `/tmp` (shared tmpfs, 1777) | its own files | files others make world-readable |
| `/nix/store` | read | read |
| processes | full control | visible in `ps`, not their environment |

- `read_only: true`: only the two volumes, `/tmp` and `/run` are writable.
  No host bind mounts, no Docker socket.
- All capabilities are dropped except `CHOWN`, `DAC_OVERRIDE`, `FOWNER`,
  `SETUID`, `SETGID` and `KILL`, which the entrypoint and `nix-daemon` need.
  Everything else (backend, tool hosts, `prigh-docker` commands) switches
  uid and has none; `no-new-privileges` keeps setuid binaries from
  regaining any.
- Without `CAP_SYS_ADMIN` Nix has no build sandbox: a user could make a
  local build impure, and another user of that store path would get it.
  Substituted (signed) paths are unaffected. Don't put mutually
  distrusting users in one container if that matters.
- Only the backend holds tokens and provider logins; a tool host has its
  user's own token (no superuser rights) and that user's GitHub token.
- Network access is unrestricted, including to the backend's ports; use an
  isolated Docker network if that matters.
- The token is the only authentication and traffic is plain HTTP/WS. Outside
  a trusted LAN, set `PRIGH_BIND=127.0.0.1` and put a TLS proxy (below) or
  an SSH tunnel in front.

## Behind a reverse proxy

Publish the port on loopback (`PRIGH_BIND=127.0.0.1`) and let a proxy
terminate TLS. Browsers open `https://prigh.example.com/`; other machines
connect to the same site:

```
prigh-tui -connect https://prigh.example.com/ -token T ~/proj     # = wss://prigh.example.com/ws
prigh tool-host -connect wss://prigh.example.com/ws -token T -cwd ~/proj
```

The proxy must:

- pass WebSocket upgrades (`Upgrade`, `Connection`) on `/ws` (RPC) and
  `/terminal` (the browser terminal);
- allow a read timeout of a minute or more (prigh pings every 20s);
- pass `Host` (or set `X-Forwarded-Host`) and `Cookie`;
- use a certificate the clients' system CAs trust (`$SSL_CERT_FILE` points
  prigh-tui at another bundle).

On a tailnet, `tailscale serve --bg 7790` on the Docker host is the shortest
route to HTTPS. HTTPS also lets prigh-web be installed as an app; keep the
site at the root of its host (the service worker's scope).

### Proxies that sign users in

With authentik, Authelia, oauth2-proxy, Cloudflare Access and the like, the
simplest fix is to **exempt `/ws` and `/terminal` from the sign-in**: prigh
checks its own token there, and `/` can stay behind it.

- **authentik**: in the provider's *Advanced protocol settings →
  Unauthenticated Paths*, add `^/(ws|terminal)(\?.*)?$` (in *Forward auth
  (domain level)* mode, the full URL:
  `^https://prigh\.example\.com/(ws|terminal)(\?.*)?$`).
- **Caddy** with `forward_auth`:

  ```
  prigh.example.com {
      @prigh_sockets path /ws /terminal
      handle @prigh_sockets {
          reverse_proxy 127.0.0.1:7790
      }
      handle {
          forward_auth authentik-server:9000 {
              uri /outpost.goauthentik.io/auth/caddy
              copy_headers X-Authentik-Username
          }
          reverse_proxy 127.0.0.1:7790
      }
  }
  ```

- **nginx** with `auth_request`:

  ```
  location ~ ^/(ws|terminal)$ {
      auth_request off;
      proxy_pass http://127.0.0.1:7790;
      proxy_http_version 1.1;
      proxy_set_header Upgrade $http_upgrade;
      proxy_set_header Connection "upgrade";
      proxy_set_header Host $host;
      proxy_read_timeout 1h;
  }
  location / {
      auth_request /outpost.goauthentik.io/auth/nginx;
      # ... the rest of the sign-in setup
      proxy_pass http://127.0.0.1:7790;
  }
  ```

- **Traefik**: a higher-priority router for the sockets without the
  sign-in middleware:

  ```
  traefik.http.routers.prigh.rule=Host(`prigh.example.com`)
  traefik.http.routers.prigh.middlewares=authentik@docker
  traefik.http.routers.prigh-sockets.rule=Host(`prigh.example.com`) && (Path(`/ws`) || Path(`/terminal`))
  traefik.http.routers.prigh-sockets.priority=100
  traefik.http.services.prigh.loadbalancer.server.port=7790
  ```

Otherwise:

- **Browser sign-in.** prigh-tui, `prigh web` and `prigh tool-host` notice
  the proxy's login page and sign in through the browser: they open
  `https://prigh.example.com/#cli-signin=…`, you sign in to the proxy and
  *Allow*, and prigh-web hands over the proxy's session cookie, saved in
  `~/.prigh/tui-connection.sexp` until it expires. Over SSH (`SSH_CONNECTION`
  or `SSH_TTY` set) the page shows a `prigh1:…` code to paste into the
  terminal instead; `PRIGH_SIGNIN_PASTE=1`/`0` overrides, or forward the
  printed port (`ssh -L PORT:127.0.0.1:PORT host`). `prigh browser-signin
  -connect URL` does it by hand (`-paste` forces the code).
- **Proxy credentials**: `prigh-tui -header 'NAME: VALUE'` (e.g. a service
  token; `$PRIGH_CONNECT_HEADERS` for several, one per line).
- **An SSH tunnel** to the published port.

## Other commands

`docker compose exec` runs as root and skips the entrypoint; prefix commands
with `prigh-docker` to drop root:

```
docker compose exec prigh prigh-docker users                 # name, uid, home, superuser
docker compose exec -it prigh prigh-docker as alice bash     # a shell as alice
docker compose exec prigh prigh-docker as alice nix profile list
docker compose exec prigh prigh-docker prigh sessions list   # any other command runs as prigh
docker compose exec prigh prigh-docker nix-gc 30d            # collect now, deleting generations over 30 days
docker compose run --rm prigh bash                           # likewise, in a new container
```

Plain `docker compose exec prigh CMD` is a root shell, for administration
(e.g. `chown`).

## Migrating from the single-user layout

Older images ran everything as `prigh` with `prigh-home` at `/home/prigh`.
On the first start of a new image:

- If the volume's root holds the old home (`.prigh` or `.config`) and there
  is no `/home/prigh`, the entrypoint moves it there, keeping logins and
  sessions (the `default` user's with a single `PRIGH_TOKEN`).
- The old SSH key and git config stay in `/home/prigh`, out of agents'
  reach: create per-user keys with `prigh-docker ssh-key NAME`.
- Remove any `user:` override: the container must start as root.

## Migrating from /workspace

Older images mounted `prigh-workspace` (or a host path in
`PRIGH_WORKSPACE`) at `/workspace`, with projects in `/workspace/<name>`.
Users now work in their homes. If a `/workspace/<name>` is still mounted,
the entrypoint logs the command that moves each one. Otherwise, after the
new image has started once (so the homes exist), copy the old volume with
the stack down:

```
docker compose down
docker run --rm -v <project>_prigh-home:/home -v <project>_prigh-workspace:/workspace debian:bookworm-slim \
  sh -c 'for d in /workspace/*/; do n=$(basename "$d"); [ -d "/home/$n" ] || { echo "skipping $d: no /home/$n"; continue; }; cp -a "$d." "/home/$n/" && chown -R "$(stat -c %u:%g "/home/$n")" "/home/$n"; done'
docker compose up -d
```

Use `-v /host/path:/workspace` for a bind mount; `docker volume ls` shows the
names. Directories not named after a user are skipped: copy them by hand.
Then `docker volume rm <project>_prigh-workspace`, and `/cd` sessions that
were in `/workspace/<name>/...` to their new place.

## Troubleshooting

The backend logs to the container's output:
`docker compose logs --timestamps --since 2h prigh`. Save it before
recreating a stuck container (`docker compose logs --timestamps prigh >
prigh.log`), as redeploying discards it.

| Symptom or log line | Meaning / fix |
|---|---|
| `prigh: closing the connection of prigh-web: …` | the backend ended a connection: `the peer is not reading what is sent to it`, `cannot write: …`, or `a newer connection from the same page took over` |
| `prigh: request from … failed: …` | a request raised an exception (the connection stays) |
| `prigh: web connection failed: …` | a connection ended with an error |
| browser keeps reconnecting behind a proxy | check WebSocket upgrades and read timeouts on `/ws` and `/terminal` ([Behind a reverse proxy](#behind-a-reverse-proxy)) |
| clients get a login page or HTTP 302/401 instead of the WebSocket | the proxy signs users in: exempt `/ws` and `/terminal` |
| start waits, then exits about `/home` or `/nix` | the volume is not writable; check the host mount (`PRIGH_VOLUME_WAIT`) |
| startup line says `nix: NOT RUNNING` | `nix-daemon` keeps failing; prigh runs without Nix installs |
| `/workspace/<name> is from an older image` | see [Migrating from /workspace](#migrating-from-workspace) |
| `PRIGH_MODE=prigh-web is now PRIGH_MODE=web` | rename the mode |

The backend closes a WebSocket whose peer answers nothing for 45 seconds or
whose writes are stuck for 5 minutes.

## Binary cache

A self-hosted build host ([Komodo](https://komo.do)) builds every push to
`main` within minutes: prigh, prigh-desktop, prigh-computer, their
dependencies and the dev shells' inputs, pushed to the Attic cache `prigh`
(`attic.caduc.co/prigh`). It then builds the image from its own store
(backend and frontend as NARs in the `prebuilt` build context, with
`REQUIRE_CACHED=1`) and pushes it to `ghcr.io/evan-a-w/prigh`. Pull requests
run nothing there.

To push by hand (Nix, enough RAM, and `attic login` with a push token):

```
nix build --no-link --print-out-paths path:.#frontend.inputDerivation path:.#backend.inputDerivation \
  | xargs nix run nixpkgs#attic-client -- push prigh
```

A stale cache makes builds slower, never broken.

## How the image is built

1. A `nixos/nix` stage (`docker/fetch-prigh.sh`) fetches `.#backend` and
   `.#frontend` (from `prebuilt` NARs or the cache, without their ~3 GB
   toolchain closure; built if missing) and keeps the binaries, prigh-web's
   site and the Nix seed (glibc, gmp, `nix`, `imagemagick`). Before that,
   `docker/fetch-agent-clis.sh` builds the agent CLIs (`AGENT_CLIS`) from
   the flake alone, for a seed layer of their own.
2. The runtime stage is `debian:bookworm-slim`, with `tini -g` as PID 1 and
   `libnss-extrausers` for users added at runtime on the read-only root.
