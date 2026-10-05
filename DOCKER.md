# Running prigh in Docker

`Dockerfile` + `docker-compose.yml` at the repository root run the prigh
backend in a container serving the browser UI, pi's web UI, terminal
frontends, or an interactive TUI. They are chosen with environment
variables.

Each user (namespace) is its own Linux user in the container. Its agent's
tools (bash, edits, the `>_` shell) run as that user, in
`/workspace/<name>`, with its own home `/home/<name>`. The backend runs as
the separate `prigh` service user, and nothing but `nix-daemon` runs as root
after startup. Agents install the tools they need with Nix, into a store
shared by all users. See [Users](#users), [Installing tools](#installing-tools)
and [Isolation](#isolation).

## Portainer

1. **Stacks → Add stack → Repository**. Fill in the repository URL, the
   reference (e.g. `refs/heads/docker`) and the compose path
   `docker-compose.yml`.
2. Under **Environment variables**, set at least `PRIGH_TOKEN`, a long
   random secret (`openssl rand -hex 32`), or `PRIGH_TOKENS` for several
   users (see [Users](#users)).
3. **Deploy the stack**. Portainer clones the repository and builds the
   image.
4. Open `http://<docker-host>:7788/`, sign in (with `PRIGH_TOKEN` the user
   is `default` and the token is the password), and `/login` to a provider
   (see [Logging in](#logging-in)).

**The first build downloads a few GB.** The OxCaml toolchain and packages
come from the binary cache `prigh.cachix.org` instead of being compiled.
Without the cache, compiling them takes an hour or more and over 8 GB of
RAM. The cache must be re-pushed when the dependencies change (see
[Binary cache](#binary-cache)). Later builds reuse the cached dependency layer whenever
`flake.nix`, `flake.lock`, `nix/` and the `*.opam`/`dune-project` files
are unchanged. In that case only prigh itself is rebuilt, which takes a
few minutes. Redeploying with "Re-pull image and redeploy" (or the
GitOps auto-update) rebuilds from the new commit.

## Docker Compose

```
export PRIGH_TOKEN=$(openssl rand -hex 32)   # or put these in a .env file next to docker-compose.yml
docker compose up -d --build
docker compose logs -f
```

## Modes

`PRIGH_MODE` is a comma-separated list of what to serve. One backend
serves all of them, and they share sessions:

| Entry | Container port | Host port variable | What |
|---|---|---|---|
| `web` (default) | 7788 | `PRIGH_WEB_PORT` | the Bonsai browser UI; also accepts terminal frontends and tool hosts (`prigh-tui -connect`, `prigh tool-host -connect`) |
| `pi-web` | 7789 | `PRIGH_PI_WEB_PORT` | pi's web UI (`pi-web/`) |
| `server` | 7777 | `PRIGH_SERVER_PORT` | plain TCP for terminal frontends and tool hosts only |
| `tui` | – | – | an interactive TUI as the container's main process (must be the only entry; single-user, see below) |

For example, `PRIGH_MODE=web,pi-web` serves both web UIs.

### TUI

There are two ways to get a TUI:

- **Against the running server.** This works with any network mode and is
  usually what you want: `docker compose exec prigh prigh-docker tui -user
  NAME -token TOKEN` or, in Portainer, **Containers → prigh → Console**,
  with that command. It sees the same sessions as the browser (`/sessions`,
  `-session ID` to join one), and the tools run on the user's tool host in
  the container (`-tools remote`). With a single `PRIGH_TOKEN` it signs in
  as `default` by itself. Extra arguments go to `prigh-tui`. `-tools local`
  is refused, because the TUI runs as the `prigh` service user; to host
  tools from a console, run it as the user:
  `prigh-docker as NAME prigh-tui -connect 127.0.0.1:7788 -user NAME -token TOKEN -tools local`.
- **As the container's process.** Set `PRIGH_MODE=tui` and attach to the
  container: `docker attach prigh-prigh-1` (detach with Ctrl+P Ctrl+Q), or
  **Attach** in Portainer. No ports are used, and `/quit` stops the
  container until it is restarted. `PRIGH_TUI_ARGS` holds TUI flags
  (e.g. `-model ...`). This mode is **single-user**: there are no
  per-namespace users, and the TUI, its backend and its tools all run as
  `prigh` in `PRIGH_CWD` (default `/workspace/prigh`), with the logins of
  the `default` namespace.

From another machine, connect to the web or server port:
`prigh-tui -connect <docker-host>:7788 -user NAME -token TOKEN -cwd ~/proj`.
By default the tools then run on *that* machine. Add `-tools remote` to run
them on the user's tool host in the container instead.

## Users

Set `PRIGH_TOKENS=alice=<token1>,bob=<token2>` instead of `PRIGH_TOKEN`.
Each name is a user: it signs in with the name and its token as the
password (the status line then shows `user:<name>`; `/signout` or the
`sign out` button next to `>_` returns to the sign-in form). A single
`PRIGH_TOKEN` is the one user `default`.

Each user is a namespace of the backend, with its own provider logins,
sessions, config and connected tool hosts, and a Linux user of the same
name in the container. At startup, the entrypoint (as root):

- creates the Linux users. Names must be valid user names (letters, digits,
  `_`, `-`, at most 32 characters, not starting with a digit or `-`), and
  must not be a user or group of the image (`root`, `prigh`, `nobody`, …).
  A user keeps its uid across restarts and reorderings: it is the owner of
  `/home/<name>` (uids start at 10000);
- creates `/home/<name>` (mode 700) and `/workspace/<name>` (mode 2770, the
  user's group) for each user;
- starts one tool host per user, running as that user in
  `/workspace/<name>`, and restarts it if it exits. The backend runs no
  tools itself (`-no-backend-host`). A new session adopts its user's first
  connected tool host, normally this one; `/host` picks another;
- runs the backend as `prigh`.

`prigh-docker users` lists the users, their uids and directories.

**Superusers.** `PRIGH_SUPERUSERS=alice` makes alice a superuser. `/setusr
NAME` in any of the UIs switches her connection to user NAME (its sessions,
logins and tool host, so its tools run as NAME), `/setusr` lists the users
and `/setusr alice` switches back; other users get an error.
Superusers are also in every user's group, so from their own shell they can
read the other users' `/workspace/<name>` (and write where files are
group-writable), but not their homes.
The container tool hosts sign in with separate per-user tokens that never
have superuser rights, so a superuser's agent cannot use `/setusr`.

**No tools in the container.** With `PRIGH_NO_BACKEND_HOST=1` no tool hosts
are started, so nothing model-controlled runs in the container. Each user
connects a tool host from their own machine, which runs that user's tools
and `>_` terminal:

```
prigh tool-host -connect <docker-host>:7788 -user <name> -token <their token> -cwd ~/proj
# or, with the tools themselves in a throwaway container on that machine:
docker run --rm -it -v "$PWD:/work" -w /work <image> prigh tool-host -connect <docker-host>:7788 -user <name> -token <their token> -cwd /work
```

Then `/host` in the web UI (or TUI) picks it. A TUI started with
`prigh-tui -connect` is a tool host too, while it is open.

**Without a token** (`PRIGH_ALLOW_NO_TOKEN=1`) there is no sign-in and one
user, `default`.

## Environment variables

| Variable | Default | Meaning |
|---|---|---|
| `PRIGH_MODE` | `web` | see above |
| `PRIGH_TOKEN` | – | password of the single user `default`. **Required** for network modes (or `PRIGH_TOKENS`) |
| `PRIGH_TOKENS` | – | several users, `name=token,name2=token2` (see [Users](#users)); replaces `PRIGH_TOKEN` |
| `PRIGH_SUPERUSERS` | – | comma-separated users that can switch to any user with `/setusr` |
| `PRIGH_NO_BACKEND_HOST` | – | `1`: no tool hosts in the container; tools and terminals run only on hosts the users connect |
| `PRIGH_ALLOW_NO_TOKEN` | – | set to `1` to start without a token (only behind something else that authenticates) |
| `PRIGH_BIND` | `0.0.0.0` | host address the ports are published on (`127.0.0.1` keeps them local, e.g. behind a reverse proxy) |
| `PRIGH_WEB_PORT`, `PRIGH_PI_WEB_PORT`, `PRIGH_SERVER_PORT` | 7788, 7789, 7777 | host ports |
| `PRIGH_WORKSPACE` | volume `prigh-workspace` | host path to bind at `/workspace` instead of the named volume. It must be world-searchable (`o+x`) so that users reach their `/workspace/<name>` |
| `PRIGH_ARGS` | – | extra backend arguments, e.g. `-model anthropic/claude-sonnet-4-5 -thinking high`, or `-faux` for a scripted provider with no API calls |
| `PRIGH_TUI_ARGS` | – | extra `prigh-tui` arguments for `PRIGH_MODE=tui` |
| `PRIGH_CWD` | `/workspace/prigh` | working directory for `PRIGH_MODE=tui` only |
| `GH_TOKEN` | – | GitHub token for `git` over HTTPS, for users without one in `PRIGH_GH_TOKENS` (shared by all of them; see [Git and SSH](#git-and-ssh)) |
| `PRIGH_GH_TOKENS` | – | per-user GitHub tokens, `name=token,...` |
| `PRIGH_GIT_NAME`, `PRIGH_GIT_EMAIL` | – | commit author written to a user's `~/.gitconfig` at start if it has none yet |
| `PRIGH_UID`, `PRIGH_GID` | 1000 | (build) uid/gid of the `prigh` service user. `/home/prigh` is chowned to it at start if it differs |
| `PRIGH_NIX_SUBSTITUTER`, `PRIGH_NIX_SUBSTITUTER_KEY` | `https://prigh.cachix.org` and its key | (build) the Nix binary cache. Set the substituter to an empty value to compile everything |
| `PRIGH_EXTRA_APT_PACKAGES` | – | (build) extra Debian packages baked into the image, e.g. `python3 build-essential` |

## Installing tools

The image ships `bash git ripgrep tmux curl jq less openssh-client procps`
and Nix. The agents run as unprivileged users on a read-only root
filesystem, so apt and sudo are out; any user (agent or `>_` shell) gets
other tools from nixpkgs instead, as itself:

```
nix shell nixpkgs#python3 nixpkgs#nodejs -c python3 script.py   # for one command
nix run nixpkgs#cowsay -- hi                                      # a package's program
nix search nixpkgs ripgrep                                        # find a package's name
nix profile install nixpkgs#gh                                    # keep it on PATH (~/.nix-profile/bin)
```

The system prompt tells the model this whenever its tool host has `nix` on
PATH. Packages come prebuilt from `cache.nixos.org` and land in `/nix`, the
`prigh-nix` volume, shared by all users: a package one user fetched is there
for the others, and everything survives restarts and upgrades. `nixpkgs`
means the revision in prigh's `flake.lock` (pinned in
`/etc/nix/flake-registry.json`), so every user sees the same package
versions; the first `nixpkgs#` use per user unpacks it (a few seconds), and
the first `nix search` evaluates all of nixpkgs (slow). Other flakes work
too (`nix run github:owner/repo`).

`PRIGH_EXTRA_APT_PACKAGES` still adds Debian packages at build time, for
tools that every user should have without asking.

How it fits together:

- **The daemon.** All store writes go through `nix-daemon`, which the
  entrypoint starts as root (and restarts if it exits; if it keeps failing,
  prigh runs on without it and the startup line says `nix: NOT RUNNING`).
  Every user's environment (tool hosts, `prigh-docker as`) has
  `NIX_REMOTE=daemon` and `~/.nix-profile/bin` on PATH.
- **Users are not trusted** by Nix (`/etc/nix/nix.conf`): they cannot add
  substituters or keys or change the sandbox settings, so everything they
  substitute is signed by `cache.nixos.org`. Packages not in the cache are
  built locally, as the `nixbld1`…`nixbld8` users.
- **Seeding.** prigh itself links against store paths (glibc), and nix is a
  store path too. Since the volume hides the image's `/nix`, the image keeps
  them in `/opt/nix-seed`, and the entrypoint copies any that are missing
  into `/nix/store` and registers them at every start (the first start
  copies about 160 MB). `/usr/local/bin/nix*` link to the seeded nix.
- **Garbage collection.** When Nix fetches or builds with less than 2 GiB
  free on the volume's filesystem, it deletes unused paths until 8 GiB is
  free. What users
  installed with `nix profile` stays; what they only used with `nix shell`
  or `nix run` may be deleted and is fetched again when needed. GC roots in
  `/nix/var/nix/gcroots/prigh` keep the paths prigh and nix need. Collect by
  hand with `docker compose exec prigh nix store gc`.
- **Upgrades.** A new image brings its seed; the entrypoint adds what is new
  and moves the GC roots to it, so the old image's paths become garbage.
  Users' profiles and packages are kept. If an image ever ships an older
  Nix than the volume was used with, Nix may refuse the newer database: then
  remove the volume (`docker volume rm <project>_prigh-nix`; users reinstall
  their profile packages).

## Logging in

Provider logins belong to a user. Use `/login` in either UI, or a console
in the container, naming the user:

```
docker compose exec prigh prigh-docker login alice anthropic                   # Claude Pro/Max (OAuth)
docker compose exec prigh prigh-docker login alice anthropic -method api_key
docker compose exec prigh prigh-docker login alice openai-codex                # ChatGPT (OAuth)
docker compose exec prigh prigh-docker login alice openai                      # API key
docker compose exec prigh prigh-docker login default deepseek                  # API key, single PRIGH_TOKEN
```

The OAuth redirect goes to `localhost` on the machine running the browser,
which is not the container. The page fails to load, so copy the full URL
from the browser's address bar and paste it at the prompt. Credentials are
stored in `/home/prigh/.prigh/namespaces/<name>/.config/prigh/auth.json`
(`/home/prigh/.config/prigh/auth.json` for `default`), in the home volume,
so they survive rebuilds. Only the backend (`prigh`) can read them.

Provider keys in the environment (`ANTHROPIC_API_KEY`, …) are ignored when
there are tokens, since every user has its own logins. They only apply with
`PRIGH_ALLOW_NO_TOKEN=1` or `PRIGH_MODE=tui`; add them under `environment:`
in `docker-compose.yml` for those.

## Git and SSH

Each user's agent can use every credential of that user: its `~/.ssh`,
`~/.gitconfig` and `GH_TOKEN`. Give it scoped, revocable credentials rather
than your own keys. There are two supported ways.

**SSH deploy key.** The key is generated inside the container and never
leaves it:

```
docker compose exec prigh prigh-docker ssh-key alice     # or the Portainer console
```

This creates alice's `~/.ssh/id_ed25519` (NAME may be omitted when there is
only one user) if it doesn't exist yet, and prints the public key. Users can
also run `ssh-keygen` and `git config --global` in their own `>_` terminal.
Add the key to each repository under **Settings → Deploy keys**, and tick
"Allow write access" only if the agent should push. A deploy key works for
one repository only; for several, use a machine user's account key, or
HTTPS with a token.

**HTTPS with a token.** Set `PRIGH_GH_TOKENS=alice=<token>,...` (or
`GH_TOKEN`, which every user without its own token then shares) to a
[fine-grained personal access token](https://github.com/settings/personal-access-tokens)
limited to the repositories (and permissions, e.g. Contents read/write)
the agent needs. A credential helper in `/etc/gitconfig` hands it to `git`
for `https://github.com/...`; `gh` reads `GH_TOKEN` too if you install it.

For both:

- GitHub's host keys are pinned in `/etc/ssh/ssh_known_hosts`. Other hosts
  are trusted on first use (`StrictHostKeyChecking accept-new`), because
  the agent cannot answer a prompt. `GIT_TERMINAL_PROMPT=0` makes `git`
  fail instead of waiting for a password.
- Set `PRIGH_GIT_NAME` / `PRIGH_GIT_EMAIL` so that commits have an author
  (a user's own `git config --global user.name` wins).

## Isolation

What a user's agent (and `>_` shell) can and cannot see:

| | own user | other users |
|---|---|---|
| `/home/<name>` (mode 700) | read/write | no access |
| `/workspace/<name>` (mode 2770) | read/write | no access, except superusers (group members) |
| `/home/prigh` (mode 700: sessions, provider logins, tokens) | no access | no access |
| `/tmp` (shared tmpfs, mode 1777) | its own files | files others make world-readable |
| `/nix/store` (written only by `nix-daemon`) | read | read: everything in the store is world-readable, so nothing secret belongs there |
| processes | full control | visible in `ps`, but not their environment (no tokens are in command lines) |

- Agents see only the container filesystem. `read_only: true` makes
  everything outside the two volumes, `/tmp` and `/run` (tmpfs)
  unwritable. There are no bind mounts of the host unless you set
  `PRIGH_WORKSPACE`, and the Docker socket is not mounted.
- The container starts as root, with all capabilities dropped except those
  the entrypoint needs to set up the users: `CHOWN` (give users their
  directories), `DAC_OVERRIDE` (create and move directories it doesn't own,
  e.g. a bound `/workspace` or the old home layout), `FOWNER` (set the mode
  of the users' directories), `SETUID`/`SETGID` (switch to the users with
  `setpriv`) and `KILL` (`tini` forwards stop signals to processes of other
  users). Only the entrypoint, its tool host restart loops and `nix-daemon`
  run as root. The processes they start (backend, tool hosts, `prigh-docker
  as/tui/login/ssh-key`) switch uid and so have no capabilities at all, and
  `no-new-privileges` keeps setuid binaries from regaining any.
- `nix-daemon` needs no capability beyond these: it chowns and fixes the
  permissions of store paths (`CHOWN`, `FOWNER`, `DAC_OVERRIDE`), runs
  builds as the `nixbld` users (`SETUID`, `SETGID`) and kills their leftover
  processes (`KILL`). It is the one root process that model-controlled
  requests reach, through its socket, as with any multi-user Nix install.
  Without `CAP_SYS_ADMIN` there is no build sandbox (`sandbox = false`, no
  seccomp filter): a local build runs as a `nixbld` user with network
  access and sees what that user can, e.g. world-readable files in `/tmp`,
  but not the users' homes or workspaces. A user could thus make a build
  of their own impure, and another user who later uses that same store path
  gets the impure result. Substituted paths are unaffected (they are
  checked against `cache.nixos.org`'s signature), and so are paths already
  in the store. If that matters, don't let mutually distrusting users share
  a container.
- The backend holds every token and provider login. A tool host has only a
  token of its own user, without superuser rights, plus that user's
  `GH_TOKEN`; neither `PRIGH_TOKENS`, `PRIGH_GH_TOKENS` nor other users'
  secrets are in its environment.
- Network access is unrestricted: agents need it for provider APIs, and
  they can reach anything the Docker network can, including the backend's
  ports. Put the stack on an isolated network if that matters.
- The token is the only authentication, and the traffic is plain HTTP/WS.
  Outside a trusted LAN, set `PRIGH_BIND=127.0.0.1` and put a
  TLS-terminating reverse proxy (Caddy, Traefik, nginx; it must pass
  WebSockets on `/ws` and `/terminal`) or an SSH tunnel in front.

## Data

| Path | Volume | Contents |
|---|---|---|
| `/home/prigh` | `prigh-home` | the backend's data: sessions (`.prigh/sessions`, per user `.prigh/namespaces/<name>/`), provider logins, config, prompt history |
| `/home/<name>` | `prigh-home` | user `<name>`'s home: `~/.ssh`, `~/.gitconfig`, shell history |
| `/workspace/<name>` | `prigh-workspace` or `PRIGH_WORKSPACE` | user `<name>`'s default working directory. Clone projects here (`/cd` switches between them) |
| `/nix` | `prigh-nix` | the Nix store: what users installed (see [Installing tools](#installing-tools)), and prigh's own runtime, copied in from the image at start |
| `/run/prigh` | tmpfs | the users (`extrausers/passwd`, `group`, `shadow`), rebuilt at each start |

Removing the stack keeps named volumes unless they are removed
explicitly (`docker compose down -v` deletes them, logins and sessions
included). `prigh-nix` alone can be removed at any time with the stack
down: the next start seeds a fresh store, and users reinstall their
packages.

## Migrating from the single-user layout

Older images ran everything as `prigh`, with the `prigh-home` volume
mounted at `/home/prigh` and projects directly in `/workspace`. On the
first start of the new image:

- The `prigh-home` volume is now mounted at `/home`. If its root holds the
  old home (`.prigh` or `.config`) and there is no `/home/prigh`, the
  entrypoint moves everything into `/home/prigh` (and logs it), so logins
  and sessions are kept. With a single `PRIGH_TOKEN`, they are the `default`
  user's.
- A `/workspace/<name>` whose name is a user is chowned to that user (once,
  logged). Other directories in `/workspace` stay owned by `prigh`, and no
  user can reach them. Move each into a user's workspace:

  ```
  docker compose exec prigh sh -c 'mv /workspace/proj /workspace/NAME/ && chown -R NAME: /workspace/NAME/proj'
  ```
- The SSH key and git config of the old home stay in `/home/prigh`, where
  no agent can use them. Create a key per user with `prigh-docker ssh-key
  NAME` (or copy the old one into `/home/NAME/.ssh` and chown it).
- Remove any `user:` override: the container must start as root.

## Other commands

`docker compose exec` runs commands as root and skips the entrypoint, so
prefix them with `prigh-docker`, which runs them without root:

```
docker compose exec prigh prigh-docker users                 # name, uid, home, workspace, superuser
docker compose exec -it prigh prigh-docker as alice bash     # a shell as alice, in alice's clean environment
docker compose exec prigh prigh-docker prigh sessions list   # any other command runs as prigh
docker compose exec prigh prigh-docker as alice nix profile list
docker compose run --rm prigh bash                           # likewise, in a new container
```

Plain `docker compose exec prigh CMD` (e.g. a root shell) is for
administration, like the `mv`/`chown` above.

## Binary cache

When `flake.lock`, `nix/` or the `*.opam`/`dune-project` files change, the
dependencies get new hashes. Rebuild and push them from a machine that has
Nix (and enough RAM):

```
nix build --no-link --print-out-paths path:.#tui.inputDerivation path:.#backend.inputDerivation \
  | nix run nixpkgs#cachix -- push prigh
```

Anything missing from the cache is compiled, so a stale cache makes the
build slower but does not break it.

## How the image is built

1. A `nixos/nix` stage builds `.#backend` and `.#tui` from the flake. It
   first builds only their dependencies, from a copy of the files that
   determine them, so that layer stays cached. It then keeps just the two
   binaries, the web assets and, as the Nix seed, the few store paths the
   binaries link against (glibc, gmp) plus the `nix` of the `nixos/nix`
   image with its closure and their database registration (`nix-store
   --dump-db`), about 160 MB instead of the ~10 GB build closure. It also
   writes the flake registry that pins `nixpkgs` to `flake.lock`.
2. A `node` stage builds `pi-web/` with `npm ci && npm run build`.
3. The runtime stage is `debian:bookworm-slim`. `tini -g` is PID 1, so
   signals reach the backend and tool hosts and child processes are
   reaped. `libnss-extrausers` lets the entrypoint add users at runtime
   despite the read-only root (`/var/lib/extrausers` points into `/run`);
   the build checks that lookups through it work. The `nixbld` group and
   users and `/etc/nix/nix.conf` (from `docker/nix.conf`) are baked in.
