# Running prigh in Docker

`Dockerfile` + `docker-compose.yml` at the repository root run the prigh
backend in a container serving the browser UI, pi's web UI, terminal
frontends, or an interactive TUI. They are chosen with environment
variables. The agent's tools (bash, edits, the `>_` shell) run inside the
container. They can only touch two volumes, `/workspace` and the prigh
user's home, plus a scratch `/tmp`.

## Portainer

1. **Stacks → Add stack → Repository**. Fill in the repository URL, the
   reference (e.g. `refs/heads/docker`) and the compose path
   `docker-compose.yml`.
2. Under **Environment variables**, set at least `PRIGH_TOKEN`, a long
   random secret (`openssl rand -hex 32`).
3. **Deploy the stack**. Portainer clones the repository and builds the
   image.
4. Open `http://<docker-host>:7788/`, enter the token in the connect form,
   and `/login` to a provider (see [Logging in](#logging-in)).

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
| `web` (default) | 7788 | `PRIGH_WEB_PORT` | the Bonsai browser UI; also accepts terminal frontends (`prigh-tui -connect`) |
| `pi-web` | 7789 | `PRIGH_PI_WEB_PORT` | pi's web UI (`pi-web/`) |
| `server` | 7777 | `PRIGH_SERVER_PORT` | plain TCP for terminal frontends only |
| `tui` | – | – | an interactive TUI as the container's main process (must be the only entry) |

For example, `PRIGH_MODE=web,pi-web` serves both web UIs.

### TUI

There are two ways to get a TUI:

- **Against the running server.** This works with any network mode and is
  usually what you want: `docker compose exec prigh prigh-docker tui` or,
  in Portainer, **Containers → prigh → Console**, with the command
  `prigh-docker tui`. It connects to the container's own server with
  tools running in the container, so it sees the same sessions as the
  browser (`/sessions`, `-session ID` to join one). Extra arguments go to
  `prigh-tui`.
- **As the container's process.** Set `PRIGH_MODE=tui` and attach to the
  container: `docker attach prigh-prigh-1` (detach with Ctrl+P Ctrl+Q), or
  **Attach** in Portainer. No ports are used, and `/quit` stops the
  container until it is restarted. `PRIGH_TUI_ARGS` holds TUI flags
  (e.g. `-model ...`).

From another machine, connect to the web or server port:
`prigh-tui -connect <docker-host>:7788 -token $PRIGH_TOKEN -cwd ~/proj`. By
default the tools then run on *that* machine. Add `-tools remote` to run
them in the container's `/workspace` instead.

## Environment variables

| Variable | Default | Meaning |
|---|---|---|
| `PRIGH_MODE` | `web` | see above |
| `PRIGH_TOKEN` | – | shared secret that clients must present. **Required** for network modes |
| `PRIGH_ALLOW_NO_TOKEN` | – | set to `1` to start without a token (only behind something else that authenticates) |
| `PRIGH_BIND` | `0.0.0.0` | host address the ports are published on (`127.0.0.1` keeps them local, e.g. behind a reverse proxy) |
| `PRIGH_WEB_PORT`, `PRIGH_PI_WEB_PORT`, `PRIGH_SERVER_PORT` | 7788, 7789, 7777 | host ports |
| `PRIGH_WORKSPACE` | volume `prigh-workspace` | host path to bind at `/workspace` instead of the named volume |
| `PRIGH_ARGS` | – | extra backend arguments, e.g. `-model anthropic/claude-sonnet-4-5 -thinking high`, or `-faux` for a scripted provider with no API calls |
| `PRIGH_TUI_ARGS` | – | extra `prigh-tui` arguments for `PRIGH_MODE=tui` |
| `GH_TOKEN` | – | GitHub token for `git` over HTTPS (see [Git and SSH](#git-and-ssh)) |
| `PRIGH_GIT_NAME`, `PRIGH_GIT_EMAIL` | – | written to the git config in the home volume at start, for the agent's commits |
| `PRIGH_UID`, `PRIGH_GID` | 1000 | (build) uid/gid of the `prigh` user. Match the owner of a bound `PRIGH_WORKSPACE`. Docker sets a named volume's ownership only when it is first created, so after changing these, `chown` the existing `prigh-home` volume or recreate it |
| `PRIGH_NIX_SUBSTITUTER`, `PRIGH_NIX_SUBSTITUTER_KEY` | `https://prigh.cachix.org` and its key | (build) the Nix binary cache. Set the substituter to an empty value to compile everything |
| `PRIGH_EXTRA_APT_PACKAGES` | – | (build) extra Debian packages for the agent to use, e.g. `python3 build-essential` |

The image ships `bash git ripgrep tmux curl jq less openssh-client
procps`. Anything else the agent needs at runtime must be added with
`PRIGH_EXTRA_APT_PACKAGES`, because the container runs as a non-root user
on a read-only root filesystem.

## Logging in

Use `/login` in either UI, or a console in the container. This covers
subscriptions (Claude Pro/Max, ChatGPT) and API keys alike:

```
docker compose exec prigh prigh login anthropic                   # Claude Pro/Max (OAuth)
docker compose exec prigh prigh login anthropic -method api_key
docker compose exec prigh prigh login openai-codex                # ChatGPT (OAuth)
docker compose exec prigh prigh login openai                      # API key
docker compose exec prigh prigh login deepseek                    # API key
```

The OAuth redirect goes to `localhost` on the machine running the browser,
which is not the container. The page fails to load, so copy the full URL
from the browser's address bar and paste it at the prompt. Credentials are
stored in `/home/prigh/.config/prigh/auth.json`, in the home volume, so
they survive rebuilds.

The compose file doesn't pass provider keys as environment variables,
because a stored login does the same job. Provider keys set in the stack
also show up in Portainer and `docker inspect`, and a stored login takes
precedence over them. If you want a fully declarative deploy anyway, add
`ANTHROPIC_API_KEY`, `OPENAI_API_KEY` or `DEEPSEEK_API_KEY` under
`environment:` in `docker-compose.yml`.

## Git and SSH

The agent can use every credential in the container, and so can anyone with
`PRIGH_TOKEN`. Give it scoped, revocable credentials rather than your own
keys. There are two supported ways.

**SSH deploy key.** The key is generated inside the container and never
leaves it:

```
docker compose exec prigh prigh-docker ssh-key     # or the Portainer console
```

This creates `~/.ssh/id_ed25519` in the home volume, if it doesn't exist
yet, and prints the public key. Add that key to each repository under
**Settings → Deploy keys**, and tick "Allow write access" only if the agent
should push. A deploy key works for one repository only; for several,
use a machine user's account key, or HTTPS with a token.

**HTTPS with a token.** Set `GH_TOKEN` to a
[fine-grained personal access token](https://github.com/settings/personal-access-tokens)
limited to the repositories (and permissions, e.g. Contents read/write)
the agent needs. A credential helper in `/etc/gitconfig` hands it to `git`
for `https://github.com/...`; `gh` reads `GH_TOKEN` too if you install it.

For both:

- GitHub's host keys are pinned in `/etc/ssh/ssh_known_hosts`. Other hosts
  are trusted on first use (`StrictHostKeyChecking accept-new`), because
  the agent cannot answer a prompt. `GIT_TERMINAL_PROMPT=0` makes `git`
  fail instead of waiting for a password.
- Set `PRIGH_GIT_NAME` / `PRIGH_GIT_EMAIL` so that commits have an author.

## Isolation

- The agent sees only the container filesystem. `read_only: true` makes
  everything outside the two volumes and `/tmp` (a tmpfs) unwritable.
  There are no bind mounts of the host unless you set `PRIGH_WORKSPACE`,
  and the Docker socket is not mounted.
- The container runs as the unprivileged `prigh` user, with all
  capabilities dropped and `no-new-privileges`, so `sudo`/setuid binaries
  cannot regain root.
- Credentials (`auth.json`, the SSH key, `GH_TOKEN`) belong to the same
  user the agent runs as, so the agent can read them; see
  [Git and SSH](#git-and-ssh).
- Network access is unrestricted: the agent needs it for provider APIs, and
  it can reach anything the Docker network can. Put the stack on an
  isolated network if that matters.
- The token is the only authentication, and the traffic is plain HTTP/WS.
  Outside a trusted LAN, set `PRIGH_BIND=127.0.0.1` and put a
  TLS-terminating reverse proxy (Caddy, Traefik, nginx; it must pass
  WebSockets on `/ws` and `/terminal`) or an SSH tunnel in front.

## Data

| Path | Volume | Contents |
|---|---|---|
| `/home/prigh` | `prigh-home` | sessions (`.prigh/sessions`), config, prompt history, `auth.json`, `~/.ssh`, git config |
| `/workspace` | `prigh-workspace` or `PRIGH_WORKSPACE` | the default working directory. Clone projects here (`/cd` switches between them) |

Removing the stack keeps named volumes unless they are removed
explicitly (`docker compose down -v` deletes them, logins and sessions
included).

## Other commands

The entrypoint (`docker/prigh-docker`) runs any other command it is given:

```
docker compose run --rm prigh prigh sessions list
docker compose run --rm prigh bash
docker compose exec prigh prigh auth
```

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
   binaries, the web assets and the few store paths the binaries link
   against (glibc, gmp), about 50 MB instead of the ~10 GB build closure.
2. A `node` stage builds `pi-web/` with `npm ci && npm run build`.
3. The runtime stage is `debian:bookworm-slim`. `tini` is PID 1, so
   signals and the agent's child processes are handled properly.
