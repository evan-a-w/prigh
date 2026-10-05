# prigh in a container: see DOCKER.md.
#
# Stage 1 builds the backend and the TUI/Bonsai web frontend with the flake
# (OxCaml is only practical to build through Nix). Stage 2 builds pi's web
# UI with npm. The runtime image is Debian with just the two binaries, the
# few Nix store paths they link against, the static web assets, and Nix
# itself for the agents to install tools with.

ARG NIX_IMAGE=nixos/nix:2.35.2
ARG NODE_IMAGE=node:22-bookworm-slim
ARG RUNTIME_IMAGE=debian:bookworm-slim

FROM ${NIX_IMAGE} AS nix-build
# A binary cache holding the OxCaml toolchain and packages, so they are
# downloaded instead of compiled (see DOCKER.md). Empty NIX_SUBSTITUTER to
# build everything from source.
ARG NIX_SUBSTITUTER=https://prigh.cachix.org
ARG NIX_SUBSTITUTER_KEY=prigh.cachix.org-1:1kHKoGOetNmpzJg8lJ1nvQzwzTL0XiOs5GInELMgSoI=
# Builds must keep running as the image's nixbld users: as root, an impure
# build can create /homeless-shelter, which fails every later build.
RUN printf '%s\n' 'experimental-features = nix-command flakes' 'sandbox = false' \
      'filter-syscalls = false' 'max-jobs = auto' >> /etc/nix/nix.conf \
 && if [ -n "$NIX_SUBSTITUTER" ]; then \
      printf '%s\n' "extra-substituters = $NIX_SUBSTITUTER" \
        "extra-trusted-public-keys = $NIX_SUBSTITUTER_KEY" >> /etc/nix/nix.conf; \
    fi

# Dependencies first, from only the files that determine them, so that source
# changes reuse these layers. The frontend's (OxCaml, very slow) come first so
# that backend dependency changes don't rebuild them.
COPY flake.nix flake.lock /deps/
COPY nix /deps/nix
COPY tui/dune-project tui/prigh_tui.opam /deps/tui/
RUN nix build --no-link path:/deps#tui.inputDerivation
COPY backend/dune-project backend/prigh.opam /deps/backend/
RUN nix build --no-link path:/deps#backend.inputDerivation

COPY . /src
RUN nix build --out-link /out/backend path:/src#backend \
 && nix build --out-link /out/tui path:/src#tui \
 && mkdir -p /opt/prigh/bin /opt/prigh/share \
 && cp -L /out/backend/bin/prigh /out/tui/bin/prigh-tui /opt/prigh/bin/ \
 && cp -rL /out/tui/share/prigh_tui/web /opt/prigh/share/web \
 && chmod -R u+w /opt/prigh \
 && refs=$(grep -raoh '/nix/store/[a-z0-9]\{32\}-[-a-zA-Z0-9+._?=]*' /opt/prigh | sort -u) \
 && echo "runtime store paths: $refs" \
 && nix_pkg=$(dirname "$(dirname "$(readlink -f "$(command -v nix)")")") \
 && closure=$(nix-store -qR $refs "$nix_pkg") \
 && mkdir -p /seed/store \
 && cp -a $closure /seed/store/ \
 && nix-store --dump-db $closure >/seed/registration \
 && printf '%s\n' $refs "$nix_pkg" >/seed/roots \
 && echo "$nix_pkg" >/seed/nix \
 && sha256sum /seed/registration | cut -c1-32 >/seed/id \
 && nix eval --raw --impure --expr 'let lock = builtins.fromJSON (builtins.readFile /src/flake.lock); \
      locked = lock.nodes.${lock.nodes.root.inputs.nixpkgs}.locked; \
    in builtins.toJSON { version = 2; flakes = [ { from = { type = "indirect"; id = "nixpkgs"; }; \
      to = { inherit (locked) type owner repo rev narHash lastModified; }; } ]; }' >/seed/flake-registry.json

FROM ${NODE_IMAGE} AS pi-web-build
WORKDIR /src/pi-web
COPY pi-web/package.json pi-web/package-lock.json ./
RUN npm ci --no-audit --no-fund
COPY tui/web-bin /src/tui/web-bin
COPY pi-web ./
RUN npm run build

FROM ${RUNTIME_IMAGE}
ARG EXTRA_APT_PACKAGES=""
ARG PRIGH_UID=1000
ARG PRIGH_GID=1000
RUN apt-get update \
 && DEBIAN_FRONTEND=noninteractive apt-get install -y --no-install-recommends \
      bash ca-certificates curl git jq less libnss-extrausers netbase \
      openssh-client procps ripgrep tini tmux util-linux ${EXTRA_APT_PACKAGES} \
 && rm -rf /var/lib/apt/lists/*

# The entrypoint creates a Linux user per namespace at runtime, on a read-only
# root: NSS also reads users from /var/lib/extrausers, which points into the
# /run tmpfs. Checked here, since a broken lookup would only show at runtime.
RUN sed -i -E '/extrausers/! s/^(passwd|group|shadow):(.*)$/\1:\2 extrausers/' /etc/nsswitch.conf \
 && [ "$(grep -cE '^(passwd|group|shadow):.* extrausers' /etc/nsswitch.conf)" = 3 ] \
 && rm -rf /var/lib/extrausers \
 && ln -s /run/prigh/extrausers /var/lib/extrausers \
 && mkdir -p /run/prigh/extrausers \
 && echo 'nsstest:x:54321:54321::/nonexistent:/bin/false' > /run/prigh/extrausers/passwd \
 && printf 'nsstest:x:54321:\nnsstest2:x:54322:nsstest\n' > /run/prigh/extrausers/group \
 && echo 'nsstest:*:::::::' > /run/prigh/extrausers/shadow \
 && [ "$(setpriv --reuid=54321 --regid=54321 --init-groups id -un):$(setpriv --reuid=54321 --regid=54321 --init-groups id -G)" = "nsstest:54321 54322" ] \
 && rm -rf /run/prigh

# The store paths prigh and nix need, outside /nix: /nix is a volume, which
# hides the image's /nix and keeps its contents across upgrades, so the
# entrypoint copies the missing ones in at every start.
COPY --from=nix-build /seed /opt/nix-seed
COPY --from=nix-build /opt/prigh /opt/prigh
COPY --from=pi-web-build /src/pi-web/dist /opt/prigh/share/pi-web
COPY docker/prigh-docker /usr/local/bin/prigh-docker
COPY docker/gitconfig /etc/gitconfig
COPY docker/ssh_known_hosts /etc/ssh/ssh_known_hosts
COPY docker/ssh_config /etc/ssh/ssh_config.d/prigh.conf
COPY docker/nix.conf /etc/nix/nix.conf
RUN mv /opt/nix-seed/flake-registry.json /etc/nix/ \
 && ln -s /opt/prigh/bin/prigh /opt/prigh/bin/prigh-tui /usr/local/bin/ \
 && nix_pkg=$(cat /opt/nix-seed/nix) \
 && for b in /opt/nix-seed/store/"${nix_pkg##*/}"/bin/*; do ln -s "$nix_pkg/bin/${b##*/}" /usr/local/bin/; done \
 && groupadd -r -g 30000 nixbld \
 && for i in 1 2 3 4 5 6 7 8; do \
      useradd -r -M -N -u $((30000 + i)) -g nixbld -G nixbld -d /var/empty -s /usr/sbin/nologin \
        -c "Nix build user $i" "nixbld$i"; \
    done \
 && mkdir /nix \
 && groupadd -o -g "${PRIGH_GID}" prigh \
 && useradd -o -m -u "${PRIGH_UID}" -g prigh -s /bin/bash prigh \
 && mkdir -p /workspace /home/prigh/.prigh /home/prigh/.config/prigh \
 && chown -R prigh:prigh /home/prigh \
 && chmod 700 /home/prigh

ENV LANG=C.UTF-8 \
    PRIGH_BACKEND=/opt/prigh/bin/prigh \
    PRIGH_WEB_ROOT=/opt/prigh/share/web \
    PRIGH_PI_WEB_ROOT=/opt/prigh/share/pi-web \
    PRIGH_MODE=web \
    GIT_TERMINAL_PROMPT=0

# Starts as root: the entrypoint creates the users, then drops privileges.
# Each user's own directory is /workspace/<name>.
WORKDIR /workspace
VOLUME ["/home", "/workspace", "/nix"]
EXPOSE 7777 7788 7789
HEALTHCHECK --interval=30s --timeout=5s --start-period=10s CMD ["prigh-docker", "healthcheck"]
# -g: stop signals go to the whole process group (backend and tool hosts).
ENTRYPOINT ["tini", "-g", "--", "prigh-docker"]
