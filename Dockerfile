# prigh in a container: see DOCKER.md.
#
# Stage 1 builds the backend and the TUI/Bonsai web frontend with the flake
# (OxCaml is only practical to build through Nix). Stage 2 builds pi's web
# UI with npm. The runtime image is Debian with just the two binaries, the
# few Nix store paths they link against, and the static web assets.

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
 && mkdir -p /opt/prigh/bin /opt/prigh/share /closure \
 && cp -L /out/backend/bin/prigh /out/tui/bin/prigh-tui /opt/prigh/bin/ \
 && cp -rL /out/tui/share/prigh_tui/web /opt/prigh/share/web \
 && chmod -R u+w /opt/prigh \
 && refs=$(grep -raoh '/nix/store/[a-z0-9]\{32\}-[-a-zA-Z0-9+._?=]*' /opt/prigh | sort -u) \
 && echo "runtime store paths: $refs" \
 && cp -a $(nix-store -qR $refs) /closure/

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
      bash ca-certificates curl git jq less netbase openssh-client procps \
      ripgrep tini tmux ${EXTRA_APT_PACKAGES} \
 && rm -rf /var/lib/apt/lists/*

COPY --from=nix-build /closure /nix/store
COPY --from=nix-build /opt/prigh /opt/prigh
COPY --from=pi-web-build /src/pi-web/dist /opt/prigh/share/pi-web
COPY docker/prigh-docker /usr/local/bin/prigh-docker
COPY docker/gitconfig /etc/gitconfig
COPY docker/ssh_known_hosts /etc/ssh/ssh_known_hosts
COPY docker/ssh_config /etc/ssh/ssh_config.d/prigh.conf
RUN ln -s /opt/prigh/bin/prigh /opt/prigh/bin/prigh-tui /usr/local/bin/ \
 && groupadd -o -g "${PRIGH_GID}" prigh \
 && useradd -o -m -u "${PRIGH_UID}" -g prigh -s /bin/bash prigh \
 && mkdir -p /workspace /home/prigh/.prigh /home/prigh/.config/prigh \
 && chown -R prigh:prigh /workspace /home/prigh

ENV LANG=C.UTF-8 \
    PRIGH_BACKEND=/opt/prigh/bin/prigh \
    PRIGH_WEB_ROOT=/opt/prigh/share/web \
    PRIGH_PI_WEB_ROOT=/opt/prigh/share/pi-web \
    PRIGH_MODE=web \
    PRIGH_CWD=/workspace \
    GIT_TERMINAL_PROMPT=0

USER prigh
WORKDIR /workspace
VOLUME ["/home/prigh", "/workspace"]
EXPOSE 7777 7788 7789
HEALTHCHECK --interval=30s --timeout=5s --start-period=10s CMD ["prigh-docker", "healthcheck"]
ENTRYPOINT ["tini", "--", "prigh-docker"]
