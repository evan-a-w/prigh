{
  description = "prigh — OCaml backend, Bonsai_term and Bonsai_web frontends on OxCaml";

  inputs = {
    nixpkgs.url = "github:NixOS/nixpkgs/nixos-unstable";
    flake-utils.url = "github:numtide/flake-utils";
    opam-nix.url = "github:tweag/opam-nix";

    # The patched packages/compiler (Jane Street `+ox` versions) live in the
    # OxCaml opam repository. It is merged *before* upstream opam-repository so
    # its definitions take precedence.
    opam-repository = {
      url = "github:ocaml/opam-repository";
      flake = false;
    };
    ox-opam-repository = {
      url = "github:oxcaml/opam-repository";
      flake = false;
    };
  };

  outputs =
    {
      self,
      nixpkgs,
      flake-utils,
      opam-nix,
      opam-repository,
      ox-opam-repository,
    }:
    flake-utils.lib.eachDefaultSystem (
      system:
      let
        pkgs = nixpkgs.legacyPackages.${system};
        on = opam-nix.lib.${system};

        oxRepos = [
          ox-opam-repository
          opam-repository
        ];

        upstreamRepos = [ opam-repository ];

        # Pinned to the exact versions of the working `prigh-ox` switch. This
        # both forces the OxCaml compiler and keeps opam-nix's solver inside its
        # 60 s IFD timeout. See nix/prigh-ox-pins.nix.
        #
        # The pins were generated from a Linux opam switch, so it includes a few
        # Linux-only packages. Requesting those packages on Darwin makes the
        # opam-nix solver fail before it can build the project.
        query =
          let
            pins = import ./nix/prigh-ox-pins.nix;
          in
          if pkgs.stdenv.hostPlatform.isLinux then
            pins
          else
            builtins.removeAttrs pins [
              "eio_linux"
              "uring"
            ];

        # OxCaml 5.2.0+ox native codegen drops the second addressing register
        # of `Ifloatarithmem` (any float-array arithmetic at the non-AVX
        # baseline); the patch makes it copy the whole arg array like upstream
        # OCaml. See nix/fix-floatarithmem.patch.
        overlay =
          final: prev:
          {
            oxcaml-compiler = prev.oxcaml-compiler.overrideAttrs (oa: {
              patches = (oa.patches or [ ]) ++ [
                ./nix/fix-floatarithmem.patch
                ./nix/limit-dune-jobs.patch
              ];
              nativeBuildInputs =
                (oa.nativeBuildInputs or [ ])
                ++ pkgs.lib.optionals pkgs.stdenv.hostPlatform.isDarwin [
                  # OxCaml's archive merge helper invokes Apple's `libtool`.
                  pkgs.darwin.cctools
                ];
              # dune ignores `make -j`; nix/limit-dune-jobs.patch sizes the
              # compiler build to min(cores, RAM / 2 GB, 4). The bootstrap
              # has only been seen to succeed on macOS at 2 jobs, so pin it
              # there.
              env =
                (oa.env or { })
                // pkgs.lib.optionalAttrs pkgs.stdenv.hostPlatform.isDarwin { JOBS = "2"; };
            });
          };

        # `depopts = false` mirrors a plain `opam install` and avoids pulling in
        # test-only optional deps (e.g. markup -> bisect_ppx -> cmdliner < 2).
        frontendScope = (on.buildOpamProject' {
          repos = oxRepos;
          resolveArgs = {
            depopts = false;
            dev = false;
          };
        } ./tui query).overrideScope overlay;

        backendScope = on.buildOpamProject' {
          repos = upstreamRepos;
          resolveArgs = {
            depopts = false;
            dev = false;
          };
        } ./backend {
          ocaml-base-compiler = "5.3.0";
        };

        prighTui = frontendScope.prigh_tui.overrideAttrs (oa: {
          meta = (oa.meta or { }) // {
            mainProgram = "prigh-tui";
          };
        });

        prighBackend = backendScope.prigh.overrideAttrs (oa: {
          meta = (oa.meta or { }) // {
            mainProgram = "prigh";
          };
        });

        # `prigh` runs the TUI; `prigh -web [serve options]` runs the backend
        # with the browser frontend instead (`prigh serve -web 127.0.0.1:7788
        # -open ...`).
        prigh = pkgs.writeShellApplication {
          name = "prigh";
          text = ''
            export PRIGH_BACKEND="''${PRIGH_BACKEND:-${prighBackend}/bin/prigh}"
            export PRIGH_WEB_ROOT="''${PRIGH_WEB_ROOT:-${prighTui}/share/prigh_tui/web}"
            if [ "''${1:-}" = "-web" ]; then
              shift
              exec "$PRIGH_BACKEND" serve -web "''${PRIGH_WEB_LISTEN:-127.0.0.1:7788}" -open "$@"
            fi
            exec ${prighTui}/bin/prigh-tui "$@"
          '';
        };
      in
      {
        legacyPackages = frontendScope;

        packages = {
          default = prigh;
          tui = prighTui;
          backend = prighBackend;
        };

        apps.default = {
          type = "app";
          program = "${prigh}/bin/prigh";
        };

        checks.web-wrapper = pkgs.runCommand "prigh-web-wrapper-test" { } ''
          cat > fake-backend <<'EOF'
          #!${pkgs.runtimeShell}
          printf '%s\n' "$@" > "$ARGS_OUT"
          printf '%s\n' "$PRIGH_WEB_ROOT" > "$ROOT_OUT"
          EOF
          chmod +x fake-backend
          ARGS_OUT="$PWD/args" ROOT_OUT="$PWD/root" \
            PRIGH_BACKEND="$PWD/fake-backend" PRIGH_WEB_LISTEN=0.0.0.0:7777 \
            ${prigh}/bin/prigh -web -token sekrit -cwd /work
          diff -u ${pkgs.writeText "expected-web-args" ''
            serve
            -web
            0.0.0.0:7777
            -open
            -token
            sekrit
            -cwd
            /work
          ''} args
          test "$(cat root)" = "${prighTui}/share/prigh_tui/web"
          test -f ${prighTui}/share/prigh_tui/web/index.html
          test -f ${prighTui}/share/prigh_tui/web/main.bc.js
          test -f ${prighTui}/share/prigh_tui/web/style.css
          ARGS_OUT="$PWD/override-args" ROOT_OUT="$PWD/override-root" \
            PRIGH_BACKEND="$PWD/fake-backend" PRIGH_WEB_ROOT=/custom \
            ${prigh}/bin/prigh -web
          test "$(cat override-root)" = /custom
          touch "$out"
        '';

        checks.web-browser =
          if pkgs.stdenv.hostPlatform.isLinux then
            pkgs.runCommand "prigh-web-browser-test" {
              nativeBuildInputs = [ pkgs.chromium pkgs.curl ];
            } ''
              mkdir -p fake-bin home cwd
              cat > fake-bin/xdg-open <<'EOF'
              #!${pkgs.runtimeShell}
              printf '%s\n' "$1" > "$OPEN_LOG"
              EOF
              chmod +x fake-bin/xdg-open
              export OPEN_LOG="$PWD/opened-url"
              export HOME="$PWD/home"
              export PATH="$PWD/fake-bin:$PATH"
              PRIGH_WEB_LISTEN=127.0.0.1:0 \
                ${prigh}/bin/prigh -web -faux -token 'sekrit&x=y' -cwd "$PWD/cwd" \
                >server.out 2>server.err &
              server_pid=$!
              trap 'kill "$server_pid" 2>/dev/null || true' EXIT
              for attempt in $(seq 1 200); do
                test -s "$OPEN_LOG" && break
                sleep 0.05
              done
              test -s "$OPEN_LOG"
              opened_url="$(cat "$OPEN_LOG")"
              base_url="''${opened_url%%\?*}"
              grep -Fx "prigh: web ui on $base_url" server.err
              ! grep -q 'sekrit' server.err
              test "$opened_url" = "''${base_url}?token=sekrit%26x%3Dy"
              curl --fail --silent "''${base_url}main.bc.js" >/dev/null
              chromium \
                --headless --no-sandbox --disable-gpu \
                --user-data-dir="$PWD/chrome" --virtual-time-budget=10000 \
                --dump-dom "$opened_url" >dom.html
              grep -q '<pre class="screen">' dom.html
              grep -q 'deepseek-flash' dom.html
              ! grep -q 'connect-form' dom.html
              ! grep -q 'connecting…' dom.html
              touch "$out"
            ''
          else
            pkgs.runCommand "prigh-web-browser-test-skipped" { } ''
              touch "$out"
            '';

        devShells.default = pkgs.mkShell {
          inputsFrom = [
            prighTui
            prighBackend
          ];
          buildInputs = [
            frontendScope.bonsai_test
            frontendScope.expect_test_helpers_core
            frontendScope.expect_test_helpers_async
            frontendScope.ocamlformat
            pkgs.ripgrep
            # the web-layer tests run under node (js_of_ocaml)
            pkgs.nodejs
          ];
        };
      }
    );
}
