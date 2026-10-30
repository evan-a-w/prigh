{
  description = "prigh — OCaml backend, Bonsai_term and Bonsai_web frontends on OxCaml";

  # The OxCaml toolchain and packages, prebuilt (see README.md).
  nixConfig = {
    extra-substituters = [ "https://prigh.cachix.org" ];
    extra-trusted-public-keys = [
      "prigh.cachix.org-1:1kHKoGOetNmpzJg8lJ1nvQzwzTL0XiOs5GInELMgSoI="
    ];
  };

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

        # nix/opam-repository: packages adapted to the OxCaml pins (see
        # nix/README.md); first, so they shadow the other repositories.
        oxRepos = [
          ./nix/opam-repository
          ox-opam-repository
          opam-repository
        ];

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
        } ./frontend query).overrideScope overlay;

        backendScope = (on.buildOpamProject' {
          repos = oxRepos;
          resolveArgs = {
            depopts = false;
            dev = false;
          };
        } ./backend query).overrideScope overlay;

        # The web site gets precompressed siblings (file.gz, file.br) that
        # the backend serves to browsers accepting them (Static_site).
        prighFrontend = frontendScope.prigh_frontend.overrideAttrs (oa: {
          nativeBuildInputs = (oa.nativeBuildInputs or [ ]) ++ [
            pkgs.zopfli
            pkgs.brotli
          ];
          postInstall = (oa.postInstall or "") + ''
            for f in "$out"/share/prigh_frontend/web/*.{js,css,html,svg,map,txt,webmanifest}; do
              [ -f "$f" ] || continue
              zopfli "$f"
              brotli --keep --best "$f"
            done
          '';
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
        # with prigh-web, the browser frontend, instead (`prigh serve -web
        # 127.0.0.1:7790 -open ...`); with -connect or -remote, prigh-web for
        # a backend elsewhere (`prigh web -open ...`). The backend's
        # subcommands (`prigh acp`, `prigh login ...`) go to the backend; a
        # directory with one of their names opens with `prigh ./NAME`.
        prighWebRoot = "${prighFrontend}/share/prigh_frontend/web";
        prigh = pkgs.writeShellApplication {
          name = "prigh";
          text = ''
            export PRIGH_BACKEND="''${PRIGH_BACKEND:-${prighBackend}/bin/prigh}"
            export PRIGH_WEB_ROOT="''${PRIGH_WEB_ROOT:-${prighWebRoot}}"
            export PRIGH_TMUX="''${PRIGH_TMUX:-${pkgs.tmux}/bin/tmux}"
            if [ "''${1:-}" = "-web" ]; then
              shift
              for arg in "$@"; do
                case "$arg" in -connect | -remote) exec "$PRIGH_BACKEND" web -open "$@" ;; esac
              done
              exec "$PRIGH_BACKEND" serve -web "''${PRIGH_WEB_LISTEN:-127.0.0.1:7790}" -open "$@"
            fi
            case "''${1:-}" in
              acp | run | serve | tool-host | web | sessions | login | logout | auth | models | trace)
                exec "$PRIGH_BACKEND" "$@" ;;
            esac
            exec ${prighFrontend}/bin/prigh-tui "$@"
          '';
        };
      in
      {
        legacyPackages = frontendScope;

        packages = {
          default = prigh;
          frontend = prighFrontend;
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
          test "$(cat root)" = "${prighWebRoot}"
          ARGS_OUT="$PWD/default-args" ROOT_OUT="$PWD/default-root" \
            PRIGH_BACKEND="$PWD/fake-backend" \
            ${prigh}/bin/prigh -web
          diff -u ${pkgs.writeText "expected-default-args" ''
            serve
            -web
            127.0.0.1:7790
            -open
          ''} default-args
          ARGS_OUT="$PWD/override-args" ROOT_OUT="$PWD/override-root" \
            PRIGH_BACKEND="$PWD/fake-backend" PRIGH_WEB_ROOT=/custom \
            ${prigh}/bin/prigh -web
          test "$(cat override-root)" = /custom
          ARGS_OUT="$PWD/connect-args" ROOT_OUT="$PWD/connect-root" \
            PRIGH_BACKEND="$PWD/fake-backend" \
            ${prigh}/bin/prigh -web -connect server:7790 -token sekrit
          diff -u ${pkgs.writeText "expected-connect-args" ''
            web
            -open
            -connect
            server:7790
            -token
            sekrit
          ''} connect-args
          test "$(cat connect-root)" = "${prighWebRoot}"
          ARGS_OUT="$PWD/remote-args" ROOT_OUT="$PWD/remote-root" \
            PRIGH_BACKEND="$PWD/fake-backend" \
            ${prigh}/bin/prigh -web -remote
          diff -u ${pkgs.writeText "expected-remote-args" ''
            web
            -open
            -remote
          ''} remote-args
          test -f ${prighWebRoot}/index.html
          test -f ${prighWebRoot}/main.bc.js
          test -f ${prighWebRoot}/style.css
          test -f ${prighWebRoot}/terminal.css
          test -f ${prighWebRoot}/files.css
          test -f ${prighWebRoot}/review.css
          test -f ${prighWebRoot}/terminal.js
          test -f ${prighWebRoot}/xterm.js
          test -f ${prighWebRoot}/xterm.css
          test -f ${prighWebRoot}/addon-fit.js
          test -f ${prighWebRoot}/katex.min.js
          test -f ${prighWebRoot}/katex.min.css
          test -f ${prighWebRoot}/copy-tex.min.js
          test -f ${prighWebRoot}/KaTeX_Main-Regular.woff2
          # every font the KaTeX stylesheet names is there
          for font in $(grep -o 'url([^)]*)' ${prighWebRoot}/katex.min.css | sed 's/^url(//; s/)$//'); do
            test -f "${prighWebRoot}/$font"
          done
          # the release build: whole-program, no source map
          test "$(stat -c %s ${prighWebRoot}/main.bc.js)" -lt 5000000
          # precompressed, decoding to the file itself
          for f in ${prighWebRoot}/main.bc.js ${prighWebRoot}/index.html \
                   ${prighWebRoot}/style.css ${prighWebRoot}/katex.min.js; do
            ${pkgs.gzip}/bin/gzip -dc "$f.gz" | cmp - "$f"
            ${pkgs.brotli}/bin/brotli -dc "$f.br" | cmp - "$f"
          done
          test "$(stat -c %s ${prighWebRoot}/main.bc.js.gz)" -lt 1000000
          touch "$out"
        '';

        # frontend/web/e2e: Chromium and Firefox against the built backend
        # and the installed prigh-web site.
        checks.web-e2e =
          if pkgs.stdenv.hostPlatform.isLinux then
            pkgs.runCommand "prigh-web-e2e"
              {
                nativeBuildInputs = [
                  pkgs.nodejs
                  pkgs.playwright-test
                  pkgs.playwright-driver.browsers
                  pkgs.dejavu_fonts
                  pkgs.git # /diff
                ];
              }
              ''
                cat > fonts.conf <<EOF
                <?xml version="1.0"?>
                <!DOCTYPE fontconfig SYSTEM "fonts.dtd">
                <fontconfig>
                  <dir>${pkgs.dejavu_fonts}/share/fonts</dir>
                  <cachedir>$PWD/font-cache</cachedir>
                </fontconfig>
                EOF
                export FONTCONFIG_FILE="$PWD/fonts.conf"
                export PLAYWRIGHT_BROWSERS_PATH=${pkgs.playwright-driver.browsers}
                export PLAYWRIGHT_MODULE=${pkgs.playwright-test}/lib/node_modules/playwright/index.mjs
                export PRIGH_BACKEND=${prighBackend}/bin/prigh
                export PRIGH_WEB_ROOT=${prighWebRoot}
                export PRIGH_TMUX=${pkgs.tmux}/bin/tmux
                export HOME="$PWD/home"
                mkdir -p "$HOME"
                cp -r ${./frontend/web/e2e} e2e
                chmod -R u+w e2e
                ${pkgs.runtimeShell} e2e/prigh_web.sh
                mkdir "$out"
              ''
          else
            pkgs.runCommand "prigh-web-e2e-skipped" { } ''touch "$out"'';

        # One OxCaml toolchain for the backend and the frontends: both projects
        # build from this shell (`.#backend` is kept as an alias).
        devShells.default = pkgs.mkShell {
          inputsFrom = [
            prighFrontend
            prighBackend
          ];
          buildInputs = [
            frontendScope.bonsai_test
            frontendScope.expect_test_helpers_core
            frontendScope.expect_test_helpers_async
            # The OxCaml ocamlformat (frontendScope.ocamlformat) has an unhashed git
            # source that does not evaluate; upstream formats the backend, and
            # whatever does not use OxCaml syntax in frontend/.
            pkgs.ocamlformat
            pkgs.ripgrep
            # the backend's terminal tests and the TUI's tmux-test
            pkgs.tmux
            # the web-layer tests run under node (js_of_ocaml)
            pkgs.nodejs
          ];
          # opam-nix puts a fake `opam` of every package on PATH (~250 of
          # them); keep one. Otherwise PATH outgrows tmux's 16 KB command
          # limit, and frontend/tui/tmux-test passes it in a tmux command.
          shellHook = ''
            PATH=$(printf '%s' "$PATH" | tr : '\n' | awk '!/-opam\/bin$/ || !seen++' | paste -sd : -)
          '';
        };
        devShells.backend = self.devShells.${system}.default;
      }
    );
}
