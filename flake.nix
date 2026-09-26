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
              nativeBuildInputs = [ pkgs.chromium pkgs.curl pkgs.jq pkgs.nodejs ];
            } ''
              mkdir -p fake-bin home cwd
              for opener in open xdg-open; do
                cat > "fake-bin/$opener" <<'EOF'
              #!${pkgs.runtimeShell}
              printf '%s\n' "$1" > "$OPEN_LOG"
              EOF
                chmod +x "fake-bin/$opener"
              done
              export OPEN_LOG="$PWD/opened-url"
              export HOME="$PWD/home"
              export PATH="$PWD/fake-bin:$PATH"
              PRIGH_WEB_LISTEN=127.0.0.1:0 \
                ${prigh}/bin/prigh -web -faux -token 'sekrit&x=y' -cwd "$PWD/cwd" \
                >server.out 2>server.err &
              server_pid=$!
              cleanup() {
                status=$?
                kill "''${chrome_pid:-}" "$server_pid" 2>/dev/null || true
                if test "$status" -ne 0; then
                  cat server.err chrome.err 2>/dev/null || true
                  cat targets.json body.txt 2>/dev/null || true
                fi
                exit "$status"
              }
              trap cleanup EXIT
              for attempt in $(seq 1 200); do
                test -s "$OPEN_LOG" && break
                sleep 0.05
              done
              test -s "$OPEN_LOG"
              opened_url="$(cat "$OPEN_LOG")"
              grep -Fx "prigh: web ui on $opened_url" server.err
              ! grep -q 'sekrit' server.err
              case "$opened_url" in *token*) exit 1;; esac
              curl --fail --silent "''${opened_url}main.bc.js" >/dev/null
              chromium \
                --headless --no-sandbox --disable-gpu \
                --user-data-dir="$PWD/chrome" --remote-debugging-port=9222 \
                "$opened_url" >chrome.out 2>chrome.err &
              chrome_pid=$!
              for attempt in $(seq 1 200); do
                curl --fail --silent http://127.0.0.1:9222/json >targets.json \
                  && test "$(jq length targets.json)" -gt 0 \
                  && break
                sleep 0.05
              done
              test -s targets.json
              debugger_url="$(jq -er '[.[] | select(.type == "page")][0].webSocketDebuggerUrl' targets.json)"
              cat > drive-browser.mjs <<'EOF'
              const ws = new WebSocket(process.argv[2]);
              await new Promise((resolve, reject) => {
                ws.onopen = resolve;
                ws.onerror = reject;
              });
              let nextId = 1;
              const pending = new Map();
              ws.onmessage = event => {
                const message = JSON.parse(event.data);
                if (message.id && pending.has(message.id)) {
                  pending.get(message.id)(message);
                  pending.delete(message.id);
                }
              };
              const call = (method, params = {}) => new Promise(resolve => {
                const id = nextId++;
                pending.set(id, resolve);
                ws.send(JSON.stringify({ id, method, params }));
              });
              const evaluate = async expression => {
                const reply = await call("Runtime.evaluate", { expression, returnByValue: true });
                return reply.result.result.value;
              };
              for (let attempt = 0; attempt < 200; attempt++) {
                if (await evaluate("Boolean(document.querySelector('form.connect'))")) break;
                await new Promise(resolve => setTimeout(resolve, 50));
              }
              if (!(await evaluate("Boolean(document.querySelector('form.connect'))"))) {
                throw new Error("tokenless page did not show the connect form");
              }
              await evaluate(`
                document.querySelector("#token").value = "sekrit&x=y";
                document.querySelector("#connect-form").requestSubmit();
              `);
              let body = "";
              for (let attempt = 0; attempt < 400; attempt++) {
                body = await evaluate(`document.body?.textContent ?? ""`);
                if (body.includes("deepseek-flash") && !body.includes("connecting…")) break;
                await new Promise(resolve => setTimeout(resolve, 50));
              }
              if (!body.includes("deepseek-flash") || body.includes("connecting…")) {
                throw new Error("authenticated page did not start: " + JSON.stringify(body));
              }
              await call("Page.bringToFront");
              if (!(await evaluate("document.hasFocus()"))) {
                throw new Error("browser page is not focused");
              }
              const press = async (key, code, text = "") => {
                await call("Input.dispatchKeyEvent", { type: "keyDown", key, code, text });
                await call("Input.dispatchKeyEvent", { type: "keyUp", key, code });
              };
              let typed = "";
              for (const key of "hello") {
                typed += key;
                await press(key, "Key" + key.toUpperCase(), key);
                for (let attempt = 0; attempt < 100; attempt++) {
                  body = await evaluate(`document.body?.textContent ?? ""`);
                  if (body.includes("> " + typed)) break;
                  await new Promise(resolve => setTimeout(resolve, 10));
                }
                if (!body.includes("> " + typed)) {
                  throw new Error("real browser input did not reach the editor: " + JSON.stringify(body));
                }
              }
              await press("Enter", "Enter", "\r");
              for (let attempt = 0; attempt < 400; attempt++) {
                body = await evaluate(`document.body?.textContent ?? ""`);
                if (body.includes("faux reply")) break;
                await new Promise(resolve => setTimeout(resolve, 50));
              }
              if (!body.includes("faux reply")) {
                throw new Error("browser prompt did not round-trip: " + JSON.stringify(body));
              }
              console.log(body);
              ws.close();
              EOF
              node drive-browser.mjs "$debugger_url" >body.txt
              grep -q 'deepseek-flash' body.txt
              ! grep -q 'connecting…' body.txt
              ! grep -q 'connect-form' body.txt
              touch "$out"
            ''
          else
            pkgs.runCommand "prigh-web-browser-test-skipped" { } ''
              touch "$out"
            '';

        checks.web-workflows =
          if pkgs.stdenv.hostPlatform.isLinux then
            pkgs.runCommand "prigh-web-workflows-test" {
              nativeBuildInputs =
                [
                  pkgs.curl
                  pkgs.nodejs
                  pkgs.playwright-test
                  pkgs.playwright-driver.browsers
                ];
            } ''
              mkdir -p fake-bin
              for opener in open xdg-open; do
                cat > "fake-bin/$opener" <<'EOF'
              #!${pkgs.runtimeShell}
              printf '%s\n' "$1" > "$OPEN_LOG"
              EOF
                chmod +x "fake-bin/$opener"
              done
              export PATH="$PWD/fake-bin:$PATH"
              export PLAYWRIGHT_BROWSERS_PATH=${pkgs.playwright-driver.browsers}
              export PLAYWRIGHT_MODULE=${pkgs.playwright-test}/lib/node_modules/playwright/index.mjs
              : >actual
              cleanup() {
                status=$?
                kill "''${server_pid:-}" 2>/dev/null || true
                if test "$status" -ne 0; then
                  cat server.err actual 2>/dev/null || true
                fi
                exit "$status"
              }
              trap cleanup EXIT
              for engine in chromium firefox; do
                mkdir -p "home-$engine" "cwd-$engine"
                export OPEN_LOG="$PWD/opened-$engine"
                export HOME="$PWD/home-$engine"
                PRIGH_WEB_LISTEN=127.0.0.1:0 \
                  ${prigh}/bin/prigh -web -faux -token 'sekrit&x=y' \
                    -cwd "$PWD/cwd-$engine" >server.out 2>server.err &
                server_pid=$!
                for attempt in $(seq 1 200); do
                  test -s "$OPEN_LOG" && break
                  sleep 0.05
                done
                test -s "$OPEN_LOG"
                opened_url="$(cat "$OPEN_LOG")"
                case "$opened_url" in *token*) exit 1;; esac
                TEST_CWD="$PWD/cwd-$engine" \
                  node ${./tui/e2e-web/web_workflows.mjs} \
                    "$opened_url" 'sekrit&x=y' "$engine" >>actual
                kill "$server_pid"
                wait "$server_pid" 2>/dev/null || true
                server_pid=
              done
              diff -u ${./tui/e2e-web/web_workflows.expected} actual
              mkdir "$out"
              cp actual "$out/screens.txt"
            ''
          else
            pkgs.runCommand "prigh-web-workflows-test-skipped" { } ''
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
