{
  description = "prigh — OCaml backend, Bonsai_term and Bonsai_web frontends on OxCaml";

  # prigh, its OxCaml toolchain and packages and the dev shells, prebuilt by
  # CI from main (see README.md).
  nixConfig = {
    extra-substituters = [ "https://attic.caduc.co/prigh" ];
    extra-trusted-public-keys = [
      "prigh:n6wPXDyh+4CS5uoNgXQ3QvKjOSLE7ED87qIiTSvih1A="
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
            # Worktrees, diffs, review and the git hosts run git: fall back
            # to ours when PATH has none (a systemd service's PATH rarely
            # includes the user's profile). Appended, so the user's git wins.
            # ImageMagick likewise: images too large for a model are
            # downscaled with it (on upload and before each request).
            export PATH="''${PATH:+$PATH:}${pkgs.git}/bin:${pkgs.imagemagick}/bin"
            if [ "''${1:-}" = "-web" ]; then
              shift
              for arg in "$@"; do
                case "$arg" in -connect | -remote) exec "$PRIGH_BACKEND" web -open "$@" ;; esac
              done
              exec "$PRIGH_BACKEND" serve -web "''${PRIGH_WEB_LISTEN:-127.0.0.1:7790}" -open "$@"
            fi
            case "''${1:-}" in
              acp | run | serve | tool-host | web | desktop | sessions | login | logout | auth | models | trace)
                exec "$PRIGH_BACKEND" "$@" ;;
            esac
            exec ${prighFrontend}/bin/prigh-tui "$@"
          '';
        };

        # The desktop app: nix/desktop.nix (a function of pkgs, for the Home
        # Manager module to build with the system's nixpkgs).
        lib = pkgs.lib;
        isLinux = pkgs.stdenv.hostPlatform.isLinux;
        desktop = import ./nix/desktop.nix { inherit pkgs prigh; };
        desktopVersion = desktop.version;
        prighDesktopUnwrapped = desktop.unwrapped;
        prighDesktop = desktop.wrapped;
        prighComputer = desktop.computer;
      in
      {
        legacyPackages = frontendScope;

        packages = {
          default = prigh;
          frontend = prighFrontend;
          backend = prighBackend;
          prigh-desktop = prighDesktop;
          prigh-desktop-unwrapped = prighDesktopUnwrapped;
        }
        // lib.optionalAttrs isLinux {
          prigh-computer = prighComputer;
        };

        apps = {
          default = {
            type = "app";
            program = "${prigh}/bin/prigh";
          };
        }
        // lib.optionalAttrs isLinux {
          # desktop/computer/test/hyprland.sh: needs a GPU, so not a check
          computer-use-hyprland = {
            type = "app";
            program = lib.getExe (
              pkgs.writeShellApplication {
                name = "computer-use-hyprland";
                runtimeInputs = [
                  pkgs.kdePackages.kwin
                  pkgs.hyprland
                  pkgs.wev
                  pkgs.procps
                  pkgs.coreutils
                  pkgs.gawk
                ];
                text = ''
                  # Mesa for the nested compositors, also where it is not
                  # the system's (outside NixOS)
                  export __EGL_VENDOR_LIBRARY_FILENAMES=${pkgs.mesa}/share/glvnd/egl_vendor.d/50_mesa.json
                  export LIBGL_DRIVERS_PATH=${pkgs.mesa}/lib/dri GBM_BACKENDS_PATH=${pkgs.mesa}/lib/gbm
                  export LD_LIBRARY_PATH=${pkgs.mesa}/lib
                  actual=$(mktemp)
                  trap 'rm -f "$actual"' EXIT
                  bash ${./desktop/computer/test}/hyprland.sh "''${1:-${prighComputer}/bin/prigh-computer}" | tee "$actual"
                  diff -u ${./desktop/computer/test/hyprland.expected} "$actual" && echo "as expected"
                '';
              }
            );
          };
        };

        checks.web-wrapper = pkgs.runCommand "prigh-web-wrapper-test" { } ''
          cat > fake-backend <<'EOF'
          #!${pkgs.runtimeShell}
          printf '%s\n' "$@" > "$ARGS_OUT"
          printf '%s\n' "$PRIGH_WEB_ROOT" > "$ROOT_OUT"
          command -v git > "$ARGS_OUT.git" || true
          command -v magick > "$ARGS_OUT.magick" || true
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
          # git is found even with no git on PATH
          test "$(cat args.git)" = "${pkgs.git}/bin/git"
          # and ImageMagick, to downscale images
          test "$(cat args.magick)" = "${pkgs.imagemagick}/bin/magick"
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

        # frontend/web/e2e: Chromium, Firefox and WebKit against the built backend
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
                export PRIGH_E2E_SHELL=${pkgs.bashInteractive}/bin/bash
                # WebKit (WPE) renders through EGL: Mesa's, in software.
                export __EGL_VENDOR_LIBRARY_DIRS=${pkgs.mesa}/share/glvnd/egl_vendor.d
                export HOME="$PWD/home"
                mkdir -p "$HOME"
                cp -r ${./frontend/web/e2e} e2e
                chmod -R u+w e2e
                # paste_vector.mjs seals with the page's own paste_seal.js.
                mkdir app
                cp ${./frontend/web/app/paste_seal.js} app/paste_seal.js
                ${pkgs.runtimeShell} e2e/prigh_web.sh
                mkdir "$out"
              ''
          else
            pkgs.runCommand "prigh-web-e2e-skipped" { } ''touch "$out"'';

        # desktop/test: the app in a real window (WebKitGTK under Xvfb, software
        # rendering) against a fake `prigh desktop` (the failure paths) and the
        # real one with a `prigh serve -web` backend.
        checks.desktop-window =
          if isLinux then
            pkgs.runCommand "prigh-desktop-window"
              {
                nativeBuildInputs = [
                  pkgs.xvfb-run
                  pkgs.xdotool
                  pkgs.dbus
                  pkgs.python3
                  pkgs.curl
                  pkgs.procps
                  pkgs.openbox
                ];
                __EGL_VENDOR_LIBRARY_FILENAMES = "${pkgs.mesa}/share/glvnd/egl_vendor.d/50_mesa.json";
                LIBGL_ALWAYS_SOFTWARE = "1";
                WEBKIT_DISABLE_DMABUF_RENDERER = "1";
                # the browser window with a real Chromium (Playwright's, as the
                # web e2e uses)
                REAL_CHROMIUM = "${pkgs.playwright-driver.browsers}/chromium-${pkgs.playwright-driver.passthru.browsersJSON.chromium.revision}/chrome-linux64/chrome";
              }
              ''
                cp -r ${./desktop/test} test
                chmod -R u+w test
                patchShebangs test
                xvfb-run -a -s "-screen 0 1920x1200x24" dbus-run-session --config-file=${pkgs.dbus}/share/dbus-1/session.conf -- \
                  bash test/run.sh ${prighDesktop}/bin/prigh-desktop ${prigh}/bin/prigh > actual.txt 2> stderr.txt || { cat stderr.txt actual.txt; exit 1; }
                diff -u test/expected.txt actual.txt
                # what the wrapper and the desktop entry say
                grep -q 'PRIGH_DESKTOP_BACKEND.*${prigh}/bin/prigh' ${prighDesktop}/bin/prigh-desktop
                grep -qx 'Exec=${prighDesktop}/bin/prigh-desktop' ${prighDesktop}/share/applications/prigh.desktop
                grep -qx 'Name=prigh' ${prighDesktop}/share/applications/prigh.desktop
                grep -qx 'Icon=prigh-desktop' ${prighDesktop}/share/applications/prigh.desktop
                test -f ${prighDesktop}/share/icons/hicolor/128x128/apps/prigh-desktop.png
                # WebKitGTK's GStreamer plugins (media, the microphone)
                # (a binary wrapper: the paths are among its strings)
                grep -aq GST_PLUGIN_SYSTEM_PATH_1_0 ${prighDesktop.unwrapped}/bin/prigh-desktop
                for plugins in gst-plugins-base gst-plugins-good; do
                  grep -aq "$plugins-[^/]*/lib/gstreamer-1.0" ${prighDesktop.unwrapped}/bin/prigh-desktop
                done
                touch "$out"
              ''
          else
            pkgs.runCommand "prigh-desktop-window-skipped" { } ''touch "$out"'';

        # desktop/computer/test: prigh-computer on a headless sway (wlroots)
        # and Xvfb (X11), each with an app printing the input it gets, and
        # an exec through `prigh tool-host -computer`.
        checks.computer-use =
          if isLinux then
            pkgs.runCommand "prigh-computer-use"
              {
                nativeBuildInputs = [
                  pkgs.sway-unwrapped
                  pkgs.wev
                  pkgs.xvfb
                  pkgs.xev
                  pkgs.procps
                ];
              }
              ''
                cp -r ${./desktop/computer/test} test
                chmod -R u+w test
                patchShebangs test
                bash test/run.sh ${prighComputer}/bin/prigh-computer ${prigh}/bin/prigh > actual.txt 2> stderr.txt || { cat stderr.txt actual.txt; exit 1; }
                diff -u test/expected.txt actual.txt
                touch "$out"
              ''
          else
            pkgs.runCommand "prigh-computer-use-skipped" { } ''touch "$out"'';

        # nix/home-manager.nix, evaluated with stand-ins for the two Home
        # Manager options it sets.
        checks.home-manager-module =
          let
            stub =
              { lib, ... }:
              {
                options.home.packages = lib.mkOption {
                  type = lib.types.listOf lib.types.package;
                  default = [ ];
                };
                options.assertions = lib.mkOption {
                  type = lib.types.listOf lib.types.unspecified;
                  default = [ ];
                };
                config._module.args.pkgs = pkgs;
              };
            eval =
              config:
              (lib.evalModules {
                modules = [
                  self.homeManagerModules.default
                  stub
                  config
                ];
              }).config;
            names = c: lib.concatMapStringsSep " " (p: p.name) c.home.packages;
            failing = c: lib.concatMapStrings (a: a.message) (lib.filter (a: !a.assertion) c.assertions);
            off = eval { };
            cli = eval { programs.prigh.enable = true; };
            desktop = eval {
              programs.prigh = {
                enable = true;
                desktop = {
                  enable = true;
                  extraArgs = [
                    "-cwd"
                    "/my proj"
                  ];
                  disableDmabuf = true;
                  computerUse = true;
                  window = "browser";
                  browser = pkgs.writeShellScriptBin "my-chrome" "";
                };
              };
            };
            secret = eval {
              programs.prigh = {
                enable = true;
                desktop.extraArgs = [
                  "-name"
                  "x"
                  "-token=sekrit"
                ];
              };
            };
            app = lib.last desktop.home.packages;
            localUi = lib.last
              (eval {
                programs.prigh = {
                  enable = true;
                  desktop = {
                    enable = true;
                    ui = "local";
                  };
                };
              }).home.packages;
            systemNixpkgs = lib.last
              (eval {
                programs.prigh = {
                  enable = true;
                  desktop = {
                    enable = true;
                    useSystemNixpkgs = true;
                  };
                };
              }).home.packages;
          in
          pkgs.runCommand "prigh-home-manager-module" { } ''
            test "${names off}" = ""
            test "${names cli}" = "prigh"
            test "${names desktop}" = "prigh prigh-desktop-${desktopVersion}"
            test -z ${lib.escapeShellArg (failing desktop)}
            echo ${lib.escapeShellArg (failing secret)} | grep -q 'programs.prigh.desktop.extraArgs has -token=sekrit:'
            # the desktop app was given the options
            wrapper=${app}/bin/prigh-desktop
            grep -q 'PRIGH_DESKTOP_BACKEND.*${prigh}/bin/prigh' "$wrapper"
            grep -q "PRIGH_DESKTOP_ARGS.*-ui remote -cwd.*/my proj" "$wrapper"
            grep -q 'WEBKIT_DISABLE_DMABUF_RENDERER.*1' "$wrapper"
            grep -q 'PRIGH_DESKTOP_WINDOW.*browser' "$wrapper"
            grep -q 'PRIGH_DESKTOP_BROWSER.*/bin/my-chrome' "$wrapper"
            grep -q "PRIGH_DESKTOP_ARGS.*/my proj.* -computer" "$wrapper"
            grep -q 'PRIGH_COMPUTER.*${prighComputer}/bin/prigh-computer' "$wrapper"
            # the default: auto
            grep -q 'PRIGH_DESKTOP_WINDOW.*auto' ${localUi}/bin/prigh-desktop
            ! grep -q PRIGH_DESKTOP_BROWSER ${localUi}/bin/prigh-desktop
            ! grep -q -- '-computer\|PRIGH_COMPUTER' ${localUi}/bin/prigh-desktop
            grep -q 'PRIGH_DESKTOP_ARGS.*-ui local' ${localUi}/bin/prigh-desktop
            # built from this nixpkgs: the same derivation as the flake's here
            test "${systemNixpkgs.unwrapped}" = "${prighDesktopUnwrapped}"
            grep -q 'PRIGH_DESKTOP_ARGS.*-ui remote' ${systemNixpkgs}/bin/prigh-desktop
            touch "$out"
          '';

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
        # desktop/: Rust, cargo-tauri and the webview's libraries, apart from
        # the OxCaml shell so that one stays lean.
        devShells.desktop = pkgs.mkShell {
          inputsFrom = [
            prighDesktopUnwrapped
            prighComputer
          ];
          packages = [
            pkgs.cargo-tauri
            pkgs.clippy
            pkgs.rustfmt
            pkgs.rust-analyzer
          ];
        };
      }
    )
    // {
      homeManagerModules.default = self.homeManagerModules.prigh;
      homeManagerModules.prigh = import ./nix/home-manager.nix self;
    };
}
