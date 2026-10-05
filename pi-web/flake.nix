{
  description = "prigh with pi's web UI — the pi web frontend (Preact/TS) on the prigh backend";

  inputs = {
    nixpkgs.url = "github:NixOS/nixpkgs/nixos-unstable";
    flake-utils.url = "github:numtide/flake-utils";
    # The parent flake (the prigh backend). Resolved inside the same source
    # tree, so `nix run ./pi-web` from the repository root works.
    prigh.url = "path:..";
  };

  outputs =
    {
      self,
      nixpkgs,
      flake-utils,
      prigh,
    }:
    flake-utils.lib.eachDefaultSystem (
      system:
      let
        pkgs = nixpkgs.legacyPackages.${system};
        backend = prigh.packages.${system}.backend;
        tui = prigh.packages.${system}.tui;

        # The built site (vite): dist/ with index.html, assets/, theme/, icons/.
        site = pkgs.buildNpmPackage {
          pname = "prigh-pi-web";
          version = "0.1.0";
          src = pkgs.lib.cleanSourceWith {
            src = ./.;
            filter =
              path: _type:
              let
                name = baseNameOf path;
              in
              name != "node_modules" && name != "dist" && name != "e2e" && name != "flake.nix" && name != "flake.lock";
          };
          npmDepsHash = "sha256-FAK5OvBZvLiBuogNbgAfaKqQXdt1ImEWr/eY1wtzUF0=";
          npmBuildScript = "build";
          # The terminal panel's script and xterm.js (see vite.config.ts).
          PRIGH_TERMINAL_ASSETS = "${prigh}/tui/web-bin";
          # Type-check and unit-test before bundling.
          preBuild = ''
            npm run check
            npm test
          '';
          installPhase = ''
            runHook preInstall
            cp -r dist "$out"
            runHook postInstall
          '';
        };

        # `prigh-pi-web [serve options]`: the backend serving the pi web UI on
        # $PRIGH_PI_WEB_LISTEN (default 127.0.0.1:7789), opening a browser.
        # PRIGH_WEB_ROOT is set too so that `-web HOST:PORT` among the serve
        # options makes the same backend serve the Bonsai web UI as well.
        prigh-pi-web = pkgs.writeShellApplication {
          name = "prigh-pi-web";
          text = ''
            export PRIGH_BACKEND="''${PRIGH_BACKEND:-${backend}/bin/prigh}"
            export PRIGH_PI_WEB_ROOT="''${PRIGH_PI_WEB_ROOT:-${site}}"
            export PRIGH_WEB_ROOT="''${PRIGH_WEB_ROOT:-${tui}/share/prigh_tui/web}"
            export PRIGH_TMUX="''${PRIGH_TMUX:-${pkgs.tmux}/bin/tmux}"
            exec "$PRIGH_BACKEND" serve -pi-web "''${PRIGH_PI_WEB_LISTEN:-127.0.0.1:7789}" -open "$@"
          '';
        };
      in
      {
        packages = {
          default = prigh-pi-web;
          inherit site;
        };

        apps.default = {
          type = "app";
          program = "${prigh-pi-web}/bin/prigh-pi-web";
        };

        checks.wrapper = pkgs.runCommand "prigh-pi-web-wrapper-test" { } ''
          cat > fake-backend <<'EOF'
          #!${pkgs.runtimeShell}
          printf '%s\n' "$@" > "$ARGS_OUT"
          printf '%s\n' "$PRIGH_PI_WEB_ROOT" > "$ROOT_OUT"
          printf '%s\n' "$PRIGH_WEB_ROOT" > "$WEB_ROOT_OUT"
          EOF
          chmod +x fake-backend
          ARGS_OUT="$PWD/args" ROOT_OUT="$PWD/root" WEB_ROOT_OUT="$PWD/web-root" \
            PRIGH_BACKEND="$PWD/fake-backend" PRIGH_PI_WEB_LISTEN=0.0.0.0:7777 \
            ${prigh-pi-web}/bin/prigh-pi-web -token sekrit -cwd /work -web 0.0.0.0:7788
          diff -u ${pkgs.writeText "expected-args" ''
            serve
            -pi-web
            0.0.0.0:7777
            -open
            -token
            sekrit
            -cwd
            /work
            -web
            0.0.0.0:7788
          ''} args
          test "$(cat root)" = "${site}"
          test "$(cat web-root)" = "${tui}/share/prigh_tui/web"
          test -f ${tui}/share/prigh_tui/web/index.html
          test -f ${site}/index.html
          test -f ${site}/theme/dark.json
          test -f ${site}/theme/light.json
          ls ${site}/assets/*.js >/dev/null
          test -f ${site}/xterm/terminal.js
          test -f ${site}/xterm/xterm.js
          test -f ${site}/xterm/xterm.css
          test -f ${site}/xterm/addon-fit.js
          touch "$out"
        '';

        # Real browsers (Playwright) against `prigh serve -pi-web -faux-script`,
        # see e2e/pi_web.sh; compared with e2e/pi_web.expected.
        checks.e2e =
          if pkgs.stdenv.hostPlatform.isLinux then
            pkgs.runCommand "prigh-pi-web-e2e"
              {
                nativeBuildInputs = [
                  pkgs.curl
                  pkgs.nodejs
                  pkgs.playwright-test
                  pkgs.playwright-driver.browsers
                  pkgs.dejavu_fonts
                  pkgs.tmux
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
                export PRIGH_BACKEND=${backend}/bin/prigh
                export PRIGH_PI_WEB_ROOT=${site}
                export HOME="$PWD/home"
                mkdir -p "$HOME"
                # the terminal panel's shell (tmux under the backend)
                export SHELL=${pkgs.bashInteractive}/bin/bash
                export TMUX_TMPDIR="$PWD/tmux"
                mkdir -p "$TMUX_TMPDIR"
                cp -r ${./e2e} e2e
                chmod -R u+w e2e
                ${pkgs.runtimeShell} e2e/pi_web.sh
                mkdir "$out"
              ''
          else
            pkgs.runCommand "prigh-pi-web-e2e-skipped" { } ''touch "$out"'';

        devShells.default = pkgs.mkShell {
          buildInputs = [
            pkgs.nodejs
            backend
          ];
          shellHook = ''
            export PRIGH_BACKEND=${backend}/bin/prigh
          '';
        };
      }
    );
}
