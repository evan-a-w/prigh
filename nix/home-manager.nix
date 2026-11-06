# The Home Manager module (flake output homeManagerModules.default):
#
#   imports = [ prigh.homeManagerModules.default ];
#   programs.prigh.enable = true;          # prigh on the PATH
#   programs.prigh.desktop.enable = true;  # and the desktop app in the menus
#   programs.prigh.desktop.ui = "local";   # prigh-web from this prigh (default: the backend's)
self:
{
  config,
  lib,
  pkgs,
  ...
}:
let
  cfg = config.programs.prigh;
  inherit (lib) mkEnableOption mkOption types;
  system = pkgs.stdenv.hostPlatform.system;
  # Built with the system's nixpkgs (useSystemNixpkgs), its WebKitGTK and
  # Mesa match the graphics driver NixOS installs.
  desktopBase =
    if cfg.desktop.useSystemNixpkgs then
      (import ./desktop.nix {
        inherit pkgs;
        prigh = cfg.package;
      }).wrapped
    else
      cfg.desktop.package;
  desktopPackage =
    if desktopBase ? override then
      desktopBase.override {
        backend = cfg.package;
        inherit (cfg.desktop)
          ui
          window
          browser
          extraArgs
          disableDmabuf
          ;
      }
    else
      desktopBase;
  secretFlags = [
    "-token"
    "-tokens"
    "-host-tokens"
    "-header"
  ];
  secretArgs = lib.filter (
    arg: lib.any (flag: arg == flag || lib.hasPrefix "${flag}=" arg) secretFlags
  ) cfg.desktop.extraArgs;
in
{
  options.programs.prigh = {
    enable = mkEnableOption "prigh, the coding agent (`prigh`: the TUI, prigh-web and the backend)";

    package = mkOption {
      type = types.package;
      default = self.packages.${system}.default;
      defaultText = lib.literalExpression "prigh.packages.\${system}.default";
      description = "The prigh to install; the desktop app runs this one's `prigh desktop`.";
    };

    desktop = {
      enable = mkEnableOption ''
        prigh's desktop app: prigh-web in an installed Chromium-family
        browser's app window or a native one (see window), running `prigh desktop` (prigh-web's site and a tool host
        for this machine) while it is open. Adds "prigh" to the application
        menu on Linux and ~/Applications/Home Manager Apps on macOS. With no
        saved connection it asks for the backend to connect to. Its file
        chooser is GTK's, or with GTK_USE_PORTAL=1 xdg-desktop-portal's,
        which needs a portal with a FileChooser (on NixOS
        xdg.portal.extraPortals = [ pkgs.xdg-desktop-portal-gtk ]); without
        one it falls back to GTK's
      '';

      package = mkOption {
        type = types.package;
        default = self.packages.${system}.prigh-desktop;
        defaultText = lib.literalExpression "prigh.packages.\${system}.prigh-desktop";
        description = ''
          The desktop app (when useSystemNixpkgs is off). When it has
          `override` (the flake's has), it is given programs.prigh.package,
          ui, window, browser, extraArgs and disableDmabuf.
        '';
      };

      ui = mkOption {
        type = types.enum [
          "remote"
          "local"
        ];
        default = "remote";
        description = ''
          What the window shows. remote: the backend's own prigh-web (you
          sign in there, and get the backend's version of it), with this
          machine's tool host running beside it. local: prigh-web served by
          this prigh, relayed to the backend and signed in for you. Either
          way the window asks for the backend the first time
          (`prigh desktop -ui`).
        '';
      };

      window = mkOption {
        type = types.enum [
          "auto"
          "browser"
          "webview"
        ];
        default = "auto";
        description = ''
          The window prigh-web is shown in. browser: an installed
          Chromium-family browser's app window (Google Chrome, Chromium,
          Brave, Microsoft Edge, Vivaldi; or browser), with a profile of its
          own (its own cookies, extensions and settings, apart from your
          usual one), the app ending when its last window closes. webview:
          the app's own window (WebKitGTK on Linux, WKWebView on macOS,
          WebView2 on Windows). auto: browser on Linux when one is installed
          (WebKitGTK is slower and does less), else webview
          (PRIGH_DESKTOP_WINDOW).
        '';
      };

      browser = mkOption {
        type = types.nullOr (types.either types.package types.path);
        default = null;
        example = lib.literalExpression "pkgs.chromium";
        description = ''
          The Chromium-family browser for the window (a package, run as its
          main program, or a path), instead of the first one found
          (PRIGH_DESKTOP_BROWSER).
        '';
      };

      useSystemNixpkgs = mkOption {
        type = types.bool;
        default = false;
        description = ''
          Build the desktop app with this configuration's nixpkgs rather
          than prigh's pinned one. On NixOS, WebKitGTK draws with the GPU
          only when its Mesa matches the system's driver
          (/run/opengl-driver): turn this on (or make prigh's nixpkgs input
          follow yours) if the window is slow, and check with
          PRIGH_DESKTOP_GPU_INFO=1 prigh-desktop. It is built from source
          then, not taken from prigh's binary cache.
        '';
      };

      extraArgs = mkOption {
        type = types.listOf types.str;
        default = [ ];
        example = [
          "-cwd"
          "/home/me/src"
          "-name"
          "laptop"
        ];
        description = ''
          Extra arguments for `prigh desktop` (see `prigh desktop -help`),
          passed as PRIGH_DESKTOP_ARGS (which, set in the environment,
          overrides them).

          Never put a token or password here (-token, -header with
          credentials): everything in Nix options ends up in the Nix store,
          readable by every user of the machine. Sign in from the app
          instead: the connection is saved in ~/.prigh/tui-connection.sexp
          (shared with prigh-tui), or set PRIGH_TOKEN outside Nix.
        '';
      };

      disableDmabuf = mkOption {
        type = types.bool;
        default = false;
        description = ''
          Linux: set WEBKIT_DISABLE_DMABUF_RENDERER=1, the workaround for
          WebKitGTK's blank or flickering window on some GPU drivers (NVIDIA,
          some Wayland compositors).
        '';
      };
    };
  };

  config = lib.mkIf cfg.enable {
    assertions = [
      {
        assertion = secretArgs == [ ];
        message = ''
          programs.prigh.desktop.extraArgs has ${lib.concatStringsSep ", " secretArgs}: secrets in Nix
          options are world-readable in the Nix store. Sign in from the app instead (its connect form
          saves the connection in ~/.prigh/tui-connection.sexp), or set PRIGH_TOKEN outside Nix.
        '';
      }
    ];
    home.packages = [ cfg.package ] ++ lib.optional cfg.desktop.enable desktopPackage;
  };
}
