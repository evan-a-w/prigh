# The Home Manager module (flake output homeManagerModules.default):
#
#   imports = [ prigh.homeManagerModules.default ];
#   programs.prigh.enable = true;          # prigh on the PATH
#   programs.prigh.desktop.enable = true;  # and the desktop app in the menus
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
  desktopPackage =
    if cfg.desktop.package ? override then
      cfg.desktop.package.override {
        backend = cfg.package;
        inherit (cfg.desktop) extraArgs disableDmabuf;
      }
    else
      cfg.desktop.package;
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
        prigh's desktop app: prigh-web in a native window (the system
        webview), running `prigh desktop` (prigh-web's site and a tool host
        for this machine) while it is open. Adds "prigh" to the application
        menu on Linux and ~/Applications/Home Manager Apps on macOS. With no
        saved connection it asks for the backend to connect to
      '';

      package = mkOption {
        type = types.package;
        default = self.packages.${system}.prigh-desktop;
        defaultText = lib.literalExpression "prigh.packages.\${system}.prigh-desktop";
        description = ''
          The desktop app. When it has `override` (the flake's has), it is
          given programs.prigh.package, extraArgs and disableDmabuf.
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
