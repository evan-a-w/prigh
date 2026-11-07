# The desktop app (desktop/): a Tauri shell, prigh-web in the system webview,
# running `prigh desktop` while it is open. A function of `pkgs`, so that the
# Home Manager module can build it with the system's nixpkgs: WebKitGTK and
# Mesa then match the graphics driver NixOS installs (/run/opengl-driver);
# from another nixpkgs, WebKitGTK may fall back to drawing on the CPU.
#
# `unwrapped` finds prigh on the PATH; `wrapped` points it at `prigh` and
# takes `.override { backend; ui; window; browser; extraArgs; disableDmabuf;
# computerUse; }`.
{ pkgs, prigh }:
let
  lib = pkgs.lib;
  isLinux = pkgs.stdenv.hostPlatform.isLinux;
  desktopVersion =
    (builtins.fromTOML (builtins.readFile ../desktop/Cargo.toml)).workspace.package.version;
  desktopLinuxDeps = [
    pkgs.webkitgtk_4_1
    pkgs.libsoup_3
    pkgs.glib-networking
    pkgs.openssl
    # WebKitGTK's media, WebAudio and the microphone (dictation): without
    # them it says "GStreamer element appsink not found". wrapGAppsHook3
    # puts them on GST_PLUGIN_SYSTEM_PATH_1_0.
    pkgs.gst_all_1.gstreamer
    pkgs.gst_all_1.gst-plugins-base
    pkgs.gst_all_1.gst-plugins-good
  ];
  # The workspace: the app, its logic and the computer-use helper.
  src = lib.fileset.toSource {
    root = ../desktop;
    fileset = lib.fileset.unions [
      ../desktop/Cargo.toml
      ../desktop/Cargo.lock
      ../desktop/core
      ../desktop/computer/Cargo.toml
      ../desktop/computer/src
      ../desktop/computer/tests
      (lib.fileset.difference ../desktop/app (lib.fileset.maybeMissing ../desktop/app/gen))
    ];
  };
  # The computer-use helper (desktop/computer): `prigh tool-host -computer`
  # and `prigh desktop -computer` run it (PRIGH_COMPUTER) for the computer
  # tool. Its Wayland and X11 clients speak the protocols themselves; the
  # xdg-desktop-portal backend (GNOME, KDE) links libpipewire for the
  # screen cast (D-Bus is zbus, pure Rust).
  prighComputer = pkgs.rustPlatform.buildRustPackage {
    pname = "prigh-computer";
    version = desktopVersion;
    inherit src;
    cargoLock.lockFile = ../desktop/Cargo.lock;
    buildAndTestSubdir = "computer";
    nativeBuildInputs = [
      pkgs.pkg-config
      pkgs.rustPlatform.bindgenHook
    ];
    buildInputs = [ pkgs.pipewire ];
    # the fake portal test runs a private bus
    nativeCheckInputs = [ pkgs.dbus ];
    env.PRIGH_REQUIRE_DBUS = "1";
    meta = {
      description = "prigh's computer-use helper: screenshots and input on the desktop (wlroots Wayland, xdg-desktop-portal, X11)";
      mainProgram = "prigh-computer";
      platforms = lib.platforms.linux;
    };
  };
  prighDesktopUnwrapped = pkgs.rustPlatform.buildRustPackage {
    pname = "prigh-desktop-unwrapped";
    version = desktopVersion;
    inherit src;
    cargoLock.lockFile = ../desktop/Cargo.lock;
    buildAndTestSubdir = "app";
    nativeBuildInputs = [
      pkgs.cargo-tauri.hook
      pkgs.pkg-config
    ]
    ++ lib.optionals isLinux [ pkgs.wrapGAppsHook3 ];
    buildInputs = lib.optionals isLinux desktopLinuxDeps;
    # The logic is in prigh-desktop-core; the app's own crate has no
    # tests.
    checkPhase = ''
      runHook preCheck
      cargo test --offline -j "$NIX_BUILD_CORES" -p prigh-desktop-core
      runHook postCheck
    '';
    meta = {
      description = "prigh's desktop app, without a default prigh";
      mainProgram = "prigh-desktop";
      platforms = lib.platforms.linux ++ lib.platforms.darwin;
    };
  };
  # `.override { backend; ui; window; browser; extraArgs; disableDmabuf; computerUse; }` is what
  # programs.prigh.desktop in the Home Manager module uses.
  prighDesktop = lib.makeOverridable (
    {
      backend ? prigh,
      # remote: the backend's own prigh-web (the default of `prigh desktop`);
      # local: prigh-web served by this prigh, relayed.
      ui ? null,
      # auto, browser or webview (PRIGH_DESKTOP_WINDOW); null: the app's default, auto.
      window ? null,
      # A Chromium-family browser for the window: a package (its main
      # program) or a path (PRIGH_DESKTOP_BROWSER).
      browser ? null,
      extraArgs ? [ ],
      disableDmabuf ? false,
      # Let the agent see and drive this desktop (`prigh desktop -computer`),
      # through prighComputer (PRIGH_COMPUTER). Linux (wlroots Wayland, X11).
      computerUse ? false,
      computer ? prighComputer,
    }:
    let
      args =
        lib.optionals (ui != null) [
          "-ui"
          ui
        ]
        ++ extraArgs
        ++ lib.optional computerUse "-computer";
      env = {
        PRIGH_DESKTOP_BACKEND = "${backend}/bin/prigh";
      }
      // lib.optionalAttrs (args != [ ]) { PRIGH_DESKTOP_ARGS = lib.escapeShellArgs args; }
      // lib.optionalAttrs (window != null) { PRIGH_DESKTOP_WINDOW = window; }
      // lib.optionalAttrs (browser != null) {
        PRIGH_DESKTOP_BROWSER = if lib.isDerivation browser then lib.getExe browser else toString browser;
      }
      // lib.optionalAttrs disableDmabuf { WEBKIT_DISABLE_DMABUF_RENDERER = "1"; }
      // lib.optionalAttrs computerUse { PRIGH_COMPUTER = lib.getExe computer; };
      wrapFlags = lib.concatLists (
        lib.mapAttrsToList (name: value: [
          "--set-default"
          name
          value
        ]) env
      );
      unwrapped = prighDesktopUnwrapped;
    in
    pkgs.runCommand "prigh-desktop-${desktopVersion}"
      {
        nativeBuildInputs = [ pkgs.makeWrapper ];
        passthru = { inherit unwrapped backend; };
        meta = unwrapped.meta // {
          description = "prigh's desktop app: prigh-web in a native window, with prigh desktop running while it is open";
        };
      }
      (
        ''
          mkdir -p "$out/bin"
          makeWrapper ${unwrapped}/bin/prigh-desktop "$out/bin/prigh-desktop" ${lib.escapeShellArgs wrapFlags}
        ''
        + (
          if isLinux then
            ''
              mkdir -p "$out/share"
              ln -s ${unwrapped}/share/icons "$out/share/icons"
              mkdir "$out/share/applications"
              sed "s|^Exec=.*|Exec=$out/bin/prigh-desktop|" \
                ${unwrapped}/share/applications/prigh.desktop > "$out/share/applications/prigh.desktop"
            ''
          else
            # A Dock or Finder launch does not go through bin/: the
            # bundle's Info.plist carries the environment
            # (LSEnvironment).
            ''
              mkdir -p "$out/Applications"
              cp -R ${unwrapped}/Applications/prigh.app "$out/Applications/"
              chmod -R u+w "$out/Applications/prigh.app"
              plist="$out/Applications/prigh.app/Contents/Info.plist"
              entries=${
                lib.escapeShellArg (
                  lib.concatStrings (
                    lib.mapAttrsToList (name: value: "<key>${name}</key><string>${lib.escapeXML value}</string>") env
                  )
                )
              }
              ENTRY="<key>LSEnvironment</key><dict>$entries</dict>" awk '
                { lines[NR] = $0 }
                /<\/dict>/ { last = NR }
                END { for (i = 1; i <= NR; i++) { if (i == last) print ENVIRON["ENTRY"]; print lines[i] } }
              ' "$plist" > plist.new
              mv plist.new "$plist"
            ''
        )
      )
  ) { };
in
{
  unwrapped = prighDesktopUnwrapped;
  wrapped = prighDesktop;
  computer = prighComputer;
  version = desktopVersion;
}
