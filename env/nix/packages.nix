{ inputs, pkgs }:

let
  inherit (pkgs) lib;

  nixgl = (import "${inputs.nixgl-src}/default.nix" { inherit pkgs; }).nixGLIntel;
  spotifast = inputs.spotifast.packages.${pkgs.stdenv.hostPlatform.system}.spotifast;

  logdrain = pkgs.rustPlatform.buildRustPackage rec {
    pname = "logdrain-cli";
    version = "0.3.2";
    src = pkgs.fetchCrate {
      inherit pname version;
      hash = "sha256-zn0ocraPXimpT8sk2Q2ZchKl01UQPDGHXfGw03YfBRI=";
    };
    cargoHash = "sha256-cwQHESxmUrPDAyTf9z/Jhatp4KXN9OFbBDt5akaFx6Q=";
    meta = {
      description = "Streaming log template miner";
      homepage = "https://github.com/vnvo/logdrain";
      license = with lib.licenses; [ asl20 mit ];
      mainProgram = "logdrain";
    };
  };

  privateFonts = pkgs.writeText "desktop-fonts.conf" ''
    <?xml version="1.0"?>
    <!DOCTYPE fontconfig SYSTEM "urn:fontconfig:fonts.dtd">
    <fontconfig>
      <dir>${pkgs.hack-font}/share/fonts/truetype</dir>
      <dir>${pkgs.dejavu_fonts}/share/fonts/truetype</dir>
      <dir>${pkgs.noto-fonts-color-emoji}/share/fonts/noto</dir>
      <cachedir prefix="xdg">fontconfig</cachedir>
      <include>${pkgs.fontconfig.out}/etc/fonts/conf.d/10-scale-bitmap-fonts.conf</include>
      <alias><family>monospace</family><prefer><family>Hack</family></prefer></alias>
    </fontconfig>
  '';

  # A private Pango map avoids global font discovery without changing child environments.
  privateFontconfig = pkgs.writeText "private-fontconfig.h" ''
    #include <fontconfig/fontconfig.h>
    #include <pango/pangocairo.h>
    #include <pango/pangofc-fontmap.h>

    static void setup_private_fontconfig(void) {
        FcConfig *config = FcConfigCreate();
        if (!config ||
            !FcConfigParseAndLoad(config, (const FcChar8 *)"${privateFonts}", FcTrue) ||
            !FcConfigBuildFonts(config)) {
            g_error("Unable to load private fontconfig");
        }

        PangoFontMap *map = pango_cairo_font_map_new();
        pango_fc_font_map_set_config(PANGO_FC_FONT_MAP(map), config);
        pango_cairo_font_map_set_default(PANGO_CAIRO_FONT_MAP(map));
        g_object_unref(map);
        FcConfigDestroy(config);
    }
  '';

  rofiUnwrapped = pkgs.rofi-unwrapped.overrideAttrs (previous: {
    buildInputs = previous.buildInputs ++ [ pkgs.fontconfig ];
    postPatch = (previous.postPatch or "") + ''
      sed -i '1i#include "${privateFontconfig}"' source/widgets/textbox.c
      substituteInPlace source/widgets/textbox.c \
        --replace-fail 'void textbox_setup(void) {' 'void textbox_setup(void) { setup_private_fontconfig();'
      substituteInPlace meson.build \
        --replace-fail "dependency('pangocairo')," "dependency('pangocairo'), dependency('pangofc'), dependency('fontconfig'),"
      substituteInPlace config/config.c --replace-fail '"mono 12"' '"Hack 12"'
    '';
  });

  # Nix's glibc needs its own locale archive on non-NixOS hosts.
  rofi = (pkgs.rofi.override { rofi-unwrapped = rofiUnwrapped; }).overrideAttrs (previous: {
    buildCommand = previous.buildCommand + ''
      wrapProgram "$out/bin/rofi" \
        --set-default LOCALE_ARCHIVE ${pkgs.glibcLocales}/lib/locale/locale-archive
    '';
  });

  errlog-filter = pkgs.rustPlatform.buildRustPackage {
    pname = "errlog-filter";
    version = "0.2.0";
    src = lib.fileset.toSource {
      root = ../errlog-filter;
      fileset = lib.fileset.unions [
        ../errlog-filter/Cargo.toml
        ../errlog-filter/Cargo.lock
        ../errlog-filter/build.rs
        ../errlog-filter/src
        ../errlog-filter/rules.toml
        ../errlog-filter/LICENSE
      ];
    };
    cargoLock.lockFile = ../errlog-filter/Cargo.lock;
    nativeBuildInputs = [ pkgs.pkg-config pkgs.makeWrapper ];
    buildInputs = [ pkgs.systemd ];
    postInstall = ''
      install -Dm644 rules.toml "$out/share/errlog-filter/rules.toml"
      wrapProgram "$out/bin/errlog-filter" --prefix PATH : ${lib.makeBinPath [ pkgs.libnotify ]}
    '';
    meta = {
      description = "Streaming error log filter with incremental rule audits";
      license = lib.licenses.bsd3;
      platforms = lib.platforms.linux;
      mainProgram = "errlog-filter";
    };
  };

  asdcontrol = pkgs.stdenv.mkDerivation {
    pname = "asdcontrol";
    version = "unstable-2025-08-21";
    src = inputs.asdcontrol-src;

    installPhase = ''
      runHook preInstall
      install -Dm755 asdcontrol "$out/bin/asdcontrol"
      runHook postInstall
    '';

    meta = {
      description = "Apple Studio Display brightness control";
      homepage = "https://github.com/nikosdion/asdcontrol";
      license = lib.licenses.gpl3Only;
      platforms = lib.platforms.linux;
      mainProgram = "asdcontrol";
    };
  };

  keynav = pkgs.stdenv.mkDerivation {
    pname = "keynav";
    version = "0.20190518.0";
    src = inputs.keynav-src;

    nativeBuildInputs = [ pkgs.pkg-config pkgs.perl ];
    buildInputs = with pkgs; [
      cairo
      glib
      xdotool
      libx11
      libxinerama
      libxrandr
    ];

    # Without VERSION, the build embeds the current date.
    postPatch = ''
      printf '%s\n' 'MAJOR=0' 'RELEASE=20190518' 'REVISION=0' > VERSION
    '';

    makeFlags = [ "PREFIX=$(out)" ];

    meta = {
      description = "Keyboard-driven mouse cursor mover";
      homepage = "https://github.com/mgsloan/keynav";
      license = lib.licenses.bsd3;
      platforms = lib.platforms.linux;
      mainProgram = "keynav";
    };
  };

  waynav = pkgs.stdenv.mkDerivation {
    pname = "waynav";
    version = "1.2.0";
    src = inputs.waynav-src;

    nativeBuildInputs = with pkgs; [ meson ninja pkg-config wayland-scanner ];
    buildInputs = with pkgs; [ cairo libxkbcommon wayland wayland-protocols ];

    mesonFlags = [ "-Dbuild_version=1.2.0" ];

    meta = {
      description = "Keyboard-driven pointer navigation for Wayland";
      homepage = "https://github.com/mgsloan/waynav";
      license = lib.licenses.mit;
      platforms = lib.platforms.linux;
      mainProgram = "waynav";
    };
  };

  # Keep nixpkgs' dependency and configure knowledge while substituting the
  # commit pinned by this repository.
  dunst = pkgs.dunst.overrideAttrs (previous: {
    version = "1.13.2";
    src = inputs.dunst-src;
    buildInputs = previous.buildInputs ++ [ pkgs.fontconfig ];
    postPatch = (previous.postPatch or "") + ''
      sed -i '1i#include "${privateFontconfig}"' src/draw.c
      substituteInPlace src/draw.c \
        --replace-fail 'const struct output *out = output_create(settings.force_xwayland);' \
          'setup_private_fontconfig(); const struct output *out = output_create(settings.force_xwayland);'
      substituteInPlace config.mk --replace-fail '                    pangocairo' '                    pangocairo pangofc fontconfig'
    '';
  });

  # The pinned source matches this nixpkgs package's release source, so
  # its fixed dependency hash remains valid while the source is overridable.
  darkman = pkgs.darkman.overrideAttrs (_: {
    version = "2.3.1-1";
    src = inputs.darkman-src;
  });

  ghosttyUnwrapped = (pkgs.callPackage "${inputs.ghostty-src}/nix/package.nix" {
    revision = inputs.ghostty-src.shortRev or "local";
    optimize = "ReleaseFast";
  }).overrideAttrs (previous: {
    # Upstream writes package names where Nix expects store paths.
    postInstall = builtins.replaceStrings [
      ''echo "gst_all_1.gstreamer" >> "$out/nix-support/propagated-user-env-packages"''
      ''echo "gst_all_1.gst-plugins-base" >> "$out/nix-support/propagated-user-env-packages"''
      ''echo "gst_all_1.gst-plugins-good" >> "$out/nix-support/propagated-user-env-packages"''
    ] [ "" "" "" ] previous.postInstall;
  });

  ghostty = pkgs.symlinkJoin {
    name = "ghostty-${ghosttyUnwrapped.version}";
    paths = [ ghosttyUnwrapped ];
    nativeBuildInputs = [ pkgs.makeWrapper ];
    postBuild = ''
      rm "$out/bin/ghostty"
      makeWrapper ${nixgl}/bin/nixGLIntel "$out/bin/ghostty" \
        --unset LD_LIBRARY_PATH \
        --set GHOSTTY_RESOURCES_DIR ${ghosttyUnwrapped}/share/ghostty \
        --add-flags ${ghosttyUnwrapped}/bin/ghostty

      # Desktop and D-Bus activation must use the graphics wrapper too.
      for relative in share/applications/com.mitchellh.ghostty.desktop \
                      share/dbus-1/services/com.mitchellh.ghostty.service; do
        if [ -f "$out/$relative" ]; then
          cp --remove-destination "$out/$relative" "$out/$relative.tmp"
          mv "$out/$relative.tmp" "$out/$relative"
          chmod u+w "$out/$relative"
          substituteInPlace "$out/$relative" \
            --replace-warn '${ghosttyUnwrapped}/bin/ghostty' "$out/bin/ghostty"
        fi
      done
    '';
    meta = ghosttyUnwrapped.meta;
  };

  wlroots = pkgs.wlroots_0_20.overrideAttrs (_: {
    version = "0.20.1";
    src = inputs.wlroots-src;
  });

  riverUnwrapped = (pkgs.river.override { wlroots_0_20 = wlroots; }).overrideAttrs (final: _: {
    version = "0.5.0-dev";
    src = inputs.river-src;
    zigDeps = pkgs.zig_0_16.fetchDeps {
      inherit (final) src pname version;
      fetchAll = true;
      hash = "sha256-MVFoc361EKGhz5V/9tAOc8lldAi45o592oyOfHX1vTM=";
    };
  });

  river = pkgs.symlinkJoin {
    name = "river-${riverUnwrapped.version}";
    paths = [ riverUnwrapped riverUnwrapped.man ];
    nativeBuildInputs = [ pkgs.makeWrapper ];
    postBuild = ''
      rm "$out/bin/river"
      makeWrapper ${nixgl}/bin/nixGLIntel "$out/bin/river" \
        --unset LD_LIBRARY_PATH --add-flags ${riverUnwrapped}/bin/river
    '';
    meta = riverUnwrapped.meta // { outputsToInstall = [ "out" ]; };
  };

  git-credential-libsecret = pkgs.stdenv.mkDerivation {
    pname = "git-credential-libsecret";
    inherit (pkgs.git) version src;
    nativeBuildInputs = [ pkgs.pkg-config ];
    buildInputs = [ pkgs.libsecret ];
    dontConfigure = true;
    buildPhase = ''
      runHook preBuild
      make -C contrib/credential/libsecret
      runHook postBuild
    '';
    installPhase = ''
      runHook preInstall
      install -Dm755 contrib/credential/libsecret/git-credential-libsecret \
        "$out/bin/git-credential-libsecret"
      runHook postInstall
    '';
  };

  commandLinePackages = {
    inherit git-credential-libsecret logdrain;
    zig = pkgs.zig_0_16;
    inherit (pkgs) gettext typst;
    inherit (pkgs) bat gh joshuto just lychee pandoc qpdf ripgrep shellcheck uv xidlehook;
    inherit (pkgs) google-cloud-sdk pnpm stack wasm-pack;
  };
  commandLineTools = builtins.attrValues commandLinePackages;

  # Executables referenced by tracked configuration and scripts.
  publicDependencies = with pkgs; [
    alacritty
    bubblewrap
    byzanz
    ccze
    curl
    earlyoom
    emacs-gtk
    feh
    ffmpeg
    flameshot
    fuzzel
    gammastep
    gist
    gitFull
    grim
    maim
    networkmanager
    playerctl
    powertop
    pulseaudio
    python3
    redshift
    rofi
    scrot
    slock
    slurp
    spotify
    spotifast
    swaybg
    swayidle
    tesseract
    tmux
    wf-recorder
    wl-clipboard
    wlopm
    wlr-randr
    xclip
    xdotool
  ];

  sourceBuilds = [ asdcontrol darkman dunst errlog-filter keynav waynav ];

  updateDesktopDatabase = ''
    if [ -d "$out/share/applications" ]; then
      ${pkgs.desktop-file-utils}/bin/update-desktop-database "$out/share/applications"
    fi
  '';
in
commandLinePackages // rec {
  inherit asdcontrol darkman dunst errlog-filter ghostty keynav nixgl river rofi spotifast waynav wlroots;

  graphics-check = pkgs.writeShellApplication {
    name = "env-nix-graphics-check";
    runtimeInputs = [ nixgl pkgs.mesa-demos pkgs.gnugrep ];
    text = ''
      renderer=$(nixGLIntel glxinfo -B)
      printf '%s\n' "$renderer"
      grep -q 'Accelerated: yes' <<< "$renderer"
    '';
  };

  cli-tools = pkgs.buildEnv {
    name = "mgsloan-cli-tools";
    paths = commandLineTools;
  };

  source-tools = pkgs.buildEnv {
    name = "mgsloan-source-tools";
    paths = sourceBuilds;
  };

  tools = cli-tools;
  desktop = pkgs.buildEnv {
    name = "mgsloan-desktop";
    paths = sourceBuilds ++ [ ghostty river ];
    postBuild = updateDesktopDatabase;
  };

  environment = pkgs.buildEnv {
    name = "mgsloan-environment";
    paths = commandLineTools ++ publicDependencies ++ sourceBuilds ++ [ ghostty river ];
    ignoreCollisions = true;
    postBuild = updateDesktopDatabase;
  };

  default = environment;
}
