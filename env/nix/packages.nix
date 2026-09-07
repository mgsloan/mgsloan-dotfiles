{ inputs, pkgs }:

let
  inherit (pkgs) lib;

  nixgl = (import "${inputs.nixgl-src}/default.nix" { inherit pkgs; }).nixGLIntel;
  fastpotify = inputs.fastpotify.packages.${pkgs.stdenv.hostPlatform.system}.fastpotify;

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

  # Nix's glibc needs its own locale archive on non-NixOS hosts.
  rofi = pkgs.rofi.overrideAttrs (previous: {
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
  dunst = pkgs.dunst.overrideAttrs (_: {
    version = "1.13.2";
    src = inputs.dunst-src;
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
    byzanz
    ccze
    curl
    earlyoom
    emacs-gtk
    fastpotify
    feh
    ffmpeg
    flameshot
    fuzzel
    gammastep
    gist
    gitFull
    grim
    libnotify
    maim
    networkmanager
    playerctl
    powertop
    python3
    redshift
    rofi
    scrot
    slock
    slurp
    spotify
    swaybg
    swayidle
    swaylock
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
in
commandLinePackages // rec {
  inherit asdcontrol darkman dunst errlog-filter fastpotify ghostty keynav nixgl river waynav wlroots;

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
  };

  environment = pkgs.buildEnv {
    name = "mgsloan-environment";
    paths = commandLineTools ++ publicDependencies ++ sourceBuilds ++ [ ghostty river ];
    ignoreCollisions = true;
  };

  default = environment;
}
