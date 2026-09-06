{ inputs, pkgs }:

let
  inherit (pkgs) lib;

  nixgl = (import "${inputs.nixgl-src}/default.nix" { inherit pkgs; }).nixGLIntel;

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

  # The home-repo submodule matches this nixpkgs package's release source, so
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

  commandLinePackages = {
    inherit (pkgs) bat joshuto just lychee pandoc qpdf ripgrep shellcheck uv xidlehook;
    inherit (pkgs) google-cloud-sdk pnpm stack wasm-pack;
  };
  commandLineTools = builtins.attrValues commandLinePackages;

  sourceBuilds = [ asdcontrol darkman dunst keynav waynav ];
in
commandLinePackages // rec {
  inherit asdcontrol darkman dunst ghostty keynav nixgl river waynav wlroots;

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

  # Service-managed tools remain opt-in until their activation is migrated.
  environment = pkgs.buildEnv {
    name = "mgsloan-environment";
    paths = commandLineTools ++ [ ghostty river ];
  };

  default = environment;
}
