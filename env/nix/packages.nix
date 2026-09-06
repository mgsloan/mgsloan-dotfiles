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

  commandLinePackages = {
    inherit (pkgs) bat joshuto just lychee pandoc qpdf ripgrep shellcheck uv xidlehook;
  };
  commandLineTools = builtins.attrValues commandLinePackages;

  sourceBuilds = [ asdcontrol darkman dunst keynav waynav ];
in
commandLinePackages // rec {
  inherit asdcontrol darkman dunst keynav nixgl waynav;

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
  desktop = source-tools;

  # Desktop packages remain opt-in until service and login validation succeeds.
  environment = tools;

  default = environment;
}
