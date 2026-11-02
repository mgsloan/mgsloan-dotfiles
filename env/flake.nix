{
  description = "mgsloan's user environment";

  inputs = {
    nixpkgs.url = "github:NixOS/nixpkgs/nixos-unstable";

    spotifast = {
      url = "github:crmne/spotifast/v0.16.0";
      inputs.nixpkgs.follows = "nixpkgs";
    };

    ghostty-src = {
      url = "github:ghostty-org/ghostty/v1.4.0";
      flake = false;
    };
    river-src = {
      url = "git+https://codeberg.org/river/river.git?ref=refs/tags/v0.5.0";
      flake = false;
    };
    wlroots-src = {
      url = "gitlab:wlroots/wlroots/0.20.2?host=gitlab.freedesktop.org";
      flake = false;
    };

    nixgl-src = {
      url = "github:nix-community/nixGL";
      flake = false;
    };

    asdcontrol-src = {
      url = "github:nikosdion/asdcontrol";
      flake = false;
    };
    darkman-src = {
      url = "gitlab:WhyNotHugo/darkman/v2.3.1";
      flake = false;
    };
    dunst-src = {
      url = "github:dunst-project/dunst/v1.13.2";
      flake = false;
    };
    keynav-src = {
      url = "github:mgsloan/keynav";
      flake = false;
    };
    waynav-src = {
      # personal fixes for cursor positioning and Shift bindings
      url = "github:mgsloan/waynav/nix-source";
      flake = false;
    };
  };

  outputs = inputs@{ self, nixpkgs, ... }:
    let
      supportedSystems = [ "x86_64-linux" ];
      forEachSystem = nixpkgs.lib.genAttrs supportedSystems;
      pkgsForSystem = system: import nixpkgs {
        inherit system;
        config.allowUnfree = true;
      };
    in {
      packages = forEachSystem (system:
        import ./nix/packages.nix {
          inherit inputs;
          pkgs = pkgsForSystem system;
        });

      checks = forEachSystem (system:
        let packages = self.packages.${system};
        in {
          inherit (packages) asdcontrol darkman dunst errlog-filter ghostty keynav river same-spot same-spot-review spotifast tools waynav;

          cli-smoke = (pkgsForSystem system).runCommand "cli-smoke" {} ''
            ${packages.tools}/bin/bat --version
            ${packages.tools}/bin/rg --version
            ${packages.tools}/bin/shellcheck --version
            ${packages.tools}/bin/stack --numeric-version
            ${packages.tools}/bin/pnpm --version
            ${packages.tools}/bin/wasm-pack --version
            CLOUDSDK_CONFIG="$TMPDIR/gcloud" ${packages.tools}/bin/gcloud version
            ${packages.tools}/bin/zig version
            ${packages.tools}/bin/typst --version
            ${packages.tools}/bin/envsubst --version
            # An unknown operation validates loading without accessing credentials.
            ${packages.tools}/bin/git-credential-libsecret env-nix-smoke
            touch "$out"
          '';
        });
    };
}
