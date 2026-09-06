{
  description = "mgsloan's user environment";

  inputs = {
    nixpkgs.url = "github:NixOS/nixpkgs/nixos-unstable";

    ghostty-src = {
      url = "github:ghostty-org/ghostty/f426f6f181ba95f45d33f683fb754b6359d9e04f";
      flake = false;
    };
    river-src = {
      url = "git+https://codeberg.org/river/river.git?rev=67379a2c8ce6f8ac143cab25d60124992e6ecce3";
      flake = false;
    };
    wlroots-src = {
      url = "gitlab:wlroots/wlroots/0.20.1?host=gitlab.freedesktop.org";
      flake = false;
    };

    nixgl-src = {
      url = "github:nix-community/nixGL/b6105297e6f0cd041670c3e8628394d4ee247ed5";
      flake = false;
    };

    asdcontrol-src = {
      url = "github:nikosdion/asdcontrol/0ee8bd576d4e93513027d713e688988cb0d827ef";
      flake = false;
    };
    darkman-src = {
      url = "gitlab:WhyNotHugo/darkman/64dd79f3b7ed946a1c04244e991b7a78e0fbfd76";
      flake = false;
    };
    dunst-src = {
      url = "github:dunst-project/dunst/312ed56fbcdc84823c99f21008467561394a8eb1";
      flake = false;
    };
    keynav-src = {
      url = "github:mgsloan/keynav/8012f9f1a557d09852e63cea7e524cbf60b70343";
      flake = false;
    };
    waynav-src = {
      url = "github:mgsloan/waynav/e779c2f94ebbc09b9ba2f59a9b4a45b075183a61";
      flake = false;
    };
  };

  outputs = inputs@{ self, nixpkgs, ... }:
    let
      supportedSystems = [ "x86_64-linux" ];
      forEachSystem = nixpkgs.lib.genAttrs supportedSystems;
    in {
      packages = forEachSystem (system:
        import ./nix/packages.nix {
          inherit inputs;
          pkgs = nixpkgs.legacyPackages.${system};
        });

      checks = forEachSystem (system:
        let packages = self.packages.${system};
        in {
          inherit (packages) asdcontrol darkman dunst ghostty keynav river tools waynav;

          cli-smoke = nixpkgs.legacyPackages.${system}.runCommand "cli-smoke" {} ''
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
