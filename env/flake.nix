{
  description = "mgsloan's user environment";

  inputs = {
    nixpkgs.url = "github:NixOS/nixpkgs/nixos-unstable";

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
          inherit (packages) asdcontrol darkman dunst keynav tools waynav;

          cli-smoke = nixpkgs.legacyPackages.${system}.runCommand "cli-smoke" {} ''
            ${packages.tools}/bin/bat --version
            ${packages.tools}/bin/rg --version
            ${packages.tools}/bin/shellcheck --version
            touch "$out"
          '';
        });
    };
}
