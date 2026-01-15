{
  description = "Description for ocaml project";

  inputs = {
    # nixpkgs.url = "github:nix-ocaml/nix-overlays";
    nixpkgs.url = "github:nixos/nixpkgs/nixos-unstable";
    flake-parts.url = "github:hercules-ci/flake-parts";
  };

  outputs = inputs @ {flake-parts, ...}:
    flake-parts.lib.mkFlake {inherit inputs;} {
      systems = ["x86_64-linux" "aarch64-linux" "aarch64-darwin" "x86_64-darwin"];

      perSystem = {
        self',
        pkgs,
        ...
      }: let
        inherit (pkgs) mkShell;
        # Use specific version of ocamlPackages
        # inherit (pkgs) ocamlPackages;
        # ocamlPackages = pkgs.ocaml-ng.ocamlPackages_5_3;
        ocamlPackages = pkgs.ocamlPackages_latest;
        inherit (ocamlPackages) buildDunePackage;
        name = "ecosify";
        version = "0.0.1";
      in {
        devShells = {
          default = mkShell {
            inputsFrom = [self'.packages.default];
            buildInputs = with ocamlPackages; [
              utop
              ocamlformat
              ocaml-lsp
            ];
          };
        };

        packages = {
          default = buildDunePackage {
            inherit version;
            pname = name;
            src = ./.;
            buildInputs = with ocamlPackages; [
              core
              ppx_jane
            ];
          };
        };
      };
    };
}
