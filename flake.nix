{
  description = "Symbolic evaluation as a library";
  inputs.nixpkgs.url = "github:NixOS/nixpkgs/nixos-unstable";
  inputs.flake-utils.url = "github:numtide/flake-utils";
  inputs.grisette-nix-build-env.url = "github:lsrcz/grisette-nix-build-env/main";

  outputs =
    {
      self,
      nixpkgs,
      flake-utils,
      grisette-nix-build-env,
    }:
    flake-utils.lib.eachDefaultSystem (
      system:
      grisette-nix-build-env.lib.${system}.output {
        inherit nixpkgs system;
        srcRoot = ./.;
        extraHaskellPackages =
          pkgs: ghcVersion: hfinal: helpers: setCIOptions: with helpers; {
            grisette = setCIOptions {
              extraTestToolDepends = byGhcVersion {
                "8107" = [ pkgs.z3 ];
                "902" = [ pkgs.z3 ];
                default = [
                  pkgs.z3
                  pkgs.bitwuzla
                ];
              };
              package = hfinal.callCabal2nix "grisette" ./. { };
              mixDirs = [
                ""
                "spec/spec-tmp"
                "doctest/doctest-tmp"
              ];
            };

          };
        devShellExtraBuildInputs =
          pkgs:
          {
            bitwuzla ? false,
            isDevelopmentEnvironment ? false,
            ...
          }:
          [
            pkgs.boolector
            pkgs.cvc5
          ]
          ++ (if bitwuzla then [ pkgs.bitwuzla ] else [ ])
          ++ (
            if isDevelopmentEnvironment then
              [
                (pkgs.ihaskell.override {
                  ghcWithPackages = pkgs.haskellPackages.ghcWithPackages;
                })
              ]
            else
              [ ]
          );
        treefmtExcludes = [
          "tutorials/*.ipynb"
          "tutorials/*.svg"
          ".gdbinit"
        ];
        pname = "grisette";
        extraOutputs = pkgs: haskellPackages: devShellWithVersion: {
          devShells = {
            "9102" = devShellWithVersion {
              ghcVersion = "9102";
              config = {
                isDevelopmentEnvironment = true;
                bitwuzla = true;
              };
            };
            default = devShellWithVersion {
              ghcVersion = "9102";
              config = {
                isDevelopmentEnvironment = true;
                bitwuzla = true;
              };
            };
            "9122" = devShellWithVersion {
              ghcVersion = "9122";
              config = {
                isDevelopmentEnvironment = false;
                bitwuzla = true;
              };
            };
          };
        };
      }
    );
}
