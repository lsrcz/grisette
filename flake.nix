{
  description = "Symbolic evaluation as a library";
  inputs.nixpkgs.url = "github:NixOS/nixpkgs/nixos-unstable";
  inputs.flake-utils.url = "github:numtide/flake-utils";
  inputs.grisette-nix-build-env.url = "github:lsrcz/grisette-nix-build-env/main";

  outputs = { self, nixpkgs, flake-utils, grisette-nix-build-env }:
    flake-utils.lib.eachDefaultSystem (system:
      let
        envLib = grisette-nix-build-env.lib.${system};

        pkgs = import nixpkgs {
          inherit system;
          overlays = [
            envLib.overlays.z3
            envLib.overlays.hlintSrc
            envLib.overlays.cvc5
          ];
        };

        patchedHPkgs = { ghcVersion }: envLib.patchedHaskellPackages {
          inherit pkgs;
          ghcVersion = ghcVersion;
        };

        hPkgs = { ghcVersion, ci }:
          (patchedHPkgs { inherit ghcVersion; }).extend (hfinal: hprev:
            with envLib.haskellPackagesExtendHelpers
              {
                inherit pkgs hfinal hprev ghcVersion;
              };
            {
              grisette = envLib.setCIOptions {
                inherit pkgs ghcVersion hfinal ci;
                extraTestToolDepends = byGhcVersion {
                  "8107" = [ pkgs.z3 ];
                  "902" = [ pkgs.z3 ];
                  default = [ pkgs.z3 pkgs.bitwuzla ];
                };
                package = hfinal.callCabal2nix "grisette" ./. { };
                mixDirs = [ "" "spec/spec-tmp" "doctest/doctest-tmp" ];
              };
            });

        devShellWithVersion =
          { ghcVersion
          , isDevelopmentEnvironment ? false
          , bitwuzla ? false
          }:
          envLib.devShell {
            inherit pkgs isDevelopmentEnvironment;
            haskellPackages = patchedHPkgs { inherit ghcVersion; };
            extraBuildInputs = [
              pkgs.boolector
              pkgs.cvc5
            ] ++ (if bitwuzla then [ pkgs.bitwuzla ] else [ ]);
          };

        plainOutputs = envLib.defaultOutputs {
          inherit pkgs devShellWithVersion;
          haskellPackagesWithCiFlags = hPkgs;
          packageName = "grisette";
        };

        grisette = (hPkgs {
          ghcVersion = envLib.developmentGhcVersion;
          ci = false;
        }).grisette;
      in
      pkgs.lib.recursiveUpdate plainOutputs
        {
          formatter = pkgs.nixpkgs-fmt;

          devShells = {
            "9101" = devShellWithVersion {
              ghcVersion = "9101";
              isDevelopmentEnvironment = true;
              bitwuzla = true;
            };
            default = devShellWithVersion {
              ghcVersion = "9101";
              isDevelopmentEnvironment = true;
              bitwuzla = true;
            };
            "9121" = devShellWithVersion {
              ghcVersion = "9121";
              bitwuzla = true;
            };
          };

          packages.grisette = grisette;
          packages.default = grisette;
        });
}
