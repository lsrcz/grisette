{
  description = "Symbolic evaluation as a library";
  inputs.nixpkgs.url = "github:NixOS/nixpkgs/nixos-unstable";
  inputs.flake-utils.url = "github:numtide/flake-utils";

  outputs = { self, nixpkgs, flake-utils }:
    flake-utils.lib.eachDefaultSystem (system:
      let
        envLib = import ./nix;

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
              grisette = (pkgs.haskell.lib.overrideCabal
                (hfinal.callCabal2nix "grisette" ./. { })
                (drv: {
                  testToolDepends = byGhcVersion {
                    "8107" = [ pkgs.z3 ];
                    "902" = [ pkgs.z3 ];
                    default = [ pkgs.z3 pkgs.bitwuzla ];
                  };
                } // (if ci then {
                  configureFlags = [
                    "--flags=-optimize"
                    "--enable-coverage"
                    "--enable-library-coverage"
                  ];
                  postCheck = ''
                    mkdir -p $out/test-report
                    cp test-report.xml $out/test-report/test-report.xml
                  '' + (if ghcVersion == "9101" then ''
                    mkdir -p $out/mix
                    cp dist/ $out/dist -r
                    cp dist/build/extra-compilation-artifacts/hpc/vanilla/mix/* $out/mix -r
                    cp dist/build/spec/spec-tmp/extra-compilation-artifacts/hpc/vanilla/mix/* $out/mix -r
                    cp dist/build/doctest/doctest-tmp/extra-compilation-artifacts/hpc/vanilla/mix/* $out/mix -r
                    mkdir -p $out/tix
                    find dist/hpc/vanilla/tix -name '*.tix' | xargs -I {} cp {} $out/tix -r
                  '' else "");
                  haddockFlags = [
                    "--html-location='https://hackage.haskell.org/package/\$pkg-\$version/docs'"
                  ];
                  postHaddock = ''
                    mkdir -p $out
                    cp -r dist/doc/ $out/haddock -r
                  '';
                } else { }))).overrideAttrs (if ci then {
                checkFlags = [
                  "--test-options=--jxml=test-report.xml"
                ];
              } else { });
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

        plainOutputs = pkgs.lib.foldl'
          (acc: ghcVersion:
            pkgs.lib.recursiveUpdate acc
              {
                devShells.${ghcVersion} =
                  devShellWithVersion { ghcVersion = ghcVersion; };
                packages.grisette.${ghcVersion} =
                  (hPkgs { ghcVersion = ghcVersion; ci = false; }).grisette;
                packages.grisette."${ghcVersion}-ci" =
                  (hPkgs { ghcVersion = ghcVersion; ci = true; }).grisette;
              })
          { } [ "8107" "902" "928" "948" "966" "984" "9101" "9121" ];
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

          packages.default =
            (hPkgs { ghcVersion = "9101"; ci = false; }).grisette;
        });
}
