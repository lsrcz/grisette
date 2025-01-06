{
  description = "Symbolic evaluation as a library";
  inputs.nixpkgs.url = "github:NixOS/nixpkgs/nixos-unstable";
  inputs.flake-utils.url = "github:numtide/flake-utils";

  outputs = { self, nixpkgs, flake-utils }:
    flake-utils.lib.eachDefaultSystem (system:
      let
        pkgs = import nixpkgs {
          inherit system;
          overlays = [
            (import ./nix/z3-overlay.nix)
            (import ./nix/hlint-src-overlay.nix)
            (import ./nix/cvc5-overlay.nix)
          ];
        };

        haskell = pkgs.haskell;

        patchedHPkgs = { ghcVersion }: import ./nix/hpkgs.nix {
          inherit pkgs;
          ghcVersion = ghcVersion;
        };

        hPkgs = { ghcVersion, ci }:
          (patchedHPkgs { inherit ghcVersion; }).extend (hfinal: hprev:
            with (import ./nix/hpkgs-extend-helpers.nix {
              inherit pkgs hfinal hprev ghcVersion;
            });
            {
              grisette = (haskell.lib.overrideCabal
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

        devShellsWithVersion = { ghcVersion, cabal, additional, bitwuzla }:
          import ./nix/dev-tools.nix {
            inherit ghcVersion pkgs cabal additional bitwuzla;
            extraAdditionalDevTools = [
              pkgs.boolector
              pkgs.cvc5
            ];
          };

      in
      {
        formatter = pkgs.nixpkgs-fmt;

        devShells = {
          "8107" = devShellsWithVersion { ghcVersion = "8107"; cabal = false; additional = false; bitwuzla = false; };
          "902" = devShellsWithVersion { ghcVersion = "902"; cabal = false; additional = false; bitwuzla = false; };
          "928" = devShellsWithVersion { ghcVersion = "928"; cabal = false; additional = false; bitwuzla = false; };
          "948" = devShellsWithVersion { ghcVersion = "948"; cabal = false; additional = false; bitwuzla = false; };
          "966" = devShellsWithVersion { ghcVersion = "966"; cabal = false; additional = false; bitwuzla = false; };
          "984" = devShellsWithVersion { ghcVersion = "984"; cabal = false; additional = false; bitwuzla = false; };
          "9101" = devShellsWithVersion { ghcVersion = "9101"; cabal = true; additional = true; bitwuzla = true; };
          default = devShellsWithVersion { ghcVersion = "9101"; cabal = true; additional = true; bitwuzla = true; };
          "9121" = devShellsWithVersion { ghcVersion = "9121"; cabal = true; additional = false; bitwuzla = false; };
        };

        packages.grisette."8107-ci" = (hPkgs { ghcVersion = "8107"; ci = true; }).grisette;
        packages.grisette."902-ci" = (hPkgs { ghcVersion = "902"; ci = true; }).grisette;
        packages.grisette."928-ci" = (hPkgs { ghcVersion = "928"; ci = true; }).grisette;
        packages.grisette."948-ci" = (hPkgs { ghcVersion = "948"; ci = true; }).grisette;
        packages.grisette."966-ci" = (hPkgs { ghcVersion = "966"; ci = true; }).grisette;
        packages.grisette."984-ci" = (hPkgs { ghcVersion = "984"; ci = true; }).grisette;
        packages.grisette."9101-ci" = (hPkgs { ghcVersion = "9101"; ci = true; }).grisette;
        packages.default = (hPkgs { ghcVersion = "9101"; ci = false; }).grisette;
      });
}
