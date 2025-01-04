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
        stableHPkgs = haskell.packages."ghc9101";

        hPkgs = { ghcVersion, ci }:
          (haskell.packages."ghc${ghcVersion}".override (prev: {
            all-cabal-hashes = pkgs.fetchFromGitHub {
              owner = "commercialhaskell";
              repo = "all-cabal-hashes";
              rev = "eb6641d2dfddf269c90c2cfdec94b3f1145418f3";
              sha256 = "sha256-hiL/IIoCISoPXq9OviWR7UMQF7RK4WukGRNP+ht4gpw=";
            };
          })).extend (hfinal: hprev:
            let
              h = import ./nix/hpkgs-extend-helpers.nix {
                inherit pkgs hfinal hprev ghcVersion;
              };
            in
            rec {
              ihaskell = h.noCheckSimpleOverwrittenHPkg "ihaskell" {
                "9101" = "0.12.0.0";
              };
              ghc-syntax-highlighter =
                h.simpleOverwrittenHPkg "ghc-syntax-highlighter" {
                  "9101" = "0.0.12.0";
                };
              hlint = hfinal.callCabal2nix "hlint" pkgs.hlintSrc { };
              uuid = h.simpleOverwrittenHPkg "uuid" {
                "9101" = "1.3.16";
              };
              ghc-parser =
                hfinal.callHackage "ghc-parser" "0.2.7.0" { };
              ghc-lib-parser = h.simpleOverwrittenHPkg "ghc-lib-parser" {
                "984" = "9.8.4.20241130";
              };
              grisette = (haskell.lib.overrideCabal
                (hfinal.callCabal2nix "grisette" ./. { })
                (drv: {
                  testToolDepends = h.byGhcVersion {
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
                    cp dist/hpc $out/hpc -r
                  '';
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
              sbv = h.noCheckSimpleOverwrittenHPkg "sbv" {
                "8107" = "9.2";
                "902" = "9.2";
                "928" = "10.10";
                "948" = "10.10";
                "966" = "10.12";
                default = "11.0";
              };
            });

        basicDevTools = { ghcVersion, ci }:
          let hPkgs0 = hPkgs { inherit ghcVersion ci; }; in [
            hPkgs0.ghc # GHC compiler in the desired version (will be available on PATH)
            stack-wrapped
            pkgs.zlib # External C library needed by some Haskell packages
            pkgs.z3
          ];

        bitwuzlaDevTools = [
          pkgs.bitwuzla
        ];

        cabalDevTools = [
          stableHPkgs.cabal-install
        ];

        additionalDevTools = { ghcVersion, ci }:
          let hPkgs0 = hPkgs { inherit ghcVersion ci; }; in [
            # hPkgs.ghcid # Continuous terminal Haskell compile checker
            # hPkgs.ormolu # Haskell formatter
            hPkgs0.hlint # Haskell codestyle checker
            hPkgs0.haskell-language-server # LSP server for editor
            (pkgs.ihaskell.override {
              ghcWithPackages = hPkgs0.ghcWithPackages;
            })
            pkgs.boolector
            pkgs.cvc5
          ];

        devTools = { ghcVersion, cabal, additional, bitwuzla, ci }:
          basicDevTools { inherit ghcVersion ci; } ++
          (if cabal
          then cabalDevTools
          else [ ]) ++
          (if additional
          then additionalDevTools { inherit ghcVersion ci; }
          else [ ]) ++
          (if bitwuzla
          then bitwuzlaDevTools
          else [ ]);

        # Wrap Stack to work with our Nix integration. We don't want to modify
        # stack.yaml so non-Nix users don't notice anything.
        # - no-nix: We don't want Stack's way of integrating Nix.
        # --system-ghc    # Use the existing GHC on PATH (will come from this Nix file)
        # --no-install-ghc  # Don't try to install GHC if no matching GHC found on PATH
        stack-wrapped = pkgs.symlinkJoin {
          name = "stack"; # will be available as the usual `stack` in terminal
          paths = [ pkgs.stack ];
          buildInputs = [ pkgs.makeWrapper ];
          postBuild = ''
            wrapProgram $out/bin/stack \
              --add-flags "\
                --no-nix \
                --system-ghc \
                --no-install-ghc \
              "
          '';
        };
        devShellsWithVersion = { ghcVersion, cabal, additional, bitwuzla, ci }:
          pkgs.mkShell {
            buildInputs = devTools {
              inherit ghcVersion cabal additional bitwuzla ci;
            };

            # Make external Nix c libraries like zlib known to GHC, like
            # pkgs.haskell.lib.buildStackProject does
            # https://github.com/NixOS/nixpkgs/blob/d64780ea0e22b5f61cd6012a456869c702a72f20/pkgs/development/haskell-modules/generic-stack-builder.nix#L38
            LD_LIBRARY_PATH = pkgs.lib.makeLibraryPath (devTools {
              inherit ghcVersion cabal additional bitwuzla ci;
            });
          };

      in
      {
        formatter = pkgs.nixpkgs-fmt;

        devShells = {
          "8107-ci" = devShellsWithVersion { ghcVersion = "8107"; cabal = false; additional = false; bitwuzla = false; ci = true; };
          "902-ci" = devShellsWithVersion { ghcVersion = "902"; cabal = false; additional = false; bitwuzla = false; ci = true; };
          "928-ci" = devShellsWithVersion { ghcVersion = "928"; cabal = false; additional = false; bitwuzla = false; ci = true; };
          "948-ci" = devShellsWithVersion { ghcVersion = "948"; cabal = false; additional = false; bitwuzla = false; ci = true; };
          "966-ci" = devShellsWithVersion { ghcVersion = "966"; cabal = false; additional = false; bitwuzla = false; ci = true; };
          "984-ci" = devShellsWithVersion { ghcVersion = "984"; cabal = false; additional = false; bitwuzla = false; ci = true; };
          "9101-ci" = devShellsWithVersion { ghcVersion = "9101"; cabal = false; additional = false; bitwuzla = true; ci = true; };
          "9101-macOS-ci" = devShellsWithVersion { ghcVersion = "9101"; cabal = false; additional = false; bitwuzla = true; ci = true; };
          "9121-ci" = devShellsWithVersion { ghcVersion = "9121"; cabal = true; additional = false; bitwuzla = true; ci = true; };

          "8107" = devShellsWithVersion { ghcVersion = "8107"; cabal = false; additional = false; bitwuzla = false; ci = false; };
          "902" = devShellsWithVersion { ghcVersion = "902"; cabal = false; additional = false; bitwuzla = false; ci = false; };
          "928" = devShellsWithVersion { ghcVersion = "928"; cabal = false; additional = false; bitwuzla = false; ci = false; };
          "948" = devShellsWithVersion { ghcVersion = "948"; cabal = false; additional = false; bitwuzla = false; ci = false; };
          "966" = devShellsWithVersion { ghcVersion = "966"; cabal = false; additional = false; bitwuzla = false; ci = false; };
          "984" = devShellsWithVersion { ghcVersion = "984"; cabal = false; additional = false; bitwuzla = false; ci = false; };
          "9101" = devShellsWithVersion { ghcVersion = "9101"; cabal = true; additional = true; bitwuzla = true; ci = false; };
          default = devShellsWithVersion { ghcVersion = "9101"; cabal = true; additional = true; bitwuzla = true; ci = false; };
          "9121" = devShellsWithVersion { ghcVersion = "9121"; cabal = true; additional = false; bitwuzla = false; ci = true; };
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

