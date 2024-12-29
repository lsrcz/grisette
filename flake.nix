{
  description = "Symbolic evaluation as a library";
  inputs.nixpkgs.url = "github:NixOS/nixpkgs/nixos-unstable";
  inputs.flake-utils.url = "github:numtide/flake-utils";

  outputs = { self, nixpkgs, flake-utils }:
    flake-utils.lib.eachDefaultSystem (system:
      let
        z3Overlay = (self: super: {
          z3 = super.z3.overrideAttrs (oldAttrs: rec {
            version = "4.13.4";
            src = pkgs.fetchFromGitHub {
              owner = "Z3Prover";
              repo = "z3";
              rev = "z3-4.13.4";
              sha256 = "sha256-8hWXCr6IuNVKkOegEmWooo5jkdmln9nU7wI8T882BSE=";
            };
          });
        });

        hlintSrcOverlay = (self: super: {
          hlintSrc = pkgs.fetchFromGitHub {
            owner = "ndmitchell";
            repo = "hlint";
            rev = "7dfba720eaf6fa9bd0b23ae269334559aa722847";
            sha256 = "sha256-niGBdSrkatr+TZCcLYXo4MDg5FyXTYiKQ5K+ZIWSWBs=";
          };
        });

        pkgs = import nixpkgs {
          inherit system;
          overlays = [ z3Overlay hlintSrcOverlay ];
        };

        stableHPkgs = pkgs.haskell.packages."ghc984";
        hPkgsWithVersion = version:
          (pkgs.haskell.packages."ghc${version}".override {
            all-cabal-hashes = pkgs.fetchFromGitHub {
              owner = "commercialhaskell";
              repo = "all-cabal-hashes";
              rev = "a63037340f628c7e210046265dc23bbcac50c450";
              sha256 = "sha256-KiuSmyUdDkriGxFVElCwFsrIlHsljQTS+EqqG0COy20=";
            };
          }).extend (hself: hsuper: rec {
            ihaskell =
              if version == "9101" then
                pkgs.haskell.lib.dontCheck (
                  hself.callHackage "ihaskell" "0.12.0.0" { }
                )
              else hsuper.ihaskell;
            ghc-syntax-highlighter =
              if version == "9101" then
                hself.callHackage "ghc-syntax-highlighter" "0.0.12.0" { }
              else hsuper.ghc-syntax-highlighter;
            hlint = hself.callCabal2nix "hlint" pkgs.hlintSrc { };
            uuid =
              if version == "9101" then
                hself.callHackage "uuid" "1.3.16" { }
              else hsuper.uuid;
            ghc-parser =
              hself.callHackage "ghc-parser" "0.2.7.0" { };
            ghc-lib-parser =
              if version == "984" then
                hself.callHackage "ghc-lib-parser" "9.8.4.20241130" { }
              else hsuper.ghc-lib-parser;
          });

        basicDevTools = { version }:
          let hPkgs = hPkgsWithVersion version; in [
            hPkgs.ghc # GHC compiler in the desired version (will be available on PATH)
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

        additionalDevTools = version:
          let hPkgs = hPkgsWithVersion version; in [
            # hPkgs.ghcid # Continuous terminal Haskell compile checker
            # hPkgs.ormolu # Haskell formatter
            hPkgs.hlint # Haskell codestyle checker
            hPkgs.haskell-language-server # LSP server for editor
            (pkgs.ihaskell.override {
              ghcWithPackages = hPkgs.ghcWithPackages;
            })
            pkgs.nixpkgs-fmt
            pkgs.boolector
            (pkgs.cvc5.overrideAttrs (oldAttrs: rec {
              cmakeFlags = oldAttrs.cmakeFlags ++ [
                "-DUSE_POLY=ON"
              ];
              buildInputs = oldAttrs.buildInputs ++ [
                pkgs.libpoly
              ];
            }))
          ];

        devTools = { version, cabal, additional, bitwuzla }:
          basicDevTools { inherit version; } ++
          (if cabal
          then cabalDevTools
          else [ ]) ++
          (if additional
          then additionalDevTools version
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
        devShellsWithVersion = { version, cabal, additional, bitwuzla }:
          pkgs.mkShell {
            buildInputs = devTools {
              inherit version cabal additional bitwuzla;
            };

            # Make external Nix c libraries like zlib known to GHC, like
            # pkgs.haskell.lib.buildStackProject does
            # https://github.com/NixOS/nixpkgs/blob/d64780ea0e22b5f61cd6012a456869c702a72f20/pkgs/development/haskell-modules/generic-stack-builder.nix#L38
            LD_LIBRARY_PATH = pkgs.lib.makeLibraryPath (devTools {
              inherit version cabal additional bitwuzla;
            });
          };

      in
      {
        formatter.x86_64-linux = pkgs.nixpkgs-fmt;

        devShells = {
          "8107-ci" = devShellsWithVersion { version = "8107"; cabal = false; additional = false; bitwuzla = false; };
          "902-ci" = devShellsWithVersion { version = "902"; cabal = false; additional = false; bitwuzla = false; };
          "928-ci" = devShellsWithVersion { version = "928"; cabal = false; additional = false; bitwuzla = false; };
          "948-ci" = devShellsWithVersion { version = "948"; cabal = false; additional = false; bitwuzla = false; };
          "966-ci" = devShellsWithVersion { version = "966"; cabal = false; additional = false; bitwuzla = false; };
          "984-ci" = devShellsWithVersion { version = "984"; cabal = false; additional = false; bitwuzla = true; };
          "9101-ci" = devShellsWithVersion { version = "9101"; cabal = false; additional = false; bitwuzla = false; };
          "9101-macOS-ci" = devShellsWithVersion { version = "9101"; cabal = false; additional = false; bitwuzla = false; };

          "8107" = devShellsWithVersion { version = "8107"; cabal = false; additional = false; bitwuzla = false; };
          "902" = devShellsWithVersion { version = "902"; cabal = false; additional = false; bitwuzla = false; };
          "928" = devShellsWithVersion { version = "928"; cabal = false; additional = false; bitwuzla = false; };
          "948" = devShellsWithVersion { version = "948"; cabal = false; additional = false; bitwuzla = false; };
          "966" = devShellsWithVersion { version = "966"; cabal = false; additional = false; bitwuzla = false; };
          "984" = devShellsWithVersion { version = "984"; cabal = false; additional = false; bitwuzla = false; };
          "9101" = devShellsWithVersion { version = "9101"; cabal = true; additional = true; bitwuzla = true; };
          default = devShellsWithVersion { version = "9101"; cabal = true; additional = true; bitwuzla = true; };
        };
      });
}

