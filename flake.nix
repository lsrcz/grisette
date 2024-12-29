{
  description = "Symbolic evaluation as a library";
  inputs.nixpkgs.url = "github:NixOS/nixpkgs/nixos-unstable";
  inputs.flake-utils.url = "github:numtide/flake-utils";

  outputs = { self, nixpkgs, flake-utils }:
    flake-utils.lib.eachDefaultSystem (system:
      let
        pkgs = import nixpkgs {
          inherit system;
        };

        stableHPkgs = pkgs.haskell.packages."ghc983";
        hPkgsWithVersion = version: pkgs.haskell.packages."ghc${version}".extend (hself: hsuper: rec {
          ihaskell = pkgs.haskell.lib.dontCheck (hself.callHackage "ihaskell" "0.11.0.0" { });
          ghc-syntax-highlighter =
            hself.callHackage "ghc-syntax-highlighter" "0.0.11.0" { };
        });

        basicDevTools = version:
          let hPkgs = hPkgsWithVersion version; in [
            hPkgs.ghc # GHC compiler in the desired version (will be available on PATH)
            stack-wrapped
            pkgs.zlib # External C library needed by some Haskell packages
            pkgs.z3_4_12
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

        devTools = { version, cabal, additional }:
          basicDevTools version ++
          (if cabal
          then cabalDevTools
          else [ ]) ++
          (if additional
          then additionalDevTools version
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
        devShellsWithVersion = { version, cabal, additional }: pkgs.mkShell {
          buildInputs = devTools { inherit version cabal additional; };

          # Make external Nix c libraries like zlib known to GHC, like
          # pkgs.haskell.lib.buildStackProject does
          # https://github.com/NixOS/nixpkgs/blob/d64780ea0e22b5f61cd6012a456869c702a72f20/pkgs/development/haskell-modules/generic-stack-builder.nix#L38
          LD_LIBRARY_PATH = pkgs.lib.makeLibraryPath (devTools { inherit version cabal additional; });
        };

      in
      {
        formatter.x86_64-linux = pkgs.nixpkgs-fmt;

        devShells = {
          "8107-ci" = devShellsWithVersion { version = "8107"; cabal = true; additional = false; };
          "902-ci" = devShellsWithVersion { version = "902"; cabal = true; additional = false; };
          "928-ci" = devShellsWithVersion { version = "928"; cabal = true; additional = false; };
          "948-ci" = devShellsWithVersion { version = "948"; cabal = true; additional = false; };
          "966-ci" = devShellsWithVersion { version = "966"; cabal = true; additional = false; };
          "983-ci" = devShellsWithVersion { version = "983"; cabal = true; additional = false; };
          "9101-ci" = devShellsWithVersion { version = "9101"; cabal = true; additional = false; };

          "8107" = devShellsWithVersion { version = "8107"; cabal = false; additional = false; };
          "902" = devShellsWithVersion { version = "902"; cabal = false; additional = false; };
          "928" = devShellsWithVersion { version = "928"; cabal = false; additional = false; };
          "948" = devShellsWithVersion { version = "948"; cabal = false; additional = false; };
          "966" = devShellsWithVersion { version = "966"; cabal = false; additional = false; };
          "983" = devShellsWithVersion { version = "983"; cabal = true; additional = true; };
          "9101" = devShellsWithVersion { version = "9101"; cabal = true; additional = false; };
          default = devShellsWithVersion { version = "983"; cabal = true; additional = true; };
        };
      });
}
