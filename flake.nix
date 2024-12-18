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
        hPkgs = pkgs.haskell.packages."ghc983".extend (hself: hsuper: rec {
          ihaskell = pkgs.haskell.lib.dontCheck (hself.callHackage "ihaskell" "0.11.0.0" { });
          ghc-syntax-highlighter =
            hself.callHackage "ghc-syntax-highlighter" "0.0.11.0" { };
        });

        basicDevTools = [
          hPkgs.ghc # GHC compiler in the desired version (will be available on PATH)
          stack-wrapped
          pkgs.zlib # External C library needed by some Haskell packages
          pkgs.boolector
          pkgs.z3_4_12
          pkgs.nixpkgs-fmt
          pkgs.bitwuzla
          (pkgs.cvc5.overrideAttrs (oldAttrs: rec {
            cmakeFlags = oldAttrs.cmakeFlags ++ [
              "-DUSE_POLY=ON"
            ];
            buildInputs = oldAttrs.buildInputs ++ [
              pkgs.libpoly
            ];
          }))
        ];

        additionalDevTools = [
          # hPkgs.ghcid # Continuous terminal Haskell compile checker
          # hPkgs.ormolu # Haskell formatter
          hPkgs.hlint # Haskell codestyle checker
          hPkgs.haskell-language-server # LSP server for editor
          stableHPkgs.cabal-install
          (pkgs.ihaskell.override {
            ghcWithPackages = hPkgs.ghcWithPackages;
          })
        ];

        devTools = basicDevTools ++ additionalDevTools;
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
      in
      {
        formatter.x86_64-linux = pkgs.nixpkgs-fmt;

        devShells.default = pkgs.mkShell {
          buildInputs = devTools;

          # Make external Nix c libraries like zlib known to GHC, like
          # pkgs.haskell.lib.buildStackProject does
          # https://github.com/NixOS/nixpkgs/blob/d64780ea0e22b5f61cd6012a456869c702a72f20/pkgs/development/haskell-modules/generic-stack-builder.nix#L38
          LD_LIBRARY_PATH = pkgs.lib.makeLibraryPath devTools;
        };
      });
}
