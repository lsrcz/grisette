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

        developmentVersions = [ "983" ];

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

        additionalDevTools = version:
          let hPkgs = hPkgsWithVersion version; in [
            # hPkgs.ghcid # Continuous terminal Haskell compile checker
            # hPkgs.ormolu # Haskell formatter
            hPkgs.hlint # Haskell codestyle checker
            hPkgs.haskell-language-server # LSP server for editor
            stableHPkgs.cabal-install
            (pkgs.ihaskell.override {
              ghcWithPackages = hPkgs.ghcWithPackages;
            })
          ];

        devTools = version:
          if builtins.elem version developmentVersions
          then basicDevTools version ++ additionalDevTools version
          else basicDevTools version;

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
      devShellsWithVersion = version: pkgs.mkShell {
          buildInputs = devTools version;

          # Make external Nix c libraries like zlib known to GHC, like
          # pkgs.haskell.lib.buildStackProject does
          # https://github.com/NixOS/nixpkgs/blob/d64780ea0e22b5f61cd6012a456869c702a72f20/pkgs/development/haskell-modules/generic-stack-builder.nix#L38
          LD_LIBRARY_PATH = pkgs.lib.makeLibraryPath (devTools version);
        };

      in
      {
        formatter.x86_64-linux = pkgs.nixpkgs-fmt;

        devShells = {
          "8107" = devShellsWithVersion "8107";
          "902" = devShellsWithVersion "902";
          "928" = devShellsWithVersion "928";
          "948" = devShellsWithVersion "948";
          "966" = devShellsWithVersion "966";
          "983" = devShellsWithVersion "983";
          "9101" = devShellsWithVersion "9101";
          default = devShellsWithVersion "983";
        };
      });
}
