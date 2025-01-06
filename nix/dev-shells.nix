{ pkgs
, haskellPackages
, isDevelopmentEnvironment ? false
, extraBuildInputs ? [ ]
}:
let
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
  stableHPkgs = pkgs.haskell.packages."ghc9101";
  basicBuildInputs =
    with haskellPackages; [
      ghc # GHC compiler in the desired version (will be available on PATH)
      stack-wrapped
      pkgs.zlib # External C library needed by some Haskell packages
      pkgs.z3
      stableHPkgs.cabal-install
    ];
  hlsBuildInputs =
    with haskellPackages; [
      hlint # Haskell codestyle checker
      haskell-language-server # LSP server for editor
    ];
  devTools = basicBuildInputs ++
    (if isDevelopmentEnvironment then hlsBuildInputs else [ ]) ++
    extraBuildInputs;
in
pkgs.mkShell {
  buildInputs = devTools;
  # Make external Nix c libraries like zlib known to GHC, like
  # pkgs.haskell.lib.buildStackProject does
  # https://github.com/NixOS/nixpkgs/blob/d64780ea0e22b5f61cd6012a456869c702a72f20/pkgs/development/haskell-modules/generic-stack-builder.nix#L38
  LD_LIBRARY_PATH = pkgs.lib.makeLibraryPath devTools;
}

