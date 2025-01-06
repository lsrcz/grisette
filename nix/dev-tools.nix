{ ghcVersion
, pkgs
, cabal
, bitwuzla
, additional
, extraBasicDevTools ? [ ]
, extraAdditionalDevTools ? [ ]
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
  patchedHPkgs = { ghcVersion }: import ./hpkgs.nix {
    inherit pkgs;
    ghcVersion = ghcVersion;
  };
  basicDevTools =
    let hPkgs0 = patchedHPkgs { inherit ghcVersion; }; in [
      hPkgs0.ghc # GHC compiler in the desired version (will be available on PATH)
      stack-wrapped
      pkgs.zlib # External C library needed by some Haskell packages
      pkgs.z3
    ] ++ extraBasicDevTools;
  bitwuzlaDevTools = [
    pkgs.bitwuzla
  ];
  cabalDevTools = [
    stableHPkgs.cabal-install
  ];
  additionalDevTools =
    let hPkgs0 = patchedHPkgs { inherit ghcVersion; }; in [
      hPkgs0.hlint # Haskell codestyle checker
      hPkgs0.haskell-language-server # LSP server for editor
    ] ++ extraAdditionalDevTools;
  devTools = basicDevTools ++
    (if cabal then cabalDevTools else [ ]) ++
    (if additional then additionalDevTools else [ ]) ++
    (if bitwuzla then bitwuzlaDevTools else [ ]);
in
pkgs.mkShell {
  buildInputs = devTools;
  # Make external Nix c libraries like zlib known to GHC, like
  # pkgs.haskell.lib.buildStackProject does
  # https://github.com/NixOS/nixpkgs/blob/d64780ea0e22b5f61cd6012a456869c702a72f20/pkgs/development/haskell-modules/generic-stack-builder.nix#L38
  LD_LIBRARY_PATH = pkgs.lib.makeLibraryPath devTools;
}

