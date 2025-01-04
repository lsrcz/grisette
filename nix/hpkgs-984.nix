{ pkgs }:
with pkgs.haskell.lib;
(import ./hpkgs-base.nix { inherit pkgs; ghcVersion = "984"; }).extend (
  hfinal: hprev: {
    sbv = dontCheck (hfinal.callHackage "sbv" "11.0" { });
    ghc-parser = hfinal.callHackage "ghc-parser" "0.2.7.0" { };
    ghc-lib-parser = hfinal.callHackage "ghc-lib-parser" "9.8.4.20241130" { };
  }
)
