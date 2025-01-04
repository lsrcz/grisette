{ pkgs }:
with pkgs.haskell.lib;
(import ./hpkgs-base.nix { inherit pkgs; ghcVersion = "9101"; }).extend (
  hfinal: hprev: {
    sbv = dontCheck (hfinal.callHackage "sbv" "11.0" { });
    ihaskell = dontCheck (hfinal.callHackage "ihaskell" "0.12.0.0" { });
    ghc-parser = hfinal.callHackage "ghc-parser" "0.2.7.0" { };
    ghc-syntax-highlighter =
      hfinal.callHackage "ghc-syntax-highlighter" "0.0.12.0" { };
    hlint = hfinal.callCabal2nix "hlint" pkgs.hlintSrc { };
    uuid = hfinal.callHackage "uuid" "1.3.16" { };
  }
)

