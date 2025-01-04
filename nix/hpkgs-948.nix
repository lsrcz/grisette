{ pkgs }:
with pkgs.haskell.lib;
(import ./hpkgs-base.nix { inherit pkgs; ghcVersion = "948"; }).extend (
  hfinal: hprev: {
    sbv = dontCheck (hfinal.callHackage "sbv" "10.10" { });
  }
)
