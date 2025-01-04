{ pkgs }:
with pkgs.haskell.lib;
(import ./hpkgs-base.nix { inherit pkgs; ghcVersion = "966"; }).extend (
  hfinal: hprev: {
    sbv = dontCheck (hfinal.callHackage "sbv" "10.12" { });
  }
)
