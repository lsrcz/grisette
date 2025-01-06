{
  overlays = {
    z3 = import ./z3-overlay.nix;
    hlintSrc = import ./hlint-src-overlay.nix;
    cvc5 = import ./cvc5-overlay.nix;
  };
  patchedHaskellPackages = import ./hpkgs.nix;
  haskellPackagesExtendHelpers = import ./hpkgs-extend-helpers.nix;

  devShell = import ./dev-shells.nix;
}
