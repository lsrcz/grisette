{ pkgs, hfinal, hprev, ghcVersion }: rec {
  overwrittenHPkg = pkgName: attrs:
    if builtins.hasAttr ghcVersion attrs
    then attrs.${ghcVersion}
    else if builtins.hasAttr "default" attrs
    then attrs.default
    else hprev.${pkgName};
  noCheckOverwrittenHPkg = pkgName: attrs:
    overwrittenHPkg pkgName
      (builtins.mapAttrs (_: pkg: pkgs.haskell.lib.dontCheck pkg) attrs);
  simpleOverwrittenHPkg = pkgName: attrs:
    overwrittenHPkg pkgName (builtins.mapAttrs
      (_: pkgVersion:
        hfinal.callHackage pkgName pkgVersion { }
      )
      attrs);
  noCheckSimpleOverwrittenHPkg = pkgName: attrs:
    overwrittenHPkg pkgName (builtins.mapAttrs
      (_: pkgVersion:
        pkgs.haskell.lib.dontCheck (hfinal.callHackage pkgName pkgVersion { })
      )
      attrs);
  byGhcVersion = attrs:
    if builtins.hasAttr ghcVersion attrs
    then attrs.${ghcVersion}
    else attrs.default;
}
