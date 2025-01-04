self: super: {
  cvc5 = super.cvc5.overrideAttrs (oldAttrs: rec {
    cmakeFlags = oldAttrs.cmakeFlags ++ [
      "-DUSE_POLY=ON"
    ];
    buildInputs = oldAttrs.buildInputs ++ [
      super.libpoly
    ];
  });
}
