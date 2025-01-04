self: super: {
  z3 = super.z3.overrideAttrs (oldAttrs: rec {
    version = "4.13.4";
    src = super.fetchFromGitHub {
      owner = "Z3Prover";
      repo = "z3";
      rev = "z3-4.13.4";
      sha256 = "sha256-8hWXCr6IuNVKkOegEmWooo5jkdmln9nU7wI8T882BSE=";
    };
  });
}
