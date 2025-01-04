{ pkgs, ghcVersion }: pkgs.haskell.packages."ghc${ghcVersion}".override (prev: {
  all-cabal-hashes = pkgs.fetchFromGitHub {
    owner = "commercialhaskell";
    repo = "all-cabal-hashes";
    rev = "eb6641d2dfddf269c90c2cfdec94b3f1145418f3";
    sha256 = "sha256-hiL/IIoCISoPXq9OviWR7UMQF7RK4WukGRNP+ht4gpw=";
  };
})
