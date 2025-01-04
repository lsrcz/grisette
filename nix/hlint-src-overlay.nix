self: super: {
  hlintSrc = super.fetchFromGitHub {
    owner = "ndmitchell";
    repo = "hlint";
    rev = "7dfba720eaf6fa9bd0b23ae269334559aa722847";
    sha256 = "sha256-niGBdSrkatr+TZCcLYXo4MDg5FyXTYiKQ5K+ZIWSWBs=";
  };
}
