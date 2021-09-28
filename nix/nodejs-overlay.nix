self: super: {
  # Note: this overlay doesn't apply to nix-npm-buildpackage
  nodejs = nodejs-16_x;
  nodePackages = nodePackages_16_x;
  nodejs-slim = nodejs-slim-16_x;
}
