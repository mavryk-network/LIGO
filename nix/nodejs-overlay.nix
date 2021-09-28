self: super: {
  # Note: this overlay doesn't apply to nix-npm-buildpackage
  nodejs = super.nodejs-16_x;
  nodePackages = super.nodePackages_16_x;
  nodejs-slim = super.nodejs-slim-16_x;
}
