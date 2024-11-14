FROM nixos/nix:2.25.1-amd64
RUN nix-env -iA \
      nixpkgs.docker \
      nixpkgs.git \
      nixpkgs.curl \
      nixpkgs.wget
