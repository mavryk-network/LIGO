{
  description = "A friendly Smart Contract Language for Tezos";

  inputs = {
    nixpkgs.url = "github:nixos/nixpkgs/nixos-unstable";
  
    flake-utils.url = "github:numtide/flake-utils";

    ocaml-overlay.url = "github:anmonteiro/nix-overlays/ulrikstrid/ocaml-recovery-parser";
    ocaml-overlay.inputs.nixpkgs.follows = "nixpkgs";
    ocaml-overlay.inputs.flake-utils.follows = "flake-utils";
  };

  outputs = { self, nixpkgs, flake-utils, ocaml-overlay }:
  flake-utils.lib.eachDefaultSystem (system:
    let
      pkgs = import nixpkgs { inherit system; overlays = [ ocaml-overlay.overlay ]; };
      pkgs' = pkgs.pkgsCross.musl64;

      coq-compiler = (pkgs'.coq_8_14.override { buildIde = false; csdp = null; })
        .overrideAttrs (o: {
          buildFlags = [ "revision" "coq" ];
          configurePlatforms = [];
          nativeBuildInputs = o.nativeBuildInputs ++ (with pkgs'.ocaml-ng.ocamlPackages_4_12; [ ocaml dune findlib menhir ]);
        });

      ligo-static = pkgs'.callPackage ./nix/ligo.nix {
        coq = coq-compiler;
        doCheck = false;
        static = true;
      };
      
      ligo = pkgs.callPackage ./nix/ligo.nix { coq = pkgs.coq_8_14; doCheck = false; };

      devShell = (pkgs.mkShell {
        inputsFrom = [ ligo ];
        buildInputs = with pkgs; with coq_8_14.ocamlPackages; [
            ocaml-lsp
            ocamlformat_0_20_1
            odoc
        ];
      });
    in
    {
      packages = {
        inherit ligo;
        inherit ligo-static;
        docker = pkgs.callPackage ./nix/docker.nix { ligo = ligo-static; };
        deb = pkgs.callPackage ./nix/packageDeb.nix { ligo = ligo-static; };
        changelog = pkgs.callPackage ./nix/changelog.nix { };
      };
      inherit devShell;
    }
  );
}
