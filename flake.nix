{
  description = "A friendly Smart Contract Language for Tezos";

  inputs = {
    nixpkgs.url = "github:nixos/nixpkgs/nixos-unstable";
  
    flake-utils.url = "github:numtide/flake-utils";

    #ocaml-overlay.url = "github:anmonteiro/nix-overlays";
    #ocaml-overlay.inputs.nixpkgs.follows = "nixpkgs";
  };


  outputs = { self, nixpkgs, flake-utils }:
  flake-utils.lib.eachDefaultSystem (system:
    let
      pkgs = import nixpkgs { inherit system; };
      
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
        docker = pkgs.callPackage ./nix/docker.nix { inherit ligo; };
        deb = pkgs.callPackage ./nix/packageDeb.nix { inherit ligo; };
        changelog = pkgs.callPackage ./nix/changelog.nix { };
      };
      inherit devShell;
    }
  );
}
