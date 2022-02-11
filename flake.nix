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
        
        packages = pkgs.callPackage ./nix/ligo.nix { coq = pkgs.coq_8_14; }; 

        devShell = (pkgs.mkShell {
          inputsFrom = [ packages.ligo ];
          buildInputs = with pkgs; with coq_8_14.ocamlPackages; [
              ocaml-lsp
              ocamlformat_0_20_1
              odoc
          ];
        });
      in
      {
        inherit packages;
        inherit devShell;
      }
    );
}
