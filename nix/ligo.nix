{ lib
, pkgs
, coq
, cacert
, patdiff
}:
let
  filters = (import ./filters.nix) { inherit lib; };
in
{
  ligo = coq.ocamlPackages.buildDunePackage rec {
    pname = "ligo";
    version = "0.36.0-dev";

    src = filters.filterGitSource {
      src = ../.;
      dirs = [ "src" "vendors" "scripts" ];
      files = [ "dune" "dune-project" "ligo.opam" ];
    };

    # The build picks this up for ligo --version
    LIGO_VERSION = version;

    useDune2 = true;

    buildInputs = with coq.ocamlPackages; [
      bisect_ppx
      cmdliner
      coq
      core
      data-encoding
      getopt
      linenoise
      menhir
      menhirLib
      ocaml-recovery-parser
      ocamlgraph
      pprint
      ppx_deriving
      ppx_deriving_yojson
      ppx_expect
      ppx_import
      qcheck
      terminal_size
      tezos-011-PtHangz2-test-helpers
      yojson
    ];

    checkInputs = [
      cacert
      coq.ocamlPackages.ca-certs
      patdiff
    ];

    doCheck = true;

    meta = with lib; {
      homepage = "https://ligolang.org/";
      downloadPage = "https://ligolang.org/docs/intro/installation";
      description = "A friendly Smart Contract Language for Tezos";
      license = licenses.mit;
      platforms = [ "x86_64-linux" ];
    };
  };
}
