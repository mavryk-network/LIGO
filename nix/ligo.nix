{ lib
, coq
, cacert
, patdiff
}:

{
  ligo = coq.ocamlPackages.buildDunePackage rec {
    pname = "ligo";
    version = "0.36.0-dev";
    src = ./..;

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
      tezos-base
      tezos-protocol-011-PtHangz2
      tezos-protocol-011-PtHangz2-parameters
      tezos-protocol-environment
      tezos-shell-services
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
