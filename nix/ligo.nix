{ lib
, coq
, cacert
, patdiff
, doCheck ? true
, static ? false
}:
let
  filters = (import ./filters.nix) { inherit lib; };
in
coq.ocamlPackages.buildDunePackage rec {
  pname = "ligo";
  version = "0.36.0-dev";

  src = filters.filterGitSource {
    src = ../.;
    dirs = [ "src" "vendors" "scripts" ];
    files = [ "dune" "dune-project" "ligo.opam" ];
  };

  buildPhase = ''
    echo "running ${if static then "static" else "release"} build"
    dune build -p ligo --display=short --profile=${if static then "static" else "release"}
  '';
  installPhase = ''
    mkdir -p $out/bin
    mv _build/default/src/bin/runligo.exe $out/bin/ligo
  '';

  # The build picks this up for ligo --version
  LIGO_VERSION = version;

  useDune2 = true;

  nativeBuildInputs = with coq.ocamlPackages; [
    coq
    findlib
    menhir
  ];


  buildInputs = with coq.ocamlPackages; [
    bisect_ppx
    cmdliner
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

  nativeCheckInputs = [
    cacert
    patdiff
  ];

  checkInputs = [
    coq.ocamlPackages.ca-certs
  ];

  inherit doCheck;

  meta = with lib; {
    homepage = "https://ligolang.org/";
    downloadPage = "https://ligolang.org/docs/intro/installation";
    description = "A friendly Smart Contract Language for Tezos";
    license = licenses.mit;
    platforms = [ "x86_64-linux" ];
  };
}
