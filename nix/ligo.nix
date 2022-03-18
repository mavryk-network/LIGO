{ lib
, pkgs
, removeReferencesTo
, coq
, cacert
, patdiff
, doCheck ? true
, static ? false
}:
let
  filters = (import ./filters.nix) { inherit lib; };
  ocamlPackages = pkgs.ocaml-ng.ocamlPackages_4_12;
in
ocamlPackages.buildDunePackage rec {
  pname = "ligo";
  version = "0.37.0-dev";

  src = filters.filterGitSource {
    src = ../.;
    dirs = [ "src" "vendors" "scripts" ];
    files = [ "dune" "dune-project" "ligo.opam" ];
  };

  buildPhase = ''
    runHook preBuild
    echo "running ${if static then "static" else "release"} build"
    dune build -p ${pname} --profile=${if static then "static" else "release"}
    runHook postBuild
  '';

  postFixup = ''
    # Remove every directory which could have links to other store paths.
    rm -rf $out/lib $out/nix-support $out/share/doc
    remove-references-to \
      -t ${ocamlPackages.ocaml} \
      $out/bin/ligo
  '' + (if static then ''
    # If we're building statically linked binaries everything should be possible to remove
    remove-references-to \
      -t ${pkgs.libffi} \
      $out/bin/ligo
    remove-references-to \
      -t ${pkgs.gmp} \
      $out/bin/ligo
    '' else "");
  isLibrary = false;

  # The build picks this up for ligo --version
  LIGO_VERSION = version;

  useDune2 = true;

  strictDeps = false;

  nativeBuildInputs = [
    removeReferencesTo
    coq
  ] ++ (with ocamlPackages; [
    menhir
    ocaml-recovery-parser
  ]);

  buildInputs = with ocamlPackages; [
    cmdliner
    core
    fpath
    data-encoding
    getopt
    linenoise
    menhirLib
    ocamlgraph
    pprint
    ppx_deriving
    ppx_deriving_yojson
    ppx_expect
    ppx_import
    terminal_size
    ocaml-recovery-parser
    yojson
    
    # vendored tezos' deps
    ctypes
    hacl-star
    hacl-star-raw
    lwt-canceler
    ipaddr
    bls12-381-unix
    bls12-381-legacy
    ptime
    mtime
    lwt_log
    ringo
    ringo-lwt
    secp256k1-internal
    resto
    resto-directory
    resto-cohttp-self-serving-client
    irmin-pack

    # Test helpers deps
    qcheck
    qcheck-alcotest
    alcotest-lwt
    bisect_ppx

    # ???
    num
  ];

  checkInputs = with ocamlPackages; [
    cacert
    ca-certs
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
