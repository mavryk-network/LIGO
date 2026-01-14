#!/bin/sh

# TODO this is exactly like install_vendors_deps.sh but doesn't
# export cargo bins path

set -e
set -x

# Install local dependencies
export PATH=~/.cargo/bin:$PATH

# Pin git dependencies before locked install (required for --locked to work with pin-depends)
opam pin add -yn ocaml-recovery-parser.0.2.4 git+https://github.com/serokell/ocaml-recovery-parser.git#e05c872d1a0e8074940d995b57556121eddbf0f2
opam pin add -yn linol.0.5 git+https://github.com/c-cube/linol.git#7730eabf98f657059920369b41d43e657a231ed5
opam pin add -yn linol-lwt.0.5 git+https://github.com/c-cube/linol.git#7730eabf98f657059920369b41d43e657a231ed5
opam pin add -yn landmarks.1.4 git+https://github.com/LexiFi/landmarks.git#b0c753cd2a4c4aa00dffdd3be187d8ed592fabf7
opam pin add -yn landmarks-ppx.1.4 git+https://github.com/LexiFi/landmarks.git#b0c753cd2a4c4aa00dffdd3be187d8ed592fabf7

BLST_PORTABLE=y opam install -y --deps-only --with-test --with-doc --locked .
opam install -y vendors/mavryk-ligo/data-encoding