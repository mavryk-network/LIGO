#!/bin/sh
set -e
set -x

# Add opam archive repository for accessing archived package versions
opam repository add opam-repository-archive https://github.com/ocaml/opam-repository-archive.git --all --set-default || true
opam repository set-url default https://opam.ocaml.org --all || true
opam repository priority default 1 || true
opam repository priority opam-repository-archive 2 || true

printf '' | opam switch create . ocaml-base-compiler.4.14.1 --no-install || true
eval $(opam config env)
