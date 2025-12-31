#!/bin/sh
set -e
set -x

printf '' | opam switch create . ocaml-base-compiler.4.14.1 --no-install || true
eval $(opam config env)

# Add opam archive repository for accessing archived package versions
# Note: The repository name will be 'archive' not 'opam-repository-archive'
opam repository add archive https://github.com/ocaml/opam-repository-archive.git || true
opam repository set-url default https://opam.ocaml.org || true
# Set priorities: higher number = higher priority (checked first)
opam repository priority default 10 || true
opam repository priority archive 5 || true
