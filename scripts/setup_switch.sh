#!/bin/sh
set -e
set -x

script_dir="$(cd "$(dirname "$0")" && pwd -P)"
src_dir="$(dirname "$script_dir")"

# ligo.opam.locked pins tezos-rust-libs.1.6, but that package has since been pruned
# from ocaml/opam-repository, which now carries only 1.0 and 1.1. Solving against the
# upstream repository therefore fails with:
#
#   [ERROR] Package conflict!
#     * Missing dependency:
#       - tezos-rust-libs >= 1.6
#       no matching version
#
# It is the *only* pin in the lockfile upstream can no longer satisfy -- the other 264
# all still resolve -- so opam/overlay supplies just that one package. Note we do not
# freeze the whole repository to an old commit the way mavryk-protocol does via
# `full_opam_repository_tag`: this lockfile is much newer than that pin, and rolling
# the repository back would break ~69 packages (dune 3.20.2, ppxlib 0.35.0, ...).
opam repository add ligo-overlay --dont-select "$src_dir/opam/overlay" ||
  opam repository set-url ligo-overlay --dont-select "$src_dir/opam/overlay"

# Local repositories are only indexed after an explicit update, even though the
# content is already on disk.
opam update ligo-overlay

# Order sets priority: upstream wins, the overlay only fills what upstream lacks.
printf '' | opam switch create . --repositories=default,ligo-overlay \
  ocaml-base-compiler.4.14.1 --no-install || true
eval $(opam config env)
