#!/bin/sh
set -e
set -x

printf '' | opam switch create . --locked --no-install
eval $(opam config env)
