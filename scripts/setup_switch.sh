#!/bin/sh
set -e
set -x

printf '' | opam switch create . ocaml-base-compiler.4.14.1 --no-install || true
eval $(opam config env)

# Add opam archive repository for accessing archived package versions
# Check if archive repo already exists before adding
if ! opam repository list | grep -q "archive"; then
  echo "Adding archive repository..."
  opam repository add archive https://github.com/ocaml/opam-repository-archive.git
fi

# Ensure default repository URL is correct
opam repository set-url default https://opam.ocaml.org || true

# Set priorities: higher number = higher priority (checked first)
echo "Setting repository priorities (default=10, archive=5)..."
opam repository priority default 10
opam repository priority archive 5

# Verify the configuration
echo "Current repository configuration:"
opam repository list
