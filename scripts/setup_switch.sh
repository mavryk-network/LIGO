#!/bin/sh
set -e
set -x

printf '' | opam switch create . ocaml-base-compiler.4.14.1 --no-install || true
eval $(opam config env)

# Add opam archive repository for accessing archived package versions
# Use local clone if available (avoids network issues), otherwise use remote URL
if ! opam repository list | grep -q "archive"; then
  echo "Adding archive repository..."
  if [ -d "/tmp/opam-repository-archive" ]; then
    echo "Using local archive repository clone"
    opam repository add archive "file:///tmp/opam-repository-archive"
  else
    echo "Using remote archive repository"
    opam repository add archive https://github.com/ocaml/opam-repository-archive.git
  fi
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
