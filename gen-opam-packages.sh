#! /bin/sh

PACKAGES=$(cat ./libs.txt)
OPAM_REPO_PATH=/Users/manas/development/callistonianembrace/ligolang/opam-repository
VERSION=18.0-alpha.3
TAG="v$VERSION"

TARBALL_URL="https://gitlab.com/ligolang/tezos-ligo/-/archive/$TAG/tezos-ligo-$TAG.tar.gz"
TARBALL_NAME="tezos-ligo-$TAG.tar.gz"

if [ ! -f ./$TARBALL_NAME ]
then
    curl -O $TARBALL_URL
fi

SHA256=$(sha256sum $TARBALL_NAME | cut -d ' ' -f 1)
SHA512=$(sha512sum $TARBALL_NAME | cut -d ' ' -f 1)

for p in $PACKAGES
do
    PACKAGE_DEST="$OPAM_REPO_PATH/packages/$p/$p.$(echo  $VERSION | sed 's/-alpha.3//')"
    mkdir -p "$PACKAGE_DEST"
    OPAM_MANIFEST_PATH="$PACKAGE_DEST/opam"
    cp "./vendors/tezos-ligo/opam/$p.opam" $OPAM_MANIFEST_PATH
    cat << SRC >> $OPAM_MANIFEST_PATH
url {
  src: "$TARBALL_URL"
  checksum: [
    "sha256=$SHA256"
    "sha512=$SHA512"
  ]
}
SRC
    gsed -i 's/"dune" { >= "3.0" }/"dune" { >= "3.0"}\n  "dune-configurator"/g' "$OPAM_MANIFEST_PATH"
    gsed -i 's/"rm" "-r"/"rm" "-rf"/g'  "$OPAM_MANIFEST_PATH"
done
