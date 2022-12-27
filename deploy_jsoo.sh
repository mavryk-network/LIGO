#! /bin/sh

mkdir $TMPDIR/ligo-ide 
cp -R  _build $TMPDIR/ligo-ide/_build
cp index.html $TMPDIR/ligo-ide/
cp editor.bundle.js $TMPDIR/ligo-ide
cp *.wasm $TMPDIR/ligo-ide/
cp api.json layouts.json $TMPDIR/ligo-ide/
